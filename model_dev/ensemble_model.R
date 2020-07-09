### Configuration
######################################################################################################
# Load Configured File Paths, Settings, Src Modules
source('configuration/configuration.R')
set.seed(config_random_seed)


### Read Training and Test Sets
######################################################################################################
train_dt = fread(paste0(config_fp_raw_data_dir, config_fp_raw_train_file))
test_dt = fread(paste0(config_fp_raw_data_dir, config_fp_raw_test_file))


### Initialize H2O Session
######################################################################################################
h2o.init()


### Define Columns, Create Dummy Earned Exposure & Pure Premium Fields
######################################################################################################
# Column Definitions
id_vars = 'id'
categ_vars = colnames(train_dt)[colnames(train_dt) %like% 'cat']
cont_vars = colnames(train_dt)[colnames(train_dt) %like% 'cont']

# Dummy Earned Exposures & Pure Premium
train_dt[, earned_exposure := 1]
test_dt[, earned_exposure := 1]
train_dt[, pure_premium := loss / earned_exposure]


### Process Data, Creating 10 Folds within Train
######################################################################################################
# Run K-fold Pipeline Function
proc_train_test_data = xgb_trainKfold_test_process(train_dtable = train_dt,
                                                   test_dtable = test_dt,
                                                   cont_cols = cont_vars,
                                                   categ_cols = categ_vars,
                                                   id_col = id_vars,
                                                   weight_col = 'earned_exposure',
                                                   y_col = 'pure_premium',
                                                   k = 10)

# Split K-Fold Train List and Processed Test Set
train_dt_proc_kfold_split = proc_train_test_data[[1]]
test_dt_proc = proc_train_test_data[[2]]
rm(proc_train_test_data)


### Read Saved Models and Generate Out of Sample Predictions
######################################################################################################

# Load GLM and XGBoost Models
saved_glm_model = h2o.loadModel(paste0(config_save_name_glm_model, list.files(config_save_name_glm_model)))
saved_xgb_model = readRDS(config_save_name_xgb_model)

# Generate K-Fold Out of Sample Predictions
out_of_sample_pred = xgb_glm_k_fold_oof_pred(train_dtable_list = train_dt_proc_kfold_split[1:2],
                                             xgb_model = saved_xgb_model,
                                             glm_model = saved_glm_model,
                                             y_col = 'pure_premium',
                                             id_cols = 'id',
                                             weight_col = 'earned_exposure')


# Performance Check on Individual Models
xgb_rmse = sqrt(mean((out_of_sample_pred$pred_xgb - out_of_sample_pred$pure_premium)^2))
glm_rmse = sqrt(mean((out_of_sample_pred$pred_glm - out_of_sample_pred$pure_premium)^2))


### Grid Search on Ensemble
######################################################################################################

# Split Validation Set Unique ID Values
set.seed(config_random_seed)
unique_ids = unique(out_of_sample_pred$id)
train_ids = sample(unique_ids, round(length(unique_ids) * 0.8, 0))
valid_ids = setdiff(unique_ids, train_ids)

# Create Train and Validation Sets
out_of_sample_pred_train = xgb.DMatrix(as.matrix(out_of_sample_pred[out_of_sample_pred$id %in% train_ids, c('pred_xgb', 'pred_glm')]),
                                       label = out_of_sample_pred[out_of_sample_pred$id %in% train_ids, c('pure_premium')])

out_of_sample_pred_valid = xgb.DMatrix(as.matrix(out_of_sample_pred[out_of_sample_pred$id %in% valid_ids, c('pred_xgb', 'pred_glm')]),
                                       label = out_of_sample_pred[out_of_sample_pred$id %in% valid_ids, c('pure_premium')])

# Run Ensemble Grid Search
ens_grid_results = xgb_early_stop_grid_search(train_matrix = out_of_sample_pred_train,
                                              valid_matrix = out_of_sample_pred_valid,
                                              hyper_param_list = config_hparam_ens_list,
                                              eval_metric = 'rmse',
                                              n_models = 80,
                                              k = 10,
                                              nrounds = 5000,
                                              stopping_rounds = 3,
                                              print_every = 1)

# Extract Best Parameters, Save Results
grid_summary = ens_grid_results[[1]]
best_params = ens_grid_results[[2]]
write.csv(grid_summary, config_save_name_ens_grid, row.names = FALSE)


### Fit Final Model & Predict on Test Set
######################################################################################################
# Fit Using Selected Hyperparameters
fit_ens = xgb.train(data = out_of_sample_pred_train,
                    watchlist = list(train = out_of_sample_pred_train, validate = out_of_sample_pred_valid),
                    eval_metric = 'mae',
                    maximize = FALSE,
                    nrounds = 5000,
                    early_stopping_rounds = 3,
                    stopping_metric = 'mae',
                    colsample_bytree = best_params$colsample_bytree,
                    learn_rate = best_params$learn_rate,
                    gamma = best_params$gamma,
                    max_depth = best_params$max_depth,
                    min_child_weight = best_params$min_child_weight,
                    reg_alpha = best_params$reg_alpha,
                    reg_lambda = best_params$reg_lambda,
                    subsample = best_params$subsample,
                    verbose = TRUE)

# Load GLM and XGBoost Test Set Predictions
xgb_test_pred = read.csv(config_save_name_xgb_test_pred) %>% dplyr::rename(pred_xgb = loss)
glm_test_pred = read.csv(config_save_name_glm_test_pred) %>% dplyr::rename(pred_glm = loss)
xgb_glm_test_pred = xgb_test_pred %>%
  dplyr::inner_join(glm_test_pred, by = 'id')

# Predict on Test Set
test_pred = test_dt[, 'id'] %>% as.data.frame()
test_pred$loss = as.data.frame(predict(fit_ens, xgb.DMatrix(as.matrix(xgb_glm_test_pred[, c('pred_xgb', 'pred_glm')]))))$predict
write.csv(test_pred, config_save_name_ens_test_pred, row.names = FALSE)

# Save Model Object
saveRDS(fit_ens, config_save_name_ens_model)
