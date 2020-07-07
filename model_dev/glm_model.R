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
train_dt[, pure_premium := loss / earned_exposure]


### Minimal Feature Transformation - Create Squared & Cubed Terms of All Continuous Variables
######################################################################################################
squared_cols = create_exponential_terms(dtable = train_dt, x_cols = cont_vars, power = 2, suffix = '_squared')
cubed_cols = create_exponential_terms(dtable = train_dt, x_cols = cont_vars, power = 3, suffix = '_cubed')
all_x_vars = c(categ_vars, cont_vars, squared_cols, cubed_cols)


### Define Column Class Types
######################################################################################################
dtable_define_variable_classes(dtable = train_dt, categ_vars = categ_vars, cont_vars = c(cont_vars, squared_cols, cubed_cols))


### Generate Single-Variable Importance
######################################################################################################
# Table
single_var_importance = tweedie_glm_h2o_rank_order_importance(dtable = train_dt,
                                                              x_cols = all_x_vars,
                                                              y_col = 'pure_premium',
                                                              n_folds = 10, 
                                                              use_metric = 'mean_residual_deviance',
                                                              weight_col = 'earned_exposure')

# Plot
single_var_importance_plot = plot_tweedie_glm_h2o_rank_order_importance(output_dframe = single_var_importance,
                                                                        plot_top_n = 20,
                                                                        save_location = config_save_name_single_varimp_plot)


### Iteratively Add Variables Contingent on K-Fold Improvements, Starting with Most Important
######################################################################################################

# Best Features Based on Unsupervised Additive Stepwise Procedure
unsupervised_best_features = tweedie_glm_h2o_iterative_feature_selection(dtable = train_dt,
                                                                         x_cols = all_x_vars,
                                                                         y_col = 'pure_premium',
                                                                         n_folds = 10, 
                                                                         min_improvement_percent = 0.001,
                                                                         use_metric = 'mean_residual_deviance',
                                                                         weight_col = 'earned_exposure')


### Perform Leave-One-Feature Out Variable Importance - Limited to Selected Features Above
######################################################################################################

# LOFO Feature Importance
lofo_importance = tweedie_glm_h2o_kfold_lofo(dtable = train_dt,
                                             x_cols = unsupervised_best_features,
                                             y_col = 'pure_premium',
                                             n_folds = 10,
                                             use_metric = 'mean_residual_deviance',
                                             weight_col = 'earned_exposure')

# Plot
lofo_importance_plot = plot_tweedie_glm_h2o_kfold_lofo(lofo_importance, save_location = config_save_name_lofo_plot)


### Final Recommended Features for Inclusion
######################################################################################################
# Subset Recommended Features
recommended_features = lofo_importance %>%
  dplyr::filter(value_percent_delta > 0) %>%
  dplyr::filter(!is.na(variable_removed) & variable_removed != 'NA') %>%
  dplyr::select(variable_removed, value_percent_delta) %>%
  dplyr::arrange(desc(value_percent_delta)) %>%
  dplyr::rename(`Relative Importance` = value_percent_delta,
                `Recommended Feature` = variable_removed)

# Save Output to Csv File
write.csv(recommended_features, config_save_name_recommended_features, row.names = FALSE)


### Tune Alpha & Lambda with Selected Features
######################################################################################################

# Tune Lambda and Alpha
train_grid = h2o.grid(algorithm = 'glm',
                      x = recommended_features$`Recommended Feature`,
                      y = 'pure_premium',
                      weights_column = 'earned_exposure',
                      family = 'tweedie',
                      tweedie_link_power = 1,
                      tweedie_variance_power = 1.5,
                      remove_collinear_columns = FALSE,
                      lambda_search = FALSE,
                      hyper_params = config_hparam_alpha_lambda_list,
                      nfolds = 10,
                      training_frame = as.h2o(train_dt[, c('pure_premium', recommended_features$`Recommended Feature`, 'earned_exposure'), with = FALSE]))

# Extract Best Parameters, Save Results
train_grid_summary = extract_kfold_measures_h2o_grid(train_grid)
train_grid_best_params = best_h2o_grid_params(train_grid)
write.csv(train_grid_summary, config_save_name_alpha_lambda_grid, row.names = FALSE)


### Tune Tweedie Variance Power with Selected Lambda, Alpha, and Features
######################################################################################################

# Tune Tweedie Power
tweed_grid = h2o.grid(algorithm = 'glm',
                      x = recommended_features$`Recommended Feature`,
                      y = 'pure_premium',
                      weights_column = 'earned_exposure',
                      family = 'tweedie',
                      tweedie_link_power = 1,
                      remove_collinear_columns = FALSE,
                      lambda_search = FALSE,
                      alpha = train_grid_best_params$alpha,
                      lambda = train_grid_best_params$lambda,
                      hyper_params = config_hparam_tweedie_list,
                      nfolds = 10,
                      training_frame = as.h2o(train_dt[, c('pure_premium', recommended_features$`Recommended Feature`, 'earned_exposure'), with = FALSE]))

# Extract Best Parameters, Save Results
tweed_grid_summary = extract_kfold_measures_h2o_grid(tweed_grid)
tweed_grid_best_params = best_h2o_grid_params(tweed_grid, use_metric = 'mse')
write.csv(tweed_grid_summary, config_save_name_tweedie_grid, row.names = FALSE)


### Fit Final Model & Predict on Test Set
######################################################################################################

# Fit Using Selected Features & Hyperparameters
fit_glm = h2o.glm(x = recommended_features$`Recommended Feature`,
                  y = 'pure_premium',
                  weights_column = 'earned_exposure',
                  family = 'tweedie',
                  tweedie_link_power = 1,
                  tweedie_variance_power = tweed_grid_best_params$tweedie_variance_power,
                  remove_collinear_columns = FALSE,
                  lambda_search = FALSE,
                  alpha = train_grid_best_params$alpha,
                  lambda = train_grid_best_params$lambda,
                  training_frame = as.h2o(train_dt[, c('pure_premium', recommended_features$`Recommended Feature`, 'earned_exposure'), with = FALSE]))

# Read Test Set & Predict
test_pred = test_dt[, 'id'] %>% as.data.frame()
test_pred$loss = as.data.frame(h2o.predict(fit_glm, as.h2o(test_dt)))$predict
write.csv(test_pred, config_save_name_glm_test_pred, row.names = FALSE)

# Save Model Object
h2o.saveModel(fit_glm, config_save_name_glm_model)



