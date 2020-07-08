### Configuration
######################################################################################################
# Load Configured File Paths, Settings, Src Modules
source('configuration/configuration.R')
set.seed(config_random_seed)


### Read Training and Test Sets
######################################################################################################
train_dt = fread(paste0(config_fp_raw_data_dir, config_fp_raw_train_file))
test_dt = fread(paste0(config_fp_raw_data_dir, config_fp_raw_test_file))


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

### Run Data Processing Pipeline to Generate Transformed Data.table Objects
######################################################################################################
# Run Pipeline Function
train_test_dtable_list = xgb_train_valid_test_process(train_dtable = train_dt,
                                                      test_dtable = test_dt,
                                                      cont_cols = cont_vars,
                                                      categ_cols = categ_vars,
                                                      id_col = id_vars,
                                                      weight_col = 'earned_exposure',
                                                      y_col = 'pure_premium',
                                                      min_sparsity = 0.005,
                                                      max_homogeneity = 0.9975)

# Separate Train, Validation, and Test from Pipeline Output
train_dt_proc = train_test_dtable_list[[1]]
valid_dt_proc = train_test_dtable_list[[2]]
test_dt_proc = train_test_dtable_list[[3]]
rm(train_test_dtable_list, train_dt, test_dt)


### Hyperparameter Tuning
######################################################################################################

# Run Grid Search
xgb_grid_results = xgb_early_stop_grid_search(train_matrix = train_dt_proc, valid_matrix = valid_dt_proc,
                                              hyper_param_list = config_hparam_xgb_list,
                                              n_models = 15, k = 5, nrounds = 5000, stopping_rounds = 20)

# Select Parameters


### Fit Final Model & Predict on Test Set
######################################################################################################

# Fit Using Selected Features & Hyperparameters

# Read Test Set & Predict

# Save Model Object




