### File Paths (config_fp...)
######################################################################################################
# Raw Data
config_fp_raw_data_dir = 'D:/insurance_data/'
config_fp_raw_train_file = 'train.csv'
config_fp_raw_test_file = 'test.csv'

# Processed Data
config_fp_proc_data_dir = 'D:/processed_insurance_data/'


### Packages (config_pkg...)
######################################################################################################
# Load Required Packages
config_pkg_req_list = c('assertthat', 'caret', 'data.table', 'h2o', 'tidyverse', 'xgboost')
lapply(config_pkg_req_list, require, character.only = TRUE)


### Modukes (config_module...)
######################################################################################################
config_module_list = paste('src/', list.files('src/'), sep = '')
sapply(config_module_list, source, .GlobalEnv)


### Settings & Random Seed
######################################################################################################
options(scipen = 999)
config_random_seed = 7022020


### File Save Names (config_save_name...)
######################################################################################################
config_save_name_lofo_plot = 'feature_importance/feature_importance_output/lofo_feature_importance.png'
config_save_name_single_varimp_plot = 'feature_importance/feature_importance_output/single_var_importance.png'
config_save_name_recommended_features = 'feature_importance/feature_importance_output/recommended_features.csv'
config_save_name_alpha_lambda_grid = 'feature_importance/feature_importance_output/alpha_lambda_grid_search.csv'
config_save_name_tweedie_grid = 'feature_importance/feature_importance_output/tweedie_grid_search.csv'
config_save_name_xgb_grid = 'feature_importance/feature_importance_output/xgb_grid_search.csv'
config_save_name_ens_grid = 'feature_importance/feature_importance_output/ens_grid_search.csv'
config_save_name_glm_test_pred = paste0(config_fp_proc_data_dir, 'glm_prediction_output.csv')
config_save_name_xgb_test_pred = paste0(config_fp_proc_data_dir, 'xgb_prediction_output.csv')
config_save_name_ens_test_pred = paste0(config_fp_proc_data_dir, 'ens_prediction_output.csv')
config_save_name_glm_model = 'D:/model_save/glm/'
config_save_name_xgb_model = 'D:/model_save/xgb/xgb_model.rds'
config_save_name_ens_model = 'D:/model_save/xgb/ens_model.rds'


### GLM Hyperparameters to Tune (config_hparam...)
######################################################################################################
config_hparam_alpha_lambda_list = list(lambda = c(0, 0.00001, 0.00005, 0.0001, 0.0005, 0.001),
                                       alpha = seq(0, 1, 0.1))

config_hparam_tweedie_list = list(tweedie_variance_power = seq(1.05, 1.95, 0.05))



### GLM Hyperparameters to Tune (config_hparam_xgb...)
######################################################################################################
# Static Grid & Model Parameters
config_static_param_xgb = list(stopping_rounds = 20,
                               stopping_tolerance = 0.0001,
                               stopping_metric = 'deviance',
                               strategy = 'RandomDiscrete',
                               max_models = 50,
                               seed = config_random_seed,
                               nrounds = 5000,
                               nfolds = 10)

config_xgb_search_strategy = list(strategy = config_static_param_xgb[['strategy']],
                                  max_models = config_static_param_xgb[['max_models']], 
                                  seed = config_static_param_xgb[['seed']],
                                  stopping_rounds = config_static_param_xgb[['stopping_rounds']],
                                  stopping_tolerance = config_static_param_xgb[['stopping_tolerance']],
                                  stopping_metric = config_static_param_xgb[['stopping_metric']])

# Baseline Hyperparameters
config_hparam_xgb_starter = list(colsample_bytree = 0.6,
                                 learn_rate = 0.04,
                                 gamma = 0.0,
                                 max_depth = 6,
                                 min_child_weight = 10,
                                 reg_alpha = 0.0,
                                 reg_lambda = 1.0,
                                 subsample = 0.7)

# Hyperparameter Search Space
config_hparam_xgb_list = list(colsample_bytree = seq(0.4, 0.8, 0.05),
                              learn_rate = seq(0.005, 0.04, 0.005),
                              gamma = seq(0, 5, 0.25),
                              max_depth = seq(4, 12, 1),
                              min_child_weight = seq(0, 20, 5),
                              reg_alpha = c(0, 0.00001, 0.001, 0.01),
                              reg_lambda = c(0.5, 0.75, 1.0),
                              subsample = seq(0.4, 0.9, 0.1))


# Ensemble Hyperparameter Search Space
config_hparam_ens_list = list(colsample_bytree = 1,
                              learn_rate = seq(0.005, 0.08, 0.005),
                              gamma = 0,
                              max_depth = seq(3, 8, 1),
                              min_child_weight = seq(0, 50, 10),
                              reg_alpha = 0,
                              reg_lambda = 0,
                              subsample = seq(0.4, 0.9, 0.1))


