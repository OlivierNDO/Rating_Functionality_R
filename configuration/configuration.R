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
config_pkg_req_list = c('assertthat', 'data.table', 'h2o', 'tidyverse')
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
config_save_name_glm_test_pred = paste0(config_fp_proc_data_dir, 'glm_prediction_output.csv')

### Hyperparameters to Tune (config_hparam...)
######################################################################################################
config_hparam_alpha_lambda_list = list(lambda = c(0, 0.00001, 0.00005, 0.0001, 0.0005, 0.001),
                                       alpha = seq(0, 1, 0.1))


