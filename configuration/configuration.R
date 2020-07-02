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
config_pkg_req_list = c('data.table', 'h2o', 'tidyverse')
lapply(config_pkg_req_list, require, character.only = TRUE)


### Modukes (config_module...)
######################################################################################################
config_module_list = c('src/processing_functions.R')
sapply(config_module_list, source, .GlobalEnv)


### Settings & Random Seed
######################################################################################################
options(scipen = 999)
config_random_seed = 7022020