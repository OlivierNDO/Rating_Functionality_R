### File Paths (config_fp...)
######################################################################################################
# Raw Data
config_fp_raw_data_dir = 'D:/insurance_data/'
config_fp_raw_train_file = 'train.csv'
config_fp_raw_train_file = 'test.csv'

# Processed Data
config_fp_proc_data_dir = 'D:/processed_insurance_data/'


### Packages (config_pkg...)
######################################################################################################
# Load Required Packages
config_pkg_req_list = c('data.table', 'h2o', 'tidyverse')
lapply(config_pkg_req_list, require, character.only = TRUE)


### Settings
######################################################################################################
options(scipen = 999)