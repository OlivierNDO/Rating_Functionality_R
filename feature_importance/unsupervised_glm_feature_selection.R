### Configuration
######################################################################################################
# Load Configured File Paths, Settings, Src Modules
source('configuration/configuration.R')
set.seed(config_random_seed)

### Read Training Set
######################################################################################################
train_dt = fread(paste0(config_fp_raw_data_dir, config_fp_raw_train_file))


### Initialize H2O Session
######################################################################################################
h2o.init()


### Define Columns, Create Dummy Earned Exposure & Pure Premium Fields
######################################################################################################
# Column Definitions
id_vars = 'id'
categ_vars = colnames(train_dt)[colnames(train_dt) %like% 'cat']
cont_vars = colnames(train_dt)[colnames(train_dt) %like% 'cont']
all_x_vars = c(categ_vars, cont_vars)

# Dummy Earned Exposures & Pure Premium
train_dt$earned_exposure = sample(x = c(seq(1,12)/12, rep(1,5)), size = nrow(train_dt), replace = TRUE)
train_dt[, pure_premium := loss / earned_exposure]


### Define Column Class Types
######################################################################################################
dtable_define_variable_classes(dtable = train_dt, categ_vars = categ_vars, cont_vars = cont_vars)


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
