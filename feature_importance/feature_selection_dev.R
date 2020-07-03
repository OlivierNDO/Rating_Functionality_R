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

# Dummy Earned Exposures & Pure Premium
train_dt$earned_exposure = sample(x = c(seq(1,12)/12, rep(1,5)), size = nrow(train_dt), replace = TRUE)
train_dt[, pure_premium := loss / earned_exposure]


### Define Column Class Types
######################################################################################################
dtable_define_variable_classes(dtable = train_dt, categ_vars = categ_vars, cont_vars = cont_vars)


### Create H2O Frames
######################################################################################################
train_dt_h2o = as.h2o(train_dt[, c('pure_premium', 'earned_exposure', c(cont_vars[1:2], categ_vars[1:2])), with = FALSE])


### Define Columns, Create Dummy Earned Exposure & Pure Premium Fields
######################################################################################################
x_vars = c(cont_vars[1:2], categ_vars[1:2])
train_glm = h2o.glm(x = x_vars,
                    y = 'pure_premium',
                    weights_column = 'earned_exposure',
                    family = 'tweedie',
                    tweedie_link_power = 1,
                    tweedie_variance_power = 1.5,
                    remove_collinear_columns = FALSE,
                    lambda_search = FALSE,
                    training_frame = train_dt_h2o)


param_list = list(lambda = seq(0, 0.15, 0.01))

train_grid = h2o.grid(algorithm = 'glm',
                      x = x_vars,
                      y = 'pure_premium',
                      weights_column = 'earned_exposure',
                      family = 'tweedie',
                      tweedie_link_power = 1,
                      tweedie_variance_power = 1.5,
                      remove_collinear_columns = FALSE,
                      lambda_search = FALSE,
                      hyper_params = param_list,
                      alpha = 1,
                      nfolds = 10,
                      training_frame = train_dt_h2o)



best_params = best_h2o_grid_params(h2o_grid_object = train_grid, use_metric = 'mean_residual_deviance', metric_optimum = 'min')

grid_results = extract_kfold_measures_h2o_grid(h2o_grid_object = train_grid)


