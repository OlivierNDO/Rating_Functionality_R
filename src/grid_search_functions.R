### Define Functions Related to H2O Grid Searches
######################################################################################################



#' Extract holdout set metrics from h2o grid search object
#' @param h2o_grid_object H2OGrid object
#' @returns data.frame object with fields:
#'          > iteration
#'          > value
#'          > iteration_num
#'          > metric
#'          > <hyperparameter fields>
#'          > hyper_param_label
extract_kfold_measures_h2o_grid = function(h2o_grid_object){
  # Create Iteration List, Model IDs, Extract Hyperparameters
  fold_result_list = list()
  model_ids = h2o_grid_object@model_ids
  grid_hyper_params = h2o_grid_object@hyper_names %>% unlist()
  
  # Loop Over Model IDs in Grid Object
  for (i in 1:length(model_ids)){
    model_i = h2o.getModel(h2o_grid_object@model_ids[[i]])
    summ_i = model_i@model$cross_validation_metrics_summary %>% 
      as.data.frame() %>%
      tidyr::gather('iteration', 'value') %>%
      dplyr::filter(iteration %!in% c('sd', 'mean')) %>%
      dplyr::mutate(iteration = gsub('cv_|_valid', '', iteration)) %>%
      dplyr::mutate(iteration_num = as.numeric(iteration)) %>%
      dplyr::mutate(iteration = paste0('Test Set ', gsub('mean', 'Mean', iteration))) %>%
      dplyr::mutate(value = as.numeric(value))
    summ_i$metric = rep(row.names(model_i@model$cross_validation_metrics_summary), model_i@parameters$nfolds)
    hyper_param_values = c()
    for (j in 1:length(grid_hyper_params)){
      summ_i[, grid_hyper_params[j]] = as.numeric(model_i@parameters[unlist(grid_hyper_params)[j]])
      hyper_param_values = c(hyper_param_values, as.numeric(model_i@parameters[unlist(grid_hyper_params)[j]]))
    }
    summ_i$hyper_param_label = paste(paste(grid_hyper_params, hyper_param_values), collapse = ', ')
    fold_result_list[[i]] = summ_i
  }
  
  # Return Output
  output_df = do.call(rbind.data.frame, fold_result_list)
  return (output_df)
}



#' Wrapper around extract_kfold_measures_h2o_grid() function that returns the hyperparameter
#' combination with the lowest or highest average value
#' @param h2o_grid_object H2OGrid object
#' @param use_metric Metric to use in determining optimal parameter combination; Defaults to 'mean_residual_deviance'
#' @param metric_optimum Accepts values of 'min' or 'max', indicating whether <use_metric> is to be minimized or maximized
#' @returns data.frame object with best parameter combinations based on defined metric and optimization criteria
best_h2o_grid_params = function(h2o_grid_object, use_metric = 'mean_residual_deviance', metric_optimum = 'min'){
  
  grid_results = extract_kfold_measures_h2o_grid(h2o_grid_object = h2o_grid_object) %>% dplyr::filter(metric == use_metric)
  # Subset Columns
  non_hyper_param_cols = c('iteration', 'value', 'iteration_num', 'metric', 'hyper_param_label')
  hyper_param_columns = colnames(grid_results)[colnames(grid_results) %!in% non_hyper_param_cols]
  
  # Aggregate
  summ_grid_results = grid_results[, c(hyper_param_columns, 'value')] %>%
    magrittr::set_colnames(c(paste('param_', 1:length(hyper_param_columns), sep = ''), 'value')) %>%
    group_by_at(vars(starts_with('param_'))) %>%
    dplyr::summarise_all(.funs = list(mean)) %>%
    data.frame() %>%
    dplyr::ungroup() %>%
    magrittr::set_colnames(c(hyper_param_columns, 'value'))
  
  # Sort
  if (metric_optimum == 'min'){
    summ_grid_results = summ_grid_results %>% dplyr::arrange(value)
  } else {
    summ_grid_results = summ_grid_results %>% dplyr::arrange(desc(value))
  }
  
  # Return Output
  best_params = head(summ_grid_results %>% dplyr::select(-value), 1)
  print(summ_grid_results)
  return (best_params)
}



#' Rank predictor features by running k-fold cross validation using
#' one X variable at a time and measuring average error metrics
#' @param dtable data.table object
#' @param x_cols vector of character strings representing X column names
#' @param y_col character string representing dependent variable column name
#' @param n_folds no. of k-fold splits to use in cross validation
#' @param use_metric character string representing grid search metric to use in evaluation of variables - defaults to 'mean_residual_deviance'
#'                   accepted values: 'mae', 'mean_residual_deviance', 'mse', 'r2', 'residual_deviance', 'rmse', 'rmsle'
#' @param use_mean_median whether to return k-fold results based on mean or median out of sample value - defaults to 'mean' - accepts values {'mean' | 'median'}
#' @param weight_col weight column in GLM fit - defaults to NULL
#' @param tweedie_link_power tweedie link power - defaults to 1
#' @param tweedie_variance_power tweedie variance power - defaults to 1.5
#' @param remove_collinear_columns boolean - in case of linearly dependent columns, remove some of the dependent columns - defaults to FALSE
#' @param intercept boolean - fit intercept in GLM model - defaults to TRUE
#' @param random_seed value to use in set.seed() call - defaults to 7052020
tweedie_glm_h2o_rank_order_importance = function(dtable, x_cols, y_col, n_folds = 10, use_metric = 'mean_residual_deviance',
                                                 use_mean_median = 'mean',
                                                 weight_col = NULL, offset_col = NULL,
                                                 tweedie_link_power = 1, tweedie_variance_power = 1.5,
                                                 remove_collinear_columns = FALSE, intercept = TRUE, random_seed = 7052020){
  set.seed(random_seed)
  # Assertion Statements
  accepted_use_metric_values = c('mae', 'mean_residual_deviance', 'mse', 'r2', 'residual_deviance', 'rmse', 'rmsle')
  assertthat::assert_that(use_metric %in% accepted_use_metric_values)
  
  # Optimization Mapping (i.e. higher is better vs. lower is better)
  optim_map = data.frame(metric = accepted_use_metric_values,
                         optimize_for = c('lower', 'lower', 'lower', 'higher', 'lower', 'lower', 'lower'))
  
  # Create H2O Frame
  dtable_h2o = as.h2o(dtable[, c(y_col, x_cols, weight_col, offset_col), with = FALSE])
  
  # Loop Over Individual X-Variables
  x_var_mean_results = c()
  x_var_median_results = c()
  for (i in 1:length(x_cols)){
    # Run K-Fold Grid
    use_x = x_cols[i]
    train_grid_i = h2o.grid(algorithm = 'glm',
                            x = use_x,
                            y = y_col,
                            weights_column = weight_col,
                            family = 'tweedie',
                            tweedie_link_power = tweedie_link_power,
                            tweedie_variance_power = tweedie_variance_power,
                            remove_collinear_columns = remove_collinear_columns,
                            lambda_search = FALSE,
                            hyper_params = list(),
                            lambda = 0,
                            alpha = 1,
                            nfolds = n_folds,
                            training_frame = dtable_h2o)
    
    # Aggregate Results for X Variable [i]
    grid_results_i = extract_kfold_measures_h2o_grid(h2o_grid_object = train_grid_i)
    mean_result_value = grid_results_i[grid_results_i$metric == use_metric, 'value'] %>% mean()
    median_result_value = grid_results_i[grid_results_i$metric == use_metric, 'value'] %>% median()
    x_var_mean_results = c(x_var_mean_results, mean_result_value)
    x_var_median_results = c(x_var_median_results, median_result_value)
    print(paste0(Sys.time(), ': Completed ', n_folds, '-Fold Regression | ', y_col, ' ~ ', use_x))
  }
  
  # Return Mean or Median Results
  if (use_mean_median == 'mean'){
    agg_grid_results = data.frame(x_variable = x_cols, value = x_var_mean_results, value_type = 'mean') %>%
      dplyr::mutate(metric_used = use_metric, k_folds = n_folds)
  } else {
    agg_grid_results = data.frame(x_variable = x_cols, value = x_var_median_results, value_type = 'median') %>%
      dplyr::mutate(metric_used = use_metric, k_folds = n_folds)
  }
  
  # Sort and Label Based on Metric Opitimization Being Minimum or Maximum
  if (optim_map[optim_map$metric == use_metric, 'optimize_for'] == 'lower'){
    agg_grid_results = agg_grid_results %>% dplyr::arrange(value) %>% dplyr::mutate(optimization_type = 'min')
  } else {
    agg_grid_results = agg_grid_results %>% dplyr::arrange(desc(value)) %>% dplyr::mutate(optimization_type = 'max')
  }
  
  # Return Output
  return (agg_grid_results)
}



#' Create ggplot2 object displaying output of tweedie_glm_h2o_rank_order_importance() function
#' @param output_dframe data.frame object returned from tweedie_glm_h2o_rank_order_importance() function
#' @param column_fill_color color used for fill and outline of geom_col()
#' @param strip_fill_color color used for fill in strip.background theme
#' @param strip_text_color color used in strip.text theme
#' @param plot_top_n if not NULL, number of most important variables to plot
#' @param round_txt_label_precision no. decimal points to round geom_text label
#' @param save_location file path and name to save resulting plot - defaults to NULL
plot_tweedie_glm_h2o_rank_order_importance = function(output_dframe,
                                                      column_fill_color = rgb(2, 52, 151, maxColorValue = 255),
                                                      strip_fill = rgb(55, 153, 245, maxColorValue = 255),
                                                      strip_text_color = 'white',
                                                      plot_top_n = NULL,
                                                      round_txt_label_precision = 3,
                                                      save_location = NULL){
  
  
  # K-Fold Result Descriptive Values
  optim_type = unique(output_dframe$optimization_type)
  metric_used = unique(output_dframe$metric_used)
  nfolds_fit = unique(output_dframe$k_folds)
  
  # Create ggplot Object, Sorting Contingent on Metric Optimization
  if (optim_type == 'min'){
    
    if (!is.null(plot_top_n)){
      output_dframe_head = output_dframe %>%
        dplyr::arrange(desc(value)) %>%
        head(plot_top_n)
      g = ggplot(output_dframe_head, aes(x = reorder(x_variable, desc(value)), y = value))
    } else {
      g = ggplot(output_dframe, aes(x = reorder(x_variable, desc(value)), y = value))
    }
    
    subtitle_txt = '(lower is better)' 
    
  } else {
    
    if (!is.null(plot_top_n)){
      output_dframe_head = output_dframe %>%
        dplyr::arrange(value) %>%
        head(plot_top_n)
      g = ggplot(output_dframe_head, aes(x = reorder(x_variable, value), y = value))
    } else {
      g = ggplot(output_dframe, aes(x = reorder(x_variable, value), y = value))
    }
    
    subtitle_txt = '(higher is better)'
  }
  
  # Caption Text
  if (!is.null(plot_top_n)){
    caption_txt = paste0('Top ', plot_top_n, ' variables by importance shown')
  } else {
    caption_txt = ''
  }
  
  # Create and Return Plot
  g_output = g +
    theme_bw() +
    facet_wrap(~value_type) +
    geom_col(alpha = 0.25, color = column_fill_color, fill = column_fill_color) +
    labs(title = paste0('Single-Predictor ', nfolds_fit, '-Fold Results'),
         x = 'Variable',
         y = camel_case_sentence(gsub('_', ' ', metric_used)),
         subtitle = subtitle_txt,
         caption = caption_txt) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5),
          strip.background = element_rect(fill = strip_fill),
          strip.text = element_text(color = strip_text_color, face = 'bold')) +
    geom_text(aes(label = round(value, round_txt_label_precision), y = value * 1.04),
              color = column_fill_color) +
    coord_flip()
  
  # Save Location - If Applicable
  if (!is.null(save_location)){
    ggsave(save_location, g_output)
    print(paste0(Sys.time(), ' plot saved to file: ', save_location))
  }
  
  return (g_output)
}



#' Determines single-variable importance with tweedie_glm_h2o_rank_order_importance() function
#' and iteratively adds variables sorted by importance, calculating K-fold results and
#' adding or omitting based on mean or median out of sample metrics. Returns vector of recommended variables.
#' @param dtable data.table object
#' @param x_cols vector of character strings representing X column names
#' @param y_col character string representing dependent variable column name
#' @param n_folds no. of k-fold splits to use in cross validation
#' @param min_improvement_percent minimum relative improvement required to add variable to list - defaults to 0.0
#' @param use_metric character string representing grid search metric to use in evaluation of variables - defaults to 'mean_residual_deviance'
#'                   accepted values: 'mae', 'mean_residual_deviance', 'mse', 'r2', 'residual_deviance', 'rmse', 'rmsle'
#' @param use_mean_median whether to return k-fold results based on mean or median out of sample value - defaults to 'mean' - accepts values {'mean' | 'median'}
#' @param weight_col weight column in GLM fit - defaults to NULL
#' @param tweedie_link_power tweedie link power - defaults to 1
#' @param tweedie_variance_power tweedie variance power - defaults to 1.5
#' @param remove_collinear_columns boolean - in case of linearly dependent columns, remove some of the dependent columns - defaults to FALSE
#' @param intercept boolean - fit intercept in GLM model - defaults to TRUE
#' @param random_seed value to use in set.seed() call - defaults to 7052020
tweedie_glm_h2o_iterative_feature_selection = function(dtable, x_cols, y_col, n_folds = 10, min_improvement_percent = 0.0,
                                                       use_metric = 'mean_residual_deviance',
                                                       use_mean_median = 'mean',
                                                       weight_col = NULL, offset_col = NULL,
                                                       tweedie_link_power = 1, tweedie_variance_power = 1.5,
                                                       remove_collinear_columns = FALSE, intercept = TRUE, random_seed = 7052020){
  set.seed(random_seed)
  
  # Calculate Single-Variable Importance 
  one_var_importance = tweedie_glm_h2o_rank_order_importance(dtable, x_cols, y_col, n_folds, use_metric,
                                                             use_mean_median, weight_col, offset_col,
                                                             tweedie_link_power, tweedie_variance_power,
                                                             remove_collinear_columns, intercept, random_seed)
  
  # Iteratively Add Variables, Calculate K-Fold Results
  optim_type = unique(one_var_importance$optimization_type)
  sorted_x_vars = one_var_importance$x_variable
  grid_result_list = c(one_var_importance$value[1])
  vars_used_list = c(sorted_x_vars[1])
  use_x_vars = sorted_x_vars[1]
  
  for (i in 2:length(sorted_x_vars)){
    add_x_var = sorted_x_vars[i]
    use_x_vars = c(use_x_vars, add_x_var)
    train_grid_i = h2o.grid(algorithm = 'glm',
                            x = use_x_vars,
                            y = y_col,
                            weights_column = weight_col,
                            family = 'tweedie',
                            tweedie_link_power = tweedie_link_power,
                            tweedie_variance_power = tweedie_variance_power,
                            remove_collinear_columns = remove_collinear_columns,
                            lambda_search = FALSE,
                            hyper_params = list(),
                            alpha = 1,
                            nfolds = n_folds,
                            training_frame = as.h2o(dtable[, c(y_col, use_x_vars, weight_col, offset_col), with = FALSE]))
    
    # Extract Results
    if (unique(one_var_importance$value_type) == 'mean'){
      grid_result_init = extract_kfold_measures_h2o_grid(h2o_grid_object = train_grid_i)
      grid_result_i = grid_result_init[grid_result_init$metric == use_metric, 'value'] %>% mean()
    } else {
      grid_result_init = extract_kfold_measures_h2o_grid(h2o_grid_object = train_grid_i)
      grid_result_i = grid_result_init[grid_result_init$metric == use_metric, 'value'] %>% median()
    }
    
    # Remove Added Variable if Results Are Not Improved
    improvement_bool = ifelse(optim_type == 'min',
                              grid_result_i < min(grid_result_list, na.rm = TRUE),
                              grid_result_i > max(grid_result_list, na.rm = TRUE))
    
    # Calculate % Improvement
    improvement_perc = ifelse(optim_type == 'min',
                              (min(grid_result_list, na.rm = TRUE) - grid_result_i) / min(grid_result_list, na.rm = TRUE),
                              (max(grid_result_list, na.rm = TRUE) - grid_result_i) / max(grid_result_list, na.rm = TRUE))
    
    improvement_label = decimal_to_perc_label(decimal_value = improvement_perc, round_precision = 4)
    improvement_threshold = decimal_to_perc_label(decimal_value = min_improvement_percent, round_precision = 4)
    
    # Print Timestamp Message with Decision on Variable
    if (is.na(improvement_bool)){
      use_x_vars = use_x_vars[use_x_vars != add_x_var]
      print(paste0(Sys.time(), ' ', add_x_var, ' resulted in NA value for improvement calculation.',
                   ' Not adding to the list of x variables.'))
    } else if (improvement_bool == FALSE){
      use_x_vars = use_x_vars[use_x_vars != add_x_var]
      print(paste0(Sys.time(), ' ', add_x_var, ' did not improve ', use_metric,
                   ' over ', n_folds, ' folds. Not adding to the list of x variables.'))
    } else if (improvement_bool == TRUE & improvement_perc > min_improvement_percent) {
      print(paste0(Sys.time(), ' ', add_x_var, ' improved ', use_metric,
                   ' over ', n_folds, ' folds by ', improvement_label, '. Adding to the list of x variables'))
    } else if (improvement_bool == TRUE & improvement_perc < min_improvement_percent) {
      use_x_vars = use_x_vars[use_x_vars != add_x_var]
      print(paste0(Sys.time(), ' ', add_x_var, ' improved ', use_metric,
                   ' over ', n_folds, ' folds by ', improvement_label,
                   ' (less than ', improvement_threshold, ' minimum) ', '. Not adding to the list of x variables.'))
    } else {
      print(paste0(Sys.time(), ' ', add_x_var, ' worsened ', use_metric,
                   ' over ', n_folds, ' folds by ', improvement_label, '. Not adding to the list of x variables.'))
    }
    
    # Append to Lists
    grid_result_list = c(grid_result_list, grid_result_i)
    vars_used_list = c(vars_used_list, use_x_vars)
    print(paste0(Sys.time(), ' Completed ', i, ' of ', length(sorted_x_vars), ' stepwise additions'))
  }
  
  return (use_x_vars)
}



#' Measures K-fold leave-one-out feature importance given a data.table and vector of features.
#' e.g. given feature set {x1, x2}, calculate change in error metric from removing x1, then removing x2
#' The worse an error metric becomes due to removing a variable, the more important it is.
#' @param dtable data.table object
#' @param x_cols vector of character strings representing X column names
#' @param y_col character string representing dependent variable column name
#' @param n_folds no. of k-fold splits to use in cross validation
#' @param use_metric character string representing grid search metric to use in evaluation of variables - defaults to 'mean_residual_deviance'
#'                   accepted values: 'mae', 'mean_residual_deviance', 'mse', 'residual_deviance', 'rmse', 'rmsle'
#' @param use_mean_median whether to return k-fold results based on mean or median out of sample value - defaults to 'mean' - accepts values {'mean' | 'median'}
#' @param weight_col weight column in GLM fit - defaults to NULL
#' @param tweedie_link_power tweedie link power - defaults to 1
#' @param tweedie_variance_power tweedie variance power - defaults to 1.5
#' @param remove_collinear_columns boolean - in case of linearly dependent columns, remove some of the dependent columns - defaults to FALSE
#' @param intercept boolean - fit intercept in GLM model - defaults to TRUE
#' @param random_seed value to use in set.seed() call - defaults to 7052020
tweedie_glm_h2o_kfold_lofo = function(dtable, x_cols, y_col, n_folds = 10,
                                      use_metric = 'mean_residual_deviance',
                                      use_mean_median = 'mean',
                                      weight_col = NULL, offset_col = NULL,
                                      tweedie_link_power = 1, tweedie_variance_power = 1.5,
                                      remove_collinear_columns = FALSE, intercept = TRUE, random_seed = 7052020){
  set.seed(random_seed)
  # Assertion Statements
  accepted_use_metric_values = c('mae', 'mean_residual_deviance', 'mse','residual_deviance', 'rmse', 'rmsle')
  assertthat::assert_that(use_metric %in% accepted_use_metric_values)
  
  # Calculate Error Metric for All Fields
  train_grid_all = h2o.grid(algorithm = 'glm',
                            x = x_cols,
                            y = y_col,
                            weights_column = weight_col,
                            family = 'tweedie',
                            tweedie_link_power = tweedie_link_power,
                            tweedie_variance_power = tweedie_variance_power,
                            remove_collinear_columns = remove_collinear_columns,
                            lambda_search = FALSE,
                            hyper_params = list(),
                            alpha = 1,
                            nfolds = n_folds,
                            training_frame = as.h2o(dtable[, c(y_col, x_cols, weight_col, offset_col), with = FALSE]))
  
  # Extract Results
  if (use_mean_median == 'mean'){
    grid_result_init = extract_kfold_measures_h2o_grid(h2o_grid_object = train_grid_all)
    grid_result_all = grid_result_init[grid_result_init$metric == use_metric, 'value'] %>% mean()
  } else {
    grid_result_init = extract_kfold_measures_h2o_grid(h2o_grid_object = train_grid_all)
    grid_result_all = grid_result_init[grid_result_init$metric == use_metric, 'value'] %>% median()
  }
  
  # Iteratively Remove Variables, Calculate K-Fold Results
  grid_result_list = c()
  var_removed_list = c()
  
  for (i in 1:length(x_cols)){
    remove_x = x_cols[i]
    use_x_vars = x_cols[x_cols != remove_x]
    train_grid_i = h2o.grid(algorithm = 'glm',
                            x = use_x_vars,
                            y = y_col,
                            weights_column = weight_col,
                            family = 'tweedie',
                            tweedie_link_power = tweedie_link_power,
                            tweedie_variance_power = tweedie_variance_power,
                            remove_collinear_columns = remove_collinear_columns,
                            lambda_search = FALSE,
                            hyper_params = list(),
                            alpha = 1,
                            nfolds = n_folds,
                            training_frame = as.h2o(dtable[, c(y_col, use_x_vars, weight_col, offset_col), with = FALSE]))
    
    # Extract Results
    if (use_mean_median == 'mean'){
      grid_result_init = extract_kfold_measures_h2o_grid(h2o_grid_object = train_grid_i)
      grid_result_i = grid_result_init[grid_result_init$metric == use_metric, 'value'] %>% mean()
    } else {
      grid_result_init = extract_kfold_measures_h2o_grid(h2o_grid_object = train_grid_i)
      grid_result_i = grid_result_init[grid_result_init$metric == use_metric, 'value'] %>% median()
    }
    
    # Append to Lists
    grid_result_list = c(grid_result_list, grid_result_i)
    var_removed_list = c(var_removed_list, remove_x)
    print(paste0(Sys.time(), ' Completed ', n_folds, '-fold CV after removing ', remove_x))
  }
  
  # Aggregate Results
  agg_df = data.frame(variable_removed = var_removed_list,
                      value = grid_result_list)  %>%
    dplyr::mutate(metric_used = use_metric, k_folds = n_folds, value_type = use_mean_median) %>%
    dplyr::mutate(value_delta = value - grid_result_all) %>%
    dplyr::mutate(value_percent_delta = value_delta / grid_result_all) %>%
    dplyr::mutate(value_percent_delta_label = decimal_to_perc_label(value_percent_delta, 5)) %>%
    dplyr::mutate(variable_inclusion = factor(ifelse(value_delta > 0, 'Improves Performance', 'Worsens Performance'),
                                              levels = c('Improves Performance', 'Worsens Performance')))
  
  
  return (agg_df)
}



#' Create ggplot2 object displaying output of tweedie_glm_h2o_kfold_lofo() function
#' @param output_dframe data.frame object returned from tweedie_glm_h2o_kfold_lofo() function
#' @param save_location file path and name to save resulting plot - defaults to NULL
plot_tweedie_glm_h2o_kfold_lofo = function(output_dframe, save_location = NULL){
  # Create Plot
  g = ggplot(output_dframe, aes(x = reorder(variable_removed, value_percent_delta), y = value_percent_delta,
                                fill = variable_inclusion, color = variable_inclusion)) +
    theme_bw() +
    scale_color_manual(values = c('forestgreen', 'darkred')) +
    scale_fill_manual(values = c('forestgreen', 'darkred')) +
    geom_col(alpha = 0.25) +
    theme(legend.position = 'bottom',
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5)) +
    coord_flip() +
    labs(fill = 'Impact of Including Variable',
         color = 'Impact of Including Variable',
         y = paste0('% Change in ', camel_case_sentence(gsub('_', ' ', unique(output_dframe$metric_used)))),
         x = 'Feature',
         title = 'Leave-One Feature Out (LOFO) Importance')
  
  # Save Location - If Applicable
  if (!is.null(save_location)){
    ggsave(save_location, g)
    print(paste0(Sys.time(), ' plot saved to file: ', save_location))
  }
  
  return (g)
}



#' Iterate over a sample of hyperparameter space with xgboost.cv, using a predefined
#' validation set for early stopping. Return a data.frame object with the hyperparameters
#' and average test set evaluation metric over k folds.
#' @param train_matrix training set xgb.DMatrix object
#' @param valid_matrix validation set xgb.DMatrix object
#' @param hyper_param_list list of hyperparameters. Must include (and is limited to):
#' colsample_bytree, learn_rate, gamma, max_depth, min_child_weight, reg_alpha, reg_lambda, and subsample
#' @param n_models no. of unique hyperparameter combinations to try in grid search.
#' this will be used to sample all possible hyperparameter combinations
#' @param k integer, number of folds to use in grid search - defaults to 10
#' @param nrounds number of rounds to use xgboost fitting - defaults to 5000
#' @param stopping_rounds number of rounds after which the validation
#'  performance has stopped improving to stop training - defaults to 20
#' @param eval_metric metric used in xgboost evaluation and for early stopping - defaults to 'rmse'
#' @param maximize_eval_metric boolean indicating whether loss function is maximizing or minimizing
#' @param random_seed integer used in set.seed() call before sampling hyperparameter space
#' @return data.frame object hyperparameters and average test set evaluation metric over k folds.
xgb_early_stop_grid_search = function(train_matrix, valid_matrix, hyper_param_list, n_models,
                                      k = 10, nrounds = 5000, stopping_rounds = 20,
                                      eval_metric = 'rmse', maximize_eval_metric = FALSE, random_seed = 7072020){
  
  # Sample Hyperparameter Space
  set.seed(random_seed)
  param_grid_df = expand.grid(hyper_param_list)
  sample_rows = sample(1:nrow(param_grid_df), n_models)
  sample_grid_df = param_grid_df[sample_rows,]
  sample_grid_df[, paste0('test_set_', eval_metric)] = numeric()
  
  # Loop Over Sampled Hyperparameters
  for (i in 1:nrow(sample_grid_df)){
    print(paste0(Sys.time(), ' starting ', k, '-fold search on hyperparameter set ', i, ' of ', nrow(sample_grid_df)))
    
    fit_xgb = xgb.cv(data = train_matrix,
                     watchlist = list(train = train_matrix,validate = train_matrix),
                     nfold = k,
                     eval_metric = eval_metric,
                     maximize = maximize_eval_metric,
                     nrounds = nrounds,
                     early_stopping_rounds = stopping_rounds,
                     stopping_metric = eval_metric,
                     colsample_bytree = sample_grid_df[i, 'colsample_bytree'],
                     learn_rate = sample_grid_df[i, 'learn_rate'],
                     gamma = sample_grid_df[i, 'gamma'],
                     max_depth = sample_grid_df[i, 'max_depth'],
                     min_child_weight = sample_grid_df[i, 'min_child_weight'],
                     reg_alpha = sample_grid_df[i, 'reg_alpha'],
                     reg_lambda = sample_grid_df[i, 'reg_lambda'],
                     subsample = sample_grid_df[i, 'subsample'],
                     verbose = TRUE)
    sample_grid_df[i, paste0('test_set_', eval_metric)] = fit_xgb$evaluation_log[, paste0('test_', eval_metric, '_mean')] %>% min()
  }
  return (sample_grid_df)
}