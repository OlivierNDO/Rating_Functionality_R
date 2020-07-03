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