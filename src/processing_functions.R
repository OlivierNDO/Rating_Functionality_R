### Define Functions Related to Data Processing
######################################################################################################

#' Assert numeric and factor column types for vectors
#' of continuous and categorical variables, respectively,
#' in a data.table object
#' 
#' *note* These tranformations occur in place, i.e. function  does
#' not return an object
#' 
#' @param dtable data.table object
#' @param categ_vars vector of character strings representing categorical column names
#' @param cont_vars vector of character strings representing continuous column names
dtable_define_variable_classes = function(dtable, categ_vars, cont_vars){
  for (cat_v in categ_vars){
    dtable[, (cat_v) := as.factor(get(cat_v))]
  }
  for (cont_v in cont_vars){
    dtable[, (cont_v) := as.numeric(get(cont_v))]
  }
}


#' Opposite of %in%
'%!in%' <- function(x,y)!('%in%'(x,y))


#' Convert numeric to formatted character string showing percentage
#' @param decimal_value numeric value
#' @param round_precision number of decimal places to round after multiplying by 100
#' @example decimal_to_perc_label(0.0032423, round_precision = 2)
#' @returns "0.32%"
decimal_to_perc_label = function(decimal_value, round_precision = 2){
  return (paste0(round(decimal_value * 100, round_precision), '%'))
}


#' Convert a sngle word to camel case
#' @param word_str character string representing a single word
camel_case = function(word_str){
  first_letter = toupper(stringr::str_sub(word_str, 1, 1))
  other_letters = tolower(stringr::str_sub(word_str, 2, nchar(word_str)))
  return (paste0(first_letter, other_letters))
}


#' Convert a sentence to camel case
#' @param sentence_str character string to be converted
#' @param delimiter substring 
camel_case_sentence = function(sentence_str, delimiter = ' '){
  split_char_str = strsplit(sentence_str, ' ') %>% unlist()
  return (paste(sapply(split_char_str, camel_case), collapse = delimiter))
}



#' Create exponential versions of existing data.table columns,
#' returning a vector of newly created column names if configured
#' @param dtable data.table object
#' @param x_cols vector of existing column names to be squared, cubed, etc.
#' @param power power to raise existing variables to
#' @param suffix character string to be appended on to existing column names
#' @param return_cols boolean indicating whether to return vector of newly created columns
create_exponential_terms = function(dtable, x_cols, power = 2, suffix = '_squared', return_cols = TRUE){
  new_variables = c()
  for (i in 1:length(x_cols)){
    new_col = paste0(x_cols[i], suffix)
    dtable[, (new_col) := get(x_cols[i])^power]
    print(paste0(Sys.time(), ' created variable ', new_col, ' from ', x_cols[i]))
    new_variables = c(new_variables,  new_col)
  }
  if (return_cols == TRUE){
    return (new_variables)
  }
}



#' Create log versions of existing data.table columns,
#' returning a vector of newly created column names if configured.
#' NA and infinite values are changed to 1 before taking log.
#' All other values are transformed as log(x + 1)
#' @param dtable data.table object
#' @param x_cols vector of existing column names to be squared, cubed, etc.
#' @param power power to raise existing variables to
#' @param suffix character string to be appended on to existing column names
#' @param return_cols boolean indicating whether to return vector of newly created columns
create_log_transformed_terms = function(dtable, x_cols, suffix = '_log', return_cols = TRUE){
  new_variables = c()
  for (i in 1:length(x_cols)){
    new_col = paste0(x_cols[i], suffix)
    dtable[, (new_col) := ifelse(is.na(get(x_cols[i])) | is.infinite(get(x_cols[i])), log(1), log(get(x_cols[i]) + 1))]
    print(paste0(Sys.time(), ' created variable ', new_col, ' from ', x_cols[i]))
    new_variables = c(new_variables,  new_col)
  }
  if (return_cols == TRUE){
    return (new_variables)
  }
}



#' Find mode of a vector
#' @param vec vector from which to find the mode
find_mode = function(vec) {
  uvec = unique(vec)
  tab = tabulate(match(vec, uvec))
  return (uvec[tab == max(tab)])
}



#' Create data.frame with the percentage of rows equal to the modal
#' value and number of unique values for each column in a data.table
#' @param dtable data.table object
dtable_col_sparseness = function(dtable){
  percent_same = c()
  n_unique = c()
  n_non_modal = c()
  dtable_rows = nrow(dtable)
  for (c in colnames(dtable)){
    mode_c = find_mode(dtable[[c]])
    percent_same = c(percent_same, nrow(dtable[get(c) == mode_c]) / dtable_rows)
    n_non_modal = c(n_non_modal, nrow(dtable[get(c) != mode_c]))
    n_unique = c(n_unique, length(unique(dtable[[c]])))
  }
  output_df = data.frame(column_name = colnames(dtable),
                         percent_same = percent_same,
                         n_unique = n_unique,
                         n_non_modal = n_non_modal) %>%
    dplyr::arrange(desc(percent_same))
  return (output_df)
}



#' Count unique levels of a categorical vector
#' @param vec vector or factor
count_unique_levels = function(vec){
  vec_table = table(vec)
  output_df = data.frame(level = names(vec_table),
                         percent_of_total = as.numeric(vec_table)/length(vec)) %>%
    dplyr::arrange(desc(percent_of_total))
  return (output_df)
}



#' Return vector of non-sparse levels (i.e. % of total is above a threshold)
#' @param vec vector or factor
#' @param min_sparsity percentage frequency below which not to return values
get_nonsparse_levels = function(vec, min_sparsity = 0.01){
  unique_summary = count_unique_levels(vec = vec) %>% dplyr::filter(percent_of_total > min_sparsity)
  return (unique_summary$level)
}



#' Recode sparse categorical levels within data.table columns
#' Note that this transformation is done * in place *
#' @param dtable vector or factor
#' @param categ_cols vector of categorical column names
#' @param min_sparsity percentage frequency below which not to return values
#' @param recode_as character string to replace sparse levels with
dtable_recode_sparse_levels = function(dtable, categ_cols, min_sparsity = 0.01, recode_as = 'RECODED_CATEGORICAL_LEVEL'){
  for (c in categ_cols){
    non_sparse_levels = get_nonsparse_levels(vec = dtable[[c]], min_sparsity = min_sparsity)
    dtable[get(c) %!in% non_sparse_levels, (c) := recode_as]
  }
}



#' Remove columns in place from a data.table object with homogeneity above a specified level
#' @param dtable data.table object
#' @param consider_cols columns to consider removing
#' @param max_homogeneity maximum percentage of sameness without removing column
#' @return vector of column names removed
dtable_remove_homogenous_cols = function(dtable, consider_cols, max_homogeneity = 0.9975){
  cat_sparseness = dtable_col_sparseness(dtable = dtable[, consider_cols, with = FALSE])
  remove_cols = cat_sparseness[cat_sparseness$percent_same > max_homogeneity, 'column_name'] %>% as.vector()
  print(paste0(Sys.time(), ' removing homogenous columns: ', paste(remove_cols, collapse = ', ')))
  dtable[, c(remove_cols) := NULL]
  return (remove_cols)
}




#' Process train, validation, and test sets for xgboost. Alters data.tables in place but returns
#' final processed data.tables in a list.
#' @param train_dtable data.table object representing training set
#' @param test_dtable data.table object representing test set
#' @param cont_cols vector of continuous column names
#' @param categ_cols vector of categorical column names
#' @param id_col single identifier column name; if one does not exist, create a field with row number.
#' @param weight_col single weight column name; if one does not exist, create a field with all 1s
#' @param y_col character string representing dependent variable column name
#' @param min_sparsity minimum sparsity in categorical level without recoding
#' @param max_homogeneity maximum homogeneity in a categorical column without removing field from data.table
#' @param validation_split percentage of training set to split for validation. note that this split will occur
#' taking into account the <id_cols> field such that the same policy/person/etc. 
#' will not be in both train and validation.
#' @return list of three data.table objects, the first of which is train,
#' second of which is validation, third of which is test
xgb_train_valid_test_process = function(train_dtable, test_dtable, cont_cols, categ_cols, id_col, weight_col, y_col,
                                        min_sparsity = 0.005, max_homogeneity = 0.9975, validation_split = 0.2){
  ### Transformation
  ######################################################################################################
  
  # Create Squared & Cubed Terms of All Continuous Variables
  squared_cols_train = create_exponential_terms(dtable = train_dtable, x_cols = cont_cols, power = 2, suffix = '_squared')
  cubed_cols_train = create_exponential_terms(dtable = train_dtable, x_cols = cont_cols, power = 3, suffix = '_cubed')
  squared_cols_test = create_exponential_terms(dtable = test_dtable, x_cols = cont_cols, power = 2, suffix = '_squared')
  cubed_cols_test = create_exponential_terms(dtable = test_dtable, x_cols = cont_cols, power = 3, suffix = '_cubed')
  
  # Classify Variable Types
  dtable_define_variable_classes(dtable = train_dtable, categ_vars = categ_cols,
                                 cont_vars = c(cont_cols, squared_cols_train, cubed_cols_train))
  dtable_define_variable_classes(dtable = test_dtable, categ_vars = categ_cols,
                                 cont_vars = c(cont_cols, squared_cols_test, cubed_cols_test))
  
  # Remove Homogenous Categorical Fields
  remove_cols = dtable_remove_homogenous_cols(train_dtable, consider_cols = categ_cols, max_homogeneity = max_homogeneity)
  test_dtable[, c(remove_cols) := NULL]
  categ_cols_filtered = categ_cols[categ_cols %!in% remove_cols]
  
  # Recode Sparse Categorical Levels
  dtable_recode_sparse_levels(dtable = train_dtable, categ_cols = categ_cols_filtered, min_sparsity = min_sparsity)
  dtable_recode_sparse_levels(dtable = test_dtable, categ_cols = categ_cols_filtered, min_sparsity = min_sparsity)
  
  # One-Hot Encode Remaining Categorical Fields
  dummy_encoder = caret::dummyVars(" ~ .", data = train_dtable[, c(categ_cols_filtered), with = FALSE])
  train_dummy_x = data.table(predict(dummy_encoder, newdata = train_dtable[, c(categ_cols_filtered), with = FALSE]))
  test_dummy_x = data.table(predict(dummy_encoder, newdata = test_dtable[, c(categ_cols_filtered), with = FALSE]))
  
  # Create Training Set Output
  train_cols = c(id_col, weight_col, y_col, squared_cols_train, cubed_cols_train)
  train_output = cbind(train_dtable[, train_cols, with = FALSE], train_dummy_x)
  
  # Create Test Set Output
  test_cols = c(id_col, weight_col, squared_cols_test, cubed_cols_test)
  test_output = cbind(test_dtable[, test_cols, with = FALSE], test_dummy_x)
  
  ### Validation Split & Output Formatting
  ######################################################################################################
  # Sample Row Numbers of Training Set
  unique_ids = unique(train_output[[id_col]])
  valid_ids = sample(x = unique_ids, size = round(validation_split * length(unique_ids), 0)) %>% as.vector()
  train_ids = setdiff(unique_ids, valid_ids) %>% as.vector()
  
  # Create xgb.DMatrix Objects for Train, Validation, and Test
  x_vars = colnames(train_output)[colnames(train_output) %!in% c(id_col, weight_col, y_col)]
  train_matrix = xgb.DMatrix(as.matrix(train_output[get(id_col) %in% train_ids, c(x_vars), with = FALSE]),
                             label = train_output[get(id_col) %in% train_ids][['pure_premium']])
  
  valid_matrix = xgb.DMatrix(as.matrix(train_output[get(id_col) %in% valid_ids, c(x_vars), with = FALSE]),
                             label = train_output[get(id_col) %in% valid_ids][['pure_premium']])
  
  test_matrix = xgb.DMatrix(as.matrix(test_output[, c(x_vars), with = FALSE]))
  
  # Return Output List
  return (list(train_matrix, valid_matrix, test_matrix))
  
}
