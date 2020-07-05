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


