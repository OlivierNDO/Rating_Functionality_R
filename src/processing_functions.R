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