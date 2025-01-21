#' Function to unlist columns
#'
#' @param df dataframe
#' @return 
#' @examples
#' 

#' 
unlist_columns <- function(df) {
  for (col_name in names(df)) {
    if (is.list(df[[col_name]])) {
      df[[col_name]] <- unlist(df[[col_name]])
    }
  }
  return(df)
}