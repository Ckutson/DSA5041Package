#' Print Function
#'
#' Prints Results of Test Using Kable
#' @param x A variable
#' @param ... A table variable
#' @return A dataframe
#' @import kableExtra
#' @examples 
#' print(obj)
#' @export
print.rttest <- function(z)
{
  
  max_length <- max(c(length(x), length(y))) 
  df <- data.frame(col1 = c(x, rep(NA, max_length - length(x))),
                   col2 = c(y, rep(NA, max_length - length(y))))
  df <- rename(df, x = col1, y = col2)
  
  kable(df)
}