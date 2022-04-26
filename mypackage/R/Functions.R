myttest <- function(x, y, alpha = 0.05) {
  
  t_test <- t.test(x, y, var.equal = (var(x) == var(y)))
  
  structure(list(df = c(x,y), alpha = 0.05, test = t_test$method,
                 ci = t_test$conf.int, p = t_test$p.value), 
            class = "rttest")
}

print.rttest <- function(z)
{
  
  max_length <- max(c(length(x), length(y))) 
  df <- data.frame(col1 = c(x, rep(NA, max_length - length(x))),
                   col2 = c(y, rep(NA, max_length - length(y))))
  df <- rename(df, x = col1, y = col2)
  
  kable(df)
}

#' Constructor Function
#'
#' Takes in Data and produces a list
#' @param x The x dataset
#' @param y The y dataset
#' @return A list
#' @examples 
#' myttest(x = x, y = y, alpha = 0.05 )
#' @export
myttest <- function(x, y, alpha = 0.05) {
  
  t_test <- t.test(x, y, var.equal = (var(x) == var(y)))

  structure(list(df = c(x,y), alpha = 0.05, test = t_test$method,
                 ci = t_test$conf.int, p = t_test$p.value), 
            class = "rttest")
}

#' Print Function
#'
#' Prints Results of Test Using Kable
#' @param z A variable
#' @return A dataframe
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

