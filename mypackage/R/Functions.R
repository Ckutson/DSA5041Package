myttest <- function(x, y, alpha = 0.05) {
  
  t_test <- t.test(x, y, var.equal = (var(x) == var(y)))
  
  df = list(x=x, y=y)
  attributes(df) = list(names = names(df),
                        row.names=1:max(length(x), length(y)), class='data.frame')
  
  structure(list(df = c(x,y) , alpha = 0.05, test = t_test$method,
                 ci = t_test$conf.int, p = t_test$p.value), 
            class = "rttest")
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
myttest <- function(x, y, alpha) {
  
  t_test <- t.test(x, y, var.equal = (var(x) == var(y)))
  
  df = list(x=x, y=y)
  attributes(df) = list(names = names(df),
                        row.names=1:max(length(x), length(y)), class='data.frame')
  
  structure(list(df = c(x,y) , alpha = 0.05, test = t_test$method,
                 ci = t_test$conf.int, p = t_test$p.value), 
            class = "rttest")
}

#' 