#' Constructor Function
#'
#' Takes in Data and produces a list
#' @param x The x dataset
#' @param y The y dataset
#' @param alpha an alpha value
#' @return A list
#' @examples 
#' myttest(x, y, alpha = 0.05)
#' @export
myttest <- function(x, y, alpha = 0.05) {
  
  t_test <- t.test(x, y, var.equal = (var(x) == var(y)))
  
  structure(list(df = c(x,y), alpha = 0.05, test = t_test$method,
                 ci = t_test$conf.int, p = t_test$p.value), 
            class = "rttest")
}
