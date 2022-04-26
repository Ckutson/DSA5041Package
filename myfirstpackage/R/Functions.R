myttest <- function(x, y, alpha = 0.05) {

  ## Creating T Test that takes X and Y Vectors and assumes variances are equal
  t_test <- t.test(x, y, var.equal = (var(x) == var(y)))

  df = list(x=x, y=y)
  attributes(df) = list(names = names(df),
                        row.names=1:max(length(x), length(y)), class='data.frame')

  ## Creating list with data frame, alpha, test type, confidence interval and p value
  ## with class set to Rttest
  structure(list(df = c(x,y) , alpha = 0.05, test = t_test$method,
                 ci = t_test$conf.int, p = t_test$p.value),
            class = "rttest")

}

print.rttest <- function(z)
{
  kableExtra::kable(z$df)
}
