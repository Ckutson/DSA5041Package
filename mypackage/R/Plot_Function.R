#' Plot Function
#' 
#' Plots Results of Data Using ggplot2
#' @param x A variable
#' @param ... A table variable
#' @return A boxplot
#' @import tidyverse
#' @import ggplot2
#' @examples 
#' plot(obj)
#' @export
plot.rttest <- function(z)
{
  
  x_df <- as.data.frame(x) 
  x_df <- x_df %>% 
    rename(data = x)
  x_df$Category <- "X"
  
  y_df <- as.data.frame(y) 
  y_df <- y_df %>% 
    rename(data = y)
  y_df$Category <- "Y"
  
  plot_data <- rbind(x_df, y_df)
  
  ggplot(data = plot_data) +
    geom_boxplot(mapping = aes(x = plot_data$Category, y = plot_data$data))
}