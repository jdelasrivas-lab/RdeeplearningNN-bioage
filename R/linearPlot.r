####################################################################
#' Linear Regression Results Plot
#'
#' This function plots a Linear Regression Result
#'
#' @import ggplot2
#' @import scales
#' @param observed Vector. Observed values: real data values label.
#' @param predicted Vector. Predicted value or model's result.
#' @param title Character. Title to be shown in the plot.
#' @param subtitle Character. Subtitle to be shown in the plot.
#' @param model_name Character. Model's name
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to save the plot
#' @param deviation Boolean. Plot the deviation?
#' @param transparent Numeric. Transparency parameter.
#' @param x.lim Vector. The bottom and upper limit of the values on X axis.
#' @param y.lim Vector. The bottom and upper limit of the values on Y axis.
#' @param x.lab Character. The label to be printed on the X axis plot.
#' @param y.lab Character. The label to be printed on the Y axis plot.
#' @param file_name Character. File name as you wish to save the plot
#' @export
mplot_lineal <- function(observed,
                         predicted,
                         title = "Regression Model Results",
                         subtitle = NA,
                         model_name = NA,
                         save = FALSE,
                         subdir = NA,
                         deviation = FALSE,
                         transparent = FALSE,
                         x.lim = c(0,100),
                         y.lim = c(0,100),
                         x.lab = "Observed values (Y)",
                         y.lab = "Predicted values (Y')",
                         file_name = "viz_lineal.png") {

  require(ggplot2)
  require(scales)
  max.val <- max(c(c(observed),c(predicted)))
  min.val <- min(c(c(observed),c(predicted)))
  x.lim <- c(min.val,max.val)
  y.lim <- c(min.val,max.val)
  dist2d <- function(a, b = c(0, 0), c = c(1, 1)) {
    v1 <- b - c
    v2 <- a - b
    m <- cbind(v1, v2)
    d <- abs(det(m)) / sqrt(sum(v1 * v1))
  }
  rmse <- function(observed, predicted){
    error <- observed - predicted
    sqrt(mean(error^2))
  }
  mae <- function(observed, predicted){
    error <- observed - predicted
    mean(abs(error))
  }
  if (length(observed) != length(predicted)) {
    message("The observed and predicted vectors should be the same length.")
    stop(message(paste("Currently, observed has",length(observed),"rows and predicted has",length(predicted))))
  }

  results <- data.frame(observed = observed, predicted = predicted, dist = 0)
  for (i in 1:nrow(results)) {
    results$dist[i] <- abs(results$observed[i] - results$predicted[i])
  }

  fit <- lm(predicted ~ observed)
  anova(fit)
  coefficients(fit)
  if(nrow(summary(fit)$coef) == 1){pval <- NA}else if(is.na(summary(fit)$coef[2,4])){pval <- NA}else{ pval <- signif(summary(fit)$coef[2,4],3)}
  labels <- paste(
    paste("R² (adj.) = ", signif(summary(fit)$adj.r.squared, 4)),
    paste("p.value =", pval),
    paste("RMSE =", signif(rmse(results$observed, results$predicted), 4)),
    sep="\n")

  p <- ggplot(results, aes(x = observed, y = predicted)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    theme_minimal() + coord_equal(ratio = 1,xlim = x.lim,ylim = y.lim) +
    labs(title = title,
         x = x.lab, y = y.lab) +
    geom_label(aes(x = x.lim[2], y = y.lim[1], hjust = 1, vjust = 0), label = labels, size = 8, label.padding = unit(0.50, "lines"), label.size = 0) +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = comma) +
    theme(legend.justification = c(0, 1), legend.position = c(0, 1),
          axis.text = element_text(size = 22), axis.title=element_text(size=20),
          plot.title = element_text(size=24,face="bold",hjust = 0.5)) +
    guides(colour = guide_colorbar(barwidth = 0.9, barheight = 4.5))

  if(deviation){
    p <- p + ggplot(colour = dist) + labs(colour = "Deviation")
  }
  if(!is.na(subtitle)) {
    p <- p + labs(subtitle = subtitle)
  }
  if(transparent){
    p <- p + geom_point(alpha = 0.5)
  }
  if(!is.na(model_name)) {
    p <- p + labs(caption = model_name)
  }
  if (save == TRUE) {
    p <- p + ggsave(file_name, width = 6, height = 6)
  }

  return(p)
}
