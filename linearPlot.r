####################################################################
#' Linear Regression Results Plot
#' 
#' This function plots a Linear Regression Result
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param subtitle Character. Subitle to show in plot
#' @param model_name Character. Model's name
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to save the plot
#' @param file_name Character. File name as you wish to save the plot
#' @export
mplot_lineal <- function(tag, 
                         score,
                         title = "Regression Model Results",
                         subtitle = NA, 
                         model_name = NA, 
                         save = FALSE, 
                         subdir = NA,
                         deviation = FALSE,
                         transparent = FALSE,
                         file_name = "viz_lineal.png") {
  
  require(ggplot2)
  require(scales)
  dist2d <- function(a, b = c(0, 0), c = c(1, 1)) {
    v1 <- b - c
    v2 <- a - b
    m <- cbind(v1, v2)
    d <- abs(det(m)) / sqrt(sum(v1 * v1))
  }
  rmse <- function(tag, score){
    error <- tag - score
    sqrt(mean(error^2))
  }
  mae <- function(tag, score){
    error <- tag - score
    mean(abs(error))
  }
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
  }
  
  results <- data.frame(tag = tag, score = score, dist = 0)
  for (i in 1:nrow(results)) { 
    results$dist[i] <- abs(results$tag[i] - results$score[i])
  }
  
  fit <- lm(score ~ tag)
  anova(fit)
  coefficients(fit)
  if(nrow(summary(fit)$coef) == 1){pval <- NA}else if(is.na(summary(fit)$coef[2,4])){pval <- NA}else{ pval <- signif(summary(fit)$coef[2,4],3)}
  labels <- paste(
    paste("R² (adj.) = ", signif(summary(fit)$adj.r.squared, 4)),
    paste("p.value =", pval), 
    paste("RMSE =", signif(rmse(results$tag, results$score), 4)), 
    sep="\n")
  
  p <- ggplot(results, aes(x = tag, y = score)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    theme_minimal() + coord_equal(ratio = 1,xlim = c(0,100),ylim = c(0,100)) + 
    labs(title = title,
         x = "Chronological age (Years)", y = "Transcriptomic age") +
    geom_label(aes(x = 100, y = 0, hjust = 1, vjust = 0), label = labels, size = 8, label.padding = unit(0.50, "lines"), label.size = 0) +
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