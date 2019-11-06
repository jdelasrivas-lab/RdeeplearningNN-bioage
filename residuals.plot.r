residuals.plot <- function(observed = NULL, predicted = NULL, title = "Residuals plot"){
  library(ggplot2)
  if(!is.vector(observed)){
    if(ncol(observed)==1)observed <- c(observed)
    else stop("observed values need to be a vector or a df with a single column")
  }
  if(!is.vector(predicted)){
    if(ncol(predicted)==1)predicted <- c(predicted)
    else stop("predicted values need to be a vector or a df with a single column")
  }
  if(length(observed)!=length(predicted))stop("The lengths of the vectors does not match.")
  residuals <- observed - predicted
  r.test <- runs.test(factor(residuals>0), alternative = c("two.sided"))
  labels <- paste(
    paste("runs test for randomness of residuals:"),
    paste("- p.value =", round(r.test$p.value,digits = 2)), 
    paste("- alternative hypothesis ",r.test$alternative), 
    sep="\n")
  df <- as.data.frame(cbind(residuals = residuals,observed = observed, col = abs(residuals)))
  
  ggplot(df, aes(x = observed, y = residuals,color=col)) +
    geom_point(size = 2, shape = 15) +
    geom_hline(yintercept = 0) +
    geom_label(aes(x = Inf, y = max(residuals), hjust = 2, vjust = 0), label = labels, size = 6, label.padding = unit(0.50, "lines"), label.size = 0) +
    labs(title = title,
       x = "observed values", y = "model residuals") +
    theme(legend.justification = c(0, 1), legend.position = c(0, 1),
          axis.text = element_text(size = 22), axis.title=element_text(size=20),
          plot.title = element_text(size=24,face="bold",hjust = 0.5))
}
