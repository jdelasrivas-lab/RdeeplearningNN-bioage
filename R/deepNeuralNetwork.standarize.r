### Standarizing gene signal

#' Standarizes the input data.
#'
#' `deepNeuralNetwork.standarize()` standarizes the given data by calculating either a z-score using the mean and the sd or
#' a robust z-score using the median and the MAD.
#'
#' `deepNeuralNetwork.standarize()` standarizes the given data by calculating either a z-score using the mean and the sd or
#' a robust z-score using the median and the MAD.
#'
#' @param x Matrix. The data to be standarized.
#' @param method Character. Specifies the method used. "s"=standar z-score using the mean. "r"=robust z-score using the median.
#' @return The standarized data matrix.
#' @export
deepNeuralNetwork.standarize <- function(x = NULL, method = "r"){
  if(is.null(x))stop("Standarizing data: ERROR, data is NULL.")
  if(!is.numeric(x))stop("Standarizing data: ERROR, trying to standarize non-numeric data.")
  if(is.null(dim(x)))x<-as.data.frame(x)
  if(!method %in% c("s","r")){
    message("Invalid Method \"",method,"\": \n - \"s\" for standar Z-score using the mean and SD \n - \"r\" for robust Z-score using the median and MAD \n - \"gene\" for specific-gene Z-score using the gene as centroid - |n \"genecomb\" for specific-gene Z-score using the gene as centroid and  sample-gene combined standar deviation")
    warning("choose method = \"-\"",immediate. = T)
  }
  ## standard Z-Score using mean and SD:
  if(method == "s"){
    message("Standarizing data [by row] using *mean standar deviation Z-score...",appendLF = F)
    MX.standarized <- t(apply(X = x,MARGIN = 1,FUN = function(y){
      (y - mean(y))/sd(y)}))
    message(" Completed.\n")
    return(MX.standarized)
  }
  #
  if(method == "r"){
    message("Standarizing data [by column] using *median absolute deviation(MAD) Z-score...",appendLF = F)
    rowmed <- apply(x, 2, median)
    rowmad <- apply(x, 2, mad)  # median absolute deviation
    rv <- (x - rowmed) / rowmad
    message(" Completed.")
    return(rv)
  }
}
