# ## WRAPPER FOR GENE-TARGET STANDARIZATION
### Standarizing gene signal

#' Standarizes the input data using a feature as a target.
#'
#' `deepNeuralNetwork.standarizegenescore()` standarizes the given data by calculating a personalized z-score using the feature as centroid and dividing by the MAD.
#'
#' `deepNeuralNetwork.standarizegenescore()` standarizes the given data by calculating a personalized z-score using the feature as centroid and dividing by the MAD.
#'
#' @import plyr
#' @param x Matrix. The data to be standarized.
#' @param method Character. Only gene is acepted.
#' @param gene.list Character vector. List of features to use if method is "genecomb".
#' @return The standarized data matrix.
#' @export
deepNeuralNetwork.standarizegenescore <- function(x = NULL, method = "gene",gene.list = NA){

  library("plyr")
  if(!method %in% c("s","r","gene","genecomb")){
    message("Invalid Method \"",method,"\": \n - \"s\" for standar Z-score using the mean and SD \n - \"r\" for robust Z-score using the median and MAD \n - \"gene\" for specific-gene Z-score using the gene as centroid - |n \"genecomb\" for specific-gene Z-score using the gene as centroid and  sample-gene combined standar deviation")
    warning("choose method = \"-\"",immediate. = T)
  }

  if(is.null(x))stop("Standarizing data: ERROR, data is NULL.")
  if(!is.numeric(x))stop("Standarizing data: ERROR, trying to standarize non-numeric data.")
  if(is.null(dim(x)))x <- as.data.frame(x)

  standarization = gene.list
  new.data <- as.matrix(plyr::ldply(lapply(X = standarization,function(gene){

    if(is.na(gene))stop("Gene-target for standarization not specified.")
    message("Standarizing data using *specific-gene Z-score with MAD...\n",appendLF = F)
    MX.standarized <- apply(X = x,MARGIN = 2,FUN = function(y){
      sample.MAD <- mad(y)
      target.gene.exprs <- y[match(gene,names(y))]
      y <- y[-match(gene,names(y))]
      return((y - target.gene.exprs)/sample.MAD)})
    return(MX.standarized)
  }), data.frame))

  message("Standarizing data [by column] using *median absolute deviation(MAD) Z-score...\n",appendLF = F)
  rv <- apply(x,2,function(z){(z - median(z)) / mad(z)})
  new.data <- as.matrix(rbind(rv,new.data))
  message(" Completed.")
  return(new.data)
}
#
