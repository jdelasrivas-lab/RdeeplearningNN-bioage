#' Saves a DeepNNModel object to a file.
#'
#' `deepNeuralNetwork.save()` Saves a DeepNNModel object to a .RData file.
#'
#' `deepNeuralNetwork.save()` Saves a DeepNNModel object to a .RData file.
#'
#' @param wn Object of class DeepNNModel.
#' @param nameoffile Character. The name of the file.
#' @return If success the name of the file.
#' @export
deepNeuralNetwork.save <- function(wn = NULL,
                                   nameoffile = paste("DNN.model.",format(Sys.time(),"%d.%m.%Y.%H.%M.%S"),sep = "")){

  assign(nameoffile,wn)
  save(nameoffile,file = paste(nameoffile,".RData",sep = ""))

  return(nameoffile)
}
