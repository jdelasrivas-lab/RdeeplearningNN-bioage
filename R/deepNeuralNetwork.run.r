#########################################
## Partial Prediction for in-training use
## - A partial prediction over the test data is calculated
##   to measure overfitting.

#' Partial Prediction for in-training use.
#'
#' `deepNeuralNetwork.run()` returns a partial prediction over the test data to measure overfitting if test data is supplied by the user.
#' It is calculated using the actual in-training-model.
#'
#' This function will calculate a prediction using test data (if it is supplied by the user). [deepNeuralNetwork.training] function will use this function
#' and depending on the result given it can decide whether to continue with the training or stop it.
#'
#' @param model.trained An object of class DeepNNModel.
#' @param data Matrix containing the test data without the observed variable that we want to predict.
#' @return A vector or matrix containing the prediction values.
#' @export
deepNeuralNetwork.run <- function(model.trained, data = X.test) {
  # Activation Function: PReLu (Parametric Rectifier Linear unit)
  #       - PReLu avoids 0 gradient minimum by applying Ai*y when y<0
  Wn <- model.trained
  Hx <- list() # Hx[[l]]:Output of layer l
  # Feed Forwad with the DataSet through the NN
  for (i in 1:length(Wn)) {
    if (i == 1) {
      Hx[[i]] <- sweep(data %*% Wn[[i]]$W , 2, Wn[[i]]$b, '+')
      # Activation Function: PReLu Parametric Rectifier Linear
      Hx[[i]] <- pmax(Hx[[i]], Wn[[i]]$A * Hx[[i]])
      # Hx[[i]] <- pmax(Hx[[i]], 0.1) # ReLU Function implementation with minimun constant 0.1
    } else if (i == length(Wn)) {
      y_output <- sweep(Hx[[i - 1]] %*% Wn[[i]]$W, 2, Wn[[i]]$b, '+')
    } else{
      Hx[[i]] <- sweep(Hx[[i - 1]] %*% Wn[[i]]$W, 2, Wn[[i]]$b, '+')
      # Activation Function: PReLu Parametric Rectifier Linear
      Hx[[i]] <- pmax(Hx[[i]], Wn[[i]]$A * Hx[[i]])
      # Hx[[i]] <- pmax(Hx[[i]], 0.1) # ReLU Function implementation with minimun constant 0.1
    }
  }
  ## The Output of the DNN
  return(y_output)
}
