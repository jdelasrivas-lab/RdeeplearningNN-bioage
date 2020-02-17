#' Calculate a prediction from a trained DeepNNModel.
#'
#' `deepNeuralNetwork.predict()` returns a prediction over the given data.
#'
#' This function will calculate a prediction using the supplied data. It performs the selected data standarization
#' (beware that the input data must be standarized using the same method as in training) and returns the predicted values.
#'
#' @param model.trained An object of class DeepNNModel containing a trained model.
#' @param data Matrix containing the test data without the observed variable that we want to predict.
#' @param standarization Character or list. Standarization method to be used. If a list of features (corresponding to rows in the input matrix) is
#' supplied, a standarization feature-z-score is done. "r"=robust median z-score. "s"=standar mean z-score.
#' @return A vector or matrix containing the predicted values.
#' @export
deepNeuralNetwork.predict <- function(
  model.trained,
  data = X.test,
  standarization = NULL) {
  # Prediction over unseen data
  # - Same standarization as in training must be done
  # Activation Function: PReLu (Parametric Rectifier Linear unit)
  #       - PReLu avoids 0 gradient minimum by applying Ai*y when y<0
  if( !class(model.trained) == "DeepNNModel" ){
    error("Model is not of type 'DeepNNModel'")
  }

  message(" Standarizing Data...", appendLF = F)
  if (is.list(standarization)) {
    new.data <- deepNeuralNetwork.standarizegenescore(x = data, gene.list = standarization)
    new.data <- t(new.data)
  } else{
    new.data <- deepNeuralNetwork.standarize(data.matrix(data))
    new.data <- t(new.data)
  }
  Sys.sleep(time = 1)
  message(" Done.")
  message(" Loading model...", appendLF = F)
  Wn <- model.trained@bestDnn
  Sys.sleep(time = 0.5)
  message(" Done.")
  N <- length(Wn)
  message("\n\t Deep Neural Network Topology:")
  Sys.sleep(time = 0.2)
  message("> Number of layers: ", N)
  sapply(1:N, function(x) {
    message("[Layer ", x, " Neurons:", nrow(Wn[[x]]$W), "]--", appendLF = F)
    Sys.sleep(time = 0.2)
  })
  message("[Output layer Neurons:", ncol(Wn[[N]]$b), "]")
  Sys.sleep(time = 1)
  message("Running model...")
  Hx <- list() # Hx[[l]]:Output of layer l
  # Feed Forwad with the DataSet through the NN
  for (i in 1:length(Wn)) {
    if (i == 1) {
      Hx[[i]] <- sweep(new.data %*% Wn[[i]]$W , 2, Wn[[i]]$b, '+')
      # Activation Function: PReLu Parametric Rectifier Linear
      Hx[[i]] <- pmax(Hx[[i]], Wn[[i]]$A * Hx[[i]])
    } else if (i == length(Wn)) {
      y_output <- sweep(Hx[[i - 1]] %*% Wn[[i]]$W, 2, Wn[[i]]$b, '+')
    } else{
      Hx[[i]] <- sweep(Hx[[i - 1]] %*% Wn[[i]]$W, 2, Wn[[i]]$b, '+')
      # Activation Function: PReLu Parametric Rectifier Linear
      Hx[[i]] <- pmax(Hx[[i]], Wn[[i]]$A * Hx[[i]])
    }
  }

  ## The Output of the DNN
  message("\n> Prediction Completed.")
  return(y_output)
}
