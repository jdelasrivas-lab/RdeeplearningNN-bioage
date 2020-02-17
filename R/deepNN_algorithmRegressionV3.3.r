########################################################################
# Version 3.3:
# Loss Function: Root Mean Squared Deviation
# Activation function: PReLu(Parametric Rectifier Linear Unit)
# Learning Rate: automatically adjusted with Adagrad algorithm (algorithm for gradient-based optimization:
# It adapts the learning rate to the parameters, performing larger updates for infrequent and smaller updates
# for frequent parameters)
# Optimized version 2018-Jun-27 by Óscar González Velasco - CiC Lab 19
# Deep Neural Network With multiple hidden Layers
########################################################################

#' Build the Neural Network structure and initial values.
#'
#' `deepNeuralNetwork.build()` returns a DeepMMModel object which contains an initialized deep neural network model with the parameters specified by the user.
#'
#' This function will create and initialize a deep neural network with a random gaussian distribution
#' the parameters specified by the user will be used to obtain the number of input neurons, number of
#' hidden layers and number of output neurons (which will depend on the number of variables that the model is intended to estimate).
#' The output of this function is intended to be used as the input object of the [deepNeuralNetwork.training] function.
#'
#' @param x Numeric, complex, or logical vectors.
#' @param y Numeric, complex, or logical vectors.
#' @param HidenLayerNeurons A numeric vector representing the hidden layers. Each number specifies the number of neurons of the corresponding hidden layer.
#' @param inputNeurons Numeric.
#' @param outputNeurons Numeric.
#' @param Ai Numeric.
#' @param traindata Matrix.
#' @param drawDNN Boolean. TRUE draw a representation of the network using ggplot. Default=FALSE.
#' @param random.seed Numeric.
#' @param standarization Character or list. Standarization method to be use. If a list of features (corresponding to rows in the input matrix) is
#' supplied, a standarization feature-z-score is done. "r"=robust median z-score. "s"=standar mean z-score.
#' @return A DeepNNModel object with initialized parameters as specified by the user.
#'
#' @examples
#' dnn.model <- deepNeuralNetwork.build(x=c(1,2,4,5),y=3, outputNeurons = 1,
#'                                      HidenLayerNeurons = c(30,10,3),traindata=data,
#'                                      random.seed = 1, drawDNN = 0)
#'
#' \dontrun{
#' sum("a")
#' }
#' @export
deepNeuralNetwork.build <- function(x,
                                    y,
                                    HidenLayerNeurons = c(4, 4),
                                    inputNeurons = 0,
                                    outputNeurons = 0,
                                    Ai = 0.25,
                                    traindata = data,
                                    drawDNN = FALSE,
                                    random.seed = NA,
                                    standarization = "r") {
  message(" Loading DNN model parameters...")
  Sys.sleep(1)
  # to make the case reproducible.
  if ( is.na(random.seed) ) {
    set.seed( runif(1, 1, 100) )
  } else {
    set.seed(random.seed)
    }
  if (!is.numeric(HidenLayerNeurons) ||
      any(HidenLayerNeurons == 0, na.rm = T) ||
      any(is.na(HidenLayerNeurons))) {
    stop("Hiden Layer Neurons specification is not valid.")
    return(NULL)
  }
  # total number of training set
  N <- nrow(traindata)

  # D = Number of Neurons in input layer
  if (inputNeurons <= 0) {
    # if number of input neurons is not set - we determine it by the dimension of the input data matrix
    if (is.list(standarization)) {
      # Calculating the number of variables resulting from the target-gene standarization:
      #     number of input variables = (number of rows of input data * number of target-genes) - number of target genes #-> removing the genes itself as they are 0's
      x <- 1:((length(x) * (length(standarization) + 1)) - length(standarization))
    }
    D <- length(x)
    if (D <= 0)
      stop("Number of input neurons must be > 0")
  } else{
    # special case in which user defines the input number of neurons
    D <- inputNeurons
  }
  ## K = Number of Neurons in output layer or number of categories for classification
  if (outputNeurons == 0) {
    K <- length(unique(traindata[, y]))
  } else{
    K <- outputNeurons
  }
  ## H = Number of Neurons in the hidden layers
  H <-  HidenLayerNeurons
  message("\n", "\tDeep Neural Network Parameters:")
  Sys.sleep(1)
  message(" Input Layer: ", D, " neurons]", appendLF = F)
  Sys.sleep(0.2)
  sapply(H, function(x) {
    message("---[", x, " neurons]", appendLF = F)
    Sys.sleep(0.2)
  })
  message("---[Output Layer: ", K, " neurons.", "\n")
  Sys.sleep(1)

  # create and init weights and bias
  ## On weight Matrices Wi the Nrows corresponds to the number
  ## of Neurons on layer i and the Ncols corresponds to the number
  ## of Neurons on layer i+1 (the layer where the data is flowing to)
  #
  ## Initial values are randomly generated by rnorm: generates random deviates with mean=0.
  ## we will apply a reduction factor 0.01 to obtain small weights
  message(" Initializing DNN weights and bias using a gaussian distribution...", appendLF = F)
  Sys.sleep(0.5)
  # Initialize weights with a gaussian distribution
  Winput <- 0.01 * matrix(rnorm(D * H[1], sd = sqrt(2.0 / D)), nrow = D, ncol = H[1])
  ## On bias matrices bi the Ncols corresponds to the number of Neurons on
  ## layer i+1
  binput <- matrix(0, nrow = 1, ncol = H[1])
  A <- matrix(Ai, nrow = 1, ncol = H[1])
  d <- list(W = Winput, b = binput, A = Ai)
  Wn <- list(d)
  Whl <- list()
  if (length(H) != 1) {
    Whl <- sapply((1:(length(H) - 1)), function(i) {
      neuronLayer <- H[i]
      neuronLayerNext <- H[i + 1]
      Wi <- 0.01 * matrix( rnorm(neuronLayer * neuronLayerNext, sd = sqrt(2.0 / neuronLayer)), nrow = neuronLayer, ncol = neuronLayerNext)
      bi <- matrix(0, nrow = 1, ncol = neuronLayerNext)
      A <- matrix(Ai, nrow = 1, ncol = neuronLayerNext)
      d <- list(W = Wi, b = bi, A = Ai)
      Whl[[i + 1]] <- list(d)
    })
    for (i in (2:(length(Whl) + 1))) {
      Wn[[i]] <- Whl[[i - 1]]
    }
  }
  remove(Whl)

  Woutput <- 0.01 * matrix(rnorm(H[length(H)] * K, sd = sqrt(2.0 / H[length(H)])), nrow = H[length(H)], ncol = K)
  boutput <- matrix(0, nrow = 1, ncol = K)
  A <- matrix(Ai, nrow = 1, ncol = K)
  Wn[[length(Wn) + 1]] <- list(W = Woutput, b = boutput, A = Ai)
  message(" Done.")
  ### Drawing the Neural Network
  if (drawDNN)
    draw.dnn(NperL = c(D, H, K))
  message(" Returning DNN model.")
  # # Creating the DeepNNModel object: this object will store all information, structure and data of the Neural Network
  # DeepNNModel <-
  #   setClass(Class = "DeepNNModel",
  #            slots = list(
  #              dnn = "list",
  #              error = "numeric",
  #              bestDnn = "list",
  #              bestError = "numeric",
  #              bestDnn2 = "list",
  #              bestError2 = "numeric",
  #              dnn.structure = "vector",
  #              training = "list"
  #            )
  #   )
  # setMethod("show",
  #           "DeepNNModel",
  #           function(object) {
  #             cat("Object of class: ", class(object), "\n")
  #             cat("\t - stores a deep neural network model and its metrics.")
  #           })
  DeepNNModel <- DeepNNModel(dnn = Wn, dnn.structure = c(D, H, K))
  return(DeepNNModel)
}

#
# ###########################################################################################
# #       END OF SCRIPT
# #########################################################################################
#
###
#
# ############################################
