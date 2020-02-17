##################################################
# Training the Deep Neural Network built by build.dnn function, a dataset must be
#    passed in order to train the DNN, x= columns of the data , y = columns of known results that can be predicted using x

#' Train the Deep Neural Network to obtain a regression model.
#'
#' `deepNeuralNetwork.training()` will train the Deep Neural Network built by [build.dnn] function. A dataset must be
#' passed in order to train the DNN, x= columns of the data , y = columns of known results that can be predicted using x
#'
#' This function trains a deep neural network previously created and initialized by [deepNeuralNetwork.build].
#'
#' @param x Numeric Vector. Specifies the columns of the data that will be used as the input variables to predict y value.
#' @param y NUmeric, or Vector. Specifies the column (or columns) of the observed variable, the output that can be predicted using x.
#' @param model An object of class DeepNNModel containing an initialized model.
#' @param traindata Matrix or Data Frame. The actual data with explanatory variables (x) and observed results (y) that will be used for training.
#' @param testdata Matrix or Data Frame (optional). If provided, the algorithm will check for overfitting using the testdata as a input.
#' @param iterations Numeric. Number of training iterations.
#' @param minError Numeric.
#' @param maxError Numeric. Maximun Error permited on training data to chose best model.
#' @param lr Numeric. Initial Learning rate. During training it is automatically adjusted using adagrad.
#' @param reg Numeric. Regularization rate.
#' @param display Numeric. Show training results each [N] iterations.
#' @param random.seed Numeric. deprecated.
#' @param standarization Character or list. Standarization method to be use. If a list of features (corresponding to rows in the input matrix) is
#' supplied, a standarization feature-z-score is done. "r"=robust median z-score. "s"=standar mean z-score.
#' @param savePlotIteration Boolean. If TRUE saves the plot that is shown every [N] iterations specified by [display].
#'
#' @return A DeepNNModel object with the trained regression model with the parameters as specified by the user.
#'
#' @examples
#' dnn.model <- deepNeuralNetwork.build(x=c(1,2,4,5),y=3, outputNeurons = 1,
#'                                      HidenLayerNeurons = c(30,10,3),traindata=data,
#'                                      random.seed = 1, drawDNN = 0)
#' @export
deepNeuralNetwork.training <- function(x,y, model = NULL,
                                       traindata=data,
                                       testdata=NULL,
                                       # Max number of iteration steps
                                       iterations=2000,
                                       # Delta loss stop
                                       minError=1e-2,
                                       # Maximun Error permited on training data to chose best model
                                       maxError=0.05,
                                       # Starting learning rate
                                       lr = 1e-2,
                                       # regularization rate
                                       reg = 1e-3,
                                       # show results every 'display' step
                                       display = 100,
                                       # Set seed for reproducible test
                                       random.seed = 1,
                                       standarization = NULL,
                                       # savePlotIteration = save the plot of each iteration as a .png image?
                                       savePlotIteration = FALSE)
{
  # traindata <- data
  # total number of training set
  if(!class(model) == "DeepNNModel"){error("The Deep NN Model is not of class: \"DeepNNModel\"")}
  else{model@training <- list(iterations = iterations,learning.rate = lr, regularization.rate = reg)}
  N <- ncol(traindata)
  # This X Data will be the explanatory data
  X.raw.t <- t(traindata)
  X.raw.t <- data.matrix(X.raw.t[,x])
  if(is.list(standarization)){
    data = traindata[x,]
    X <- deepNeuralNetwork.standarizegenescore(x = data,gene.list = standarization)
    X <- t(X)
  }else{
    X <- deepNeuralNetwork.standarize(traindata[x,])
    X <- t(X)
  }
  print(dim(X))
  # correct categories represented by integer
  # This Y data will be the data to be predicted: Y = F(X) + a
  Y <- traindata[y,]

  # Test data for overfitting purposes
  if(is.factor(Y)) { Y <- as.integer(Y) }
  if(!is.null(testdata) && is.factor(testdata[y,])){
    Yprima <- as.integer(testdata[y,])
    batchsize.test <- ncol(testdata)
    SST2 <- sum((Yprima - mean(Yprima))^2)
    SSE2 <- 0.4
    bestEtest <- 0.5
  } else {
    batchsize.test <- ncol(testdata)
    Yprima <- testdata[y,]
    SST2 <- sum((Yprima - mean(Yprima))^2)
    SSE2 <- 0.4
    bestEtest <- 0.5
  }
  X.prima.raw <- t(testdata)
  X.prima.t <- X.prima.raw[,x]
  if(is.list(standarization)){
    data = testdata[x,]
    X.prima <- deepNeuralNetwork.standarizegenescore(x = data,gene.list = standarization)
    X.prima <- t(X.prima)
  }else{
    X.prima <- deepNeuralNetwork.standarize(testdata[x,])
    X.prima <- t(X.prima)
  }

  # create index for both row and col
  Y.len   <- length(Y)
  # len is the number of different values that Y can take
  Y.set   <- sort(Y)
  Y.index <- cbind(1:N, match(Y, Y.set))
  # The model list with the Weights and Biases
  Wn <- model@dnn
  # use all train data to update weights since it's a small dataset
  batchsize <- N
  # Loss error and pastLoss initialization value
  loss <- 50000
  pastLoss <- 50001
  bestModel <- NULL
  bestModel2 <- NULL
  bestLoss <- 1e6
  bestLoss2 <- 1
  predictionLoss <- 50000
  partialPredict <- 50000
  # Y value dispersion index SST
  SST <- sum((Y - mean(Y))^2)
  ## epsilon: Adagrad value to prevent division by 0
  epsilon <- 1e-8
  # Training the network
  i <- 0
  Hx_function <- list()
  N <- length(Wn)
  X11(width = 10,height = 10)
  while(i < iterations && loss > minError ) {
    # iteration index
    i <- i +1
    ## Calculation of the very first layer (INPUTDATA x WEIGHTMatrix + BIAS)
    Hx_function[[1]] <- sweep(X %*% Wn[[1]]$W ,2, Wn[[1]]$b , '+')

    # PReLu Function
    Hx_function[[1]] <- pmax(Hx_function[[1]], Wn[[1]]$A * Hx_function[[1]])
    #Hx_function[[1]] <- pmax(Hx_function[[1]], 0.1) ## RElu function
    for(j in 2:(N-1)){
      # forward ....
      # 1 indicate row, 2 indicate col
      if(N==2)break()
      ## Special case contemplating the NN of only 3 layers (minimun Nº of layers)
      ## on a 3-layer NN there will be only 2 weight matrices length(Wn)==2
      # neurons : ReLU
      # pmax compares parallely each row with a row of 0's
      # in this case, if a value is <0 it will be set to 0
      # this is a fast way to compute the ReLU function (f(x)= max(0,x))
      Hx_function[[j]] <- sweep(Hx_function[[j-1]] %*% Wn[[j]]$W ,2, Wn[[j]]$b , '+')
      # PReLu Function
      Hx_function[[j]] <- pmax(Hx_function[[j]], Wn[[j]]$A * Hx_function[[j]])
      #Hx_function[[j]] <- pmax(Hx_function[[j]], 0.1)  ## RElu function
    }
    y_DNNoutput <- sweep(Hx_function[[N-1]] %*% Wn[[N]]$W, 2, Wn[[N]]$b, '+')

    # LOSS FUNCTION:
    # Root Mean Square Error function to map y_DNNoutputs to a value of a variable
    # Minimize the output error
    probs <- (y_DNNoutput - Y) #/ batchsize # y output predicted data minus Y observed data
    data.loss <- sqrt(mean((probs)^2))
    SSE <- sqrt(mean(sum((probs)^2)))
    #SSE <- sum(probs^2) # Summatory before the division by mean ???????**************************************
    Epredicted <- SSE/SST
    reg.loss   <- reg* (sum(sapply(1:N,function(x){sum(Wn[[x]]$W*Wn[[x]]$W)})) / batchsize )
    probs <- probs+reg.loss
    loss <- sum(data.loss,reg.loss,na.rm = T)
    if(loss > 1000){
      cat("\n WARNING:")
      cat("\n","Error loss value has reached:",loss,"\n")
      cat("Check parameters: \n")
      cat("Learning Rate(lr) - actual=",lr,"\n")
      cat("Regularization Rate(reg) - actual=",reg,"\n")
      cat("Returning the LAST Best Model and Best Error \n")
      # Returning the trained model (Wn)
      model@dnn <- Wn
      model@error <- loss
      model@bestDnn <- bestModel
      model@bestError <- bestLoss
      if(is.null(bestModel2)){model@bestDnn2 <- as.list(bestModel2)}
      else{model@bestDnn2 <- bestModel2}
      model@bestError2 <- bestLoss2
      rm(Wn,bestModel,dhidden,Hx_function,X,Y)
      gc()
      return(model)
    }
    if(loss < bestLoss){
      bestLoss <- loss
      bestModel <- Wn
    }
    pastLoss <- loss
    ########################
    ## Print partial results
    if(i == 1){
      if(!is.null(testdata)){
        print(dim(X.prima))
        partialResult <- deepNeuralNetwork.run(model.trained = Wn,
                                               data = X.prima)
        partialPredict <- (sqrt(mean((partialResult - Yprima)^2)))/ batchsize.test
        e2 <- partialResult - Yprima
        SSE2 <- sum(e2^2)
        Etest <- SSE2/SST2
        if(partialPredict < predictionLoss){ predictionLoss <- partialPredict }
      }

      cat("\r","Iteration Number:",i," Actual Loss:", loss, " Squared:",SSE,"\n")
      cat("TLoss Ratio: ",(partialPredict - loss)," Prediction Loss:", predictionLoss, " Partial predict:",partialPredict ,"\n")
      cat("Error 1:",Epredicted," Error 2:",Etest,"\n")
      cat("\n Log2(E1/E2):",log2(Epredicted/Etest),"\n")
      cat("##################################################### \n")
    }
    if( i %% display == 0){
      #######################################
      ## Check for Overfitting and early stop
      if(!is.null(testdata)){
        partialResult <- deepNeuralNetwork.run(model.trained = Wn,
                                               data = X.prima)
        partialPredict <- (sqrt(mean((partialResult - Yprima)^2)))/ batchsize.test
        e2 <- partialResult - Yprima
        SSE2 <- sum(e2^2)
        Etest <- SSE2/SST2
        if(partialPredict < predictionLoss){ predictionLoss <- partialPredict }
      }
      if((Epredicted < maxError) & (Etest < bestEtest)){
        bestEtest <- Etest
        bestLoss2 <- Etest
        bestModel2 <- Wn
      }
      cat("\r","Iteration Number:",i," Actual Loss:", loss, " Squared:",SSE,"\n")
      cat("TLoss Ratio: ",(partialPredict - loss)," Prediction Loss:", predictionLoss, " Partial predict:",partialPredict ,"\n")
      cat("Error 1:",Epredicted," Error 2:",Etest,"\n")
      cat("\n Log2(E1/E2):",log2(Epredicted/Etest),"\n")
      cat("##################################################### \n")
      title <- paste("Deep Neural Network Training - Iteration Number:",i,"[",round((i/iterations)*100,digits = 1),"%]")
      subtitle <- "Regresion Model parcial prediction."
      if(savePlotIteration){
        # Should the plot image for each epoque-iteration be saved? TRUE -> png image with the iteration number as name
        print(mplot_lineal(tag = Y,score = y_DNNoutput,title = title,subtitle = subtitle,save = T,file_name = paste("regression.model.iteration.",as.character(i),".png",sep = ""),subdir = "/home/oscar/Escritorio/DeepNeuralNetworks4R/images/"))
      }
      else{
        print(mplot_lineal(tag = Y,score = y_DNNoutput,title = title,subtitle = subtitle))
      }
    }else{
      cat("\r","Iteration Number:",i," Actual Loss:", loss)
    }
    ############################
    # Backpropagation Algorithm: updating of Weights and Bias
    # We start by updating and calculating the values for the output layer -> j layer -> inputLayer

    # We apply the derivative function of the Root Mean Square Error formula:
    delta_y <- 2*(probs)
    # Output Layer (Layer N-1)
    derivativeWeights <- crossprod(Hx_function[[N-1]],delta_y)
    derivativeBias <- colSums(delta_y)
    # Wn[[N]] Here corresponds with layer l-1 weights
    dhidden <- tcrossprod(delta_y,Wn[[N]]$W)

    # Applying the derivative function of the ReLu formula
    dPrelu <- pmin(dhidden,0)
    dhidden[Hx_function[[N-1]] <= 0] <- Wn[[N-1]]$A * dhidden[Hx_function[[N-1]] <= 0]

    adagradW <- (lr / sqrt(mean(derivativeWeights^2)+epsilon))
    adagradB <- (lr / sqrt(mean(derivativeBias^2)+epsilon))
    Wn[[N]]$W <- Wn[[N]]$W - adagradW * derivativeWeights
    Wn[[N]]$b <- Wn[[N]]$b - adagradB * derivativeBias
    ## NO Wn$A on N Layer as there is no activation function on
    #     the last layer, only loss function

    for(j in (N-1):2){
      if(N==2)break()
      ## Special case contemplating the NN of only 3 layers (minimun Nº of layers)
      ## on a 3-layer NN there will be only 2 waight matrices length(Wn)==2
      ## The i intermediate Layers Backpropagation:
      derivativeWeights <- crossprod(Hx_function[[j-1]],dhidden)
      derivativeBias <- colSums(dhidden)
      derivativePrelu <- sum(colSums(dPrelu))
      adagradW <- lr / sqrt(mean(derivativeWeights^2)+epsilon)#%*%derivativeWeights
      adagradB <- lr / sqrt(mean(derivativeBias^2)+epsilon)
      adagradA <- (lr / sqrt(mean(colSums(dPrelu)^2)+epsilon))

      #Calculate Delta for the next gradient computation:
      dhidden <- tcrossprod(dhidden,Wn[[j]]$W)
      dPrelu <- pmin(dhidden,0)
      dhidden[Hx_function[[j-1]] <= 0] <- Wn[[j-1]]$A * dhidden[Hx_function[[j-1]] <= 0]

      # Updating of Weights, Bias and Alfa-PReLu value:
      Wn[[j]]$W <- Wn[[j]]$W - adagradW * derivativeWeights
      Wn[[j]]$b <- Wn[[j]]$b - adagradB * derivativeBias
      Wn[[j]]$A <- 0.9 * Wn[[j]]$A + adagradA * derivativePrelu # 0.9:momentum of PReLu Ai parameter
    }
    ## Backpropagation for the Input Layer:
    derivativeWeights <- crossprod(X,dhidden)
    derivativeBias <- colSums(dhidden)
    derivativePrelu <- sum(colSums(dPrelu))
    # Updating parameters of the Input layer
    adagradW <- (lr / sqrt(mean(derivativeWeights^2)+epsilon))#%*%derivativeWeights
    adagradB <- (lr / sqrt(mean(derivativeBias^2)+epsilon))
    adagradA <- (lr / sqrt(mean(colSums(dPrelu)^2)+epsilon))
    Wn[[1]]$W <- Wn[[1]]$W - adagradW * derivativeWeights
    Wn[[1]]$b <- Wn[[1]]$b - adagradB * derivativeBias
    Wn[[1]]$A <- 0.9 * Wn[[1]]$A + adagradA * derivativePrelu # 0.9:momentum of PReLu Ai parameter
  }
  #############################################
  # Final results, statistics and memory release #
  cat("Total Data Loss MSE:", loss, "\n")
  residuals <- Y - y_DNNoutput
  res.std <- (residuals - mean(residuals))/sd(residuals)
  dev.set(1)
  plot(y_DNNoutput,res.std,
       xlab = "Predicted Values", ylab = "Standarized Residuals",
       main = "Residuals Plot vs Predicted Values")
  mtext(paste("Iteration:",i,"Loss:",round(loss, digits = 4),sep = " "))
  abline(0,0)
  # Returning the trained model (Wn)
  model@dnn <- Wn
  model@error <- loss
  model@bestDnn <- bestModel
  model@bestError <- bestLoss
  if(is.null(bestModel2)){model@bestDnn2 <- as.list(bestModel2)}
  else{model@bestDnn2 <- bestModel2}
  model@bestError2 <- bestLoss2
  rm(Wn,bestModel,dhidden,Hx_function,X,Y)
  gc()
  message("\n Finished training. Returning Deep Neural Network model.\n")
  return(model)
}


