### Defining the DeepNNModel class:
# This class will store all information, structure and data of the Neural Network.
DeepNNModel <-
  setClass(Class = "DeepNNModel",
           slots = list(
             dnn = "list",
             error = "numeric",
             bestDnn = "list",
             bestError = "numeric",
             bestDnn2 = "list",
             bestError2 = "numeric",
             dnn.structure = "vector",
             training = "list"
           )
  )
setMethod("show",
          "DeepNNModel",
          function(object) {
            cat("Object of class: ", class(object), "\n")
            cat("\t - stores a deep neural network model and its metrics.")
          })
