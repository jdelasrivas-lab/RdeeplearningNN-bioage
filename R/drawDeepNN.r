#' Plot of the DNN
#'
#' This function plots the DNN arquitecture.
#'
#' @import ggplot2
#'
#' @param NperL Vector. The layers and the number of neurons in each.
#' @param biasVector NA. Deprecated.
#' @export
draw.dnn <- function(NperL = 0,biasVector){
  if (!require("igraph",quietly = T,warn.conflicts = F)){
    message("igraph library is not installed. Can not plot the DNN arquitecture.")
    return(-1)
  }
  message("\n\t Drawing a beautiful Deep Neural Network for you!! :)\n")
  nOfLayer <- length(NperL)
  x <- unlist(sapply(c(1:nOfLayer),function(x){rep(x,NperL[x])}))
  y <- unlist(sapply(c(1:nOfLayer),function(x){
    scale(c(NperL[x]:1), center=TRUE, scale=FALSE)}))
  nodes <- c(1:sum(NperL))
    #nodes <- c('a','b','c','d','e','f','g')
  from <- c(nodes[1:sum(NperL[1:length(NperL)-1])])
  from <- unlist(sapply(from,function(i){
    rep(i,NperL[x[i]+1])
  }))
  #from <- c('a','b','c','d','e','f')
  to <- unlist(sapply(nodes[1:sum(NperL[1:length(NperL)-1])],function(i){
    which(x == x[i]+1)
  }))
  #to <- c('e','e','f','f','g','g')
  NodeList <- data.frame(nodes, x ,y)
  EdgeList <- data.frame(from, to)
  ################
  ## Bias Entities
  bias <- sapply(1:(nOfLayer-1),function(x)paste("b",x,sep = ""))
  ybias <- rep(max(y)+1,length(bias))
  xbias <- 1.5:(length(bias)+0.5)
  frombias <- unlist(sapply(1:length(bias),function(i){
    rep(bias[i],NperL[i+1])
  }))
  tobias <- unlist(sapply(1:length(bias),function(i){
    which(x == i +1)
  }))
  NodeListBias <- data.frame(bias, xbias ,ybias)
  NodeListBias$bias <- as.character(NodeListBias$bias)
  EdgeListBias <- data.frame(frombias, tobias)
  EdgeListBias$frombias <- as.character(EdgeListBias$frombias)
  ### End of Bias
  ###############
  ## Variables inputs Xn
  input <- sapply(1:NperL[1],function(x)paste("X",x,sep = ""))
  yinput <- y[1:NperL[1]]
  xinput <- rep(0,NperL[1])
  NodeListInput <- data.frame(input, xinput ,yinput)
  NodeListInput$input <- as.character(NodeListInput$input)
  fromInput <- input
  toInput <- nodes[1:NperL[1]]
  EdgeListInput <- data.frame(fromInput, toInput)
  EdgeListInput$fromInput <- as.character(EdgeListInput$fromInput)
  ## End inputs
  NodeList$nodes <- as.character(NodeList$nodes)
  NodeList[c((nrow(NodeList)+1):(nrow(NodeList)+nrow(NodeListBias))),] <- NodeListBias
  EdgeList[c((nrow(EdgeList)+1):(nrow(EdgeList)+nrow(EdgeListBias))),] <- EdgeListBias
  NodeList[c((nrow(NodeList)+1):(nrow(NodeList)+nrow(NodeListInput))),] <- NodeListInput
  EdgeList[c((nrow(EdgeList)+1):(nrow(EdgeList)+nrow(EdgeListInput))),] <- EdgeListInput
  ## PLOTING ##
  a <- graph_from_data_frame(vertices = NodeList, d= EdgeList, directed = F)
  V(a)$shape <- "circle"
  V(a)[V(a)$name %in% input]$shape <- "none"
  E(a)$arrow.mode <- 0
  # Mode 2 to put arrows forward
  ##E(a)[V(a)$name %in% input]$arrow.mode <- 2
  netColors <- rep("white",length(NodeList$nodes))
  netColors[which(NodeList$nodes %in% bias)] <- "red"
  netColors[which(x == 1)] <- "green"
  netColors[which(x == nOfLayer)] <- "blue"
  V(a)$color <- netColors
  plot(a)
  return(a)
}

# dev.off()
# a <- draw.dnn(NperL = c(6,10,10,8,5))
