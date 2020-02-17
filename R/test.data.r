#' Gene expression of 3 different human brain datasets of cortex region for testing a Deep Neural Network model.
#'
#' Data from 3 different datasets of human cortex. Gene expression of 1049 genes selected
#' because its implication in human aging (Oscar González-Velasco, et al., BBA - Gene Regulatory Mechanisms, https://doi.org/10.1016/j.bbagrm.2020.194491).
#' All 3 datasets are from Affymetrix HG 2.0 microarrays.
#' Gene expression signal is normalized using RMA and it includes the age of every individual.
#'
#' @docType data
#'
#' @usage data(test.data)
#'
#' @format A data.frame of class "data.frame"
#'
#' @keywords datasets
#'
#' @references Datasets are available publicy at GEO: GSE25219, GSE46706, GSE48350
#'
#' @source Datasets are available publicy at GEO: GSE25219, GSE46706, GSE48350
#'
#' @examples
#' data(test.data)
#' head(test.data)
#'
"test.data"
