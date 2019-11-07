README
================
Óscar González-Velasco
6 november 2019

\# DeepNeuralNetworks4R
=======================

Implementation of *Deep Neural Networks* in R programing language.
------------------------------------------------------------------

Regression algorithm for Omic data prediction in brain transcriptomics (although as a regression model, it can be applied to **any** problem with a dependent continuous variable).

We will use **iris** dataset as a tiny example of a **regression** model using *deep neural networks*:

``` r
library(datasets)
data(iris)
summary(iris)
```

    ##   Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
    ##  Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100  
    ##  1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300  
    ##  Median :5.800   Median :3.000   Median :4.350   Median :1.300  
    ##  Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199  
    ##  3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800  
    ##  Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500  
    ##        Species  
    ##  setosa    :50  
    ##  versicolor:50  
    ##  virginica :50  
    ##                 
    ##                 
    ## 

``` r
setwd("/home/oscar/Escritorio/DeepNeuralNetworks4R/")
source("deepNN_algorithmRegressionV3.3.r")

# We will pick 110 samples for the training set and the remaining 40 for the test set.
training.MX <- sample(1:nrow(iris),size = 110)
test.MX <- setdiff(1:150,training.MX)

training.MX <- iris[training.MX,]
training.MX$Species <- as.numeric(training.MX$Species)
training.MX <- t(training.MX)

test.MX <- iris[test.MX,]
test.MX$Species <- as.numeric(test.MX$Species)
test.MX <- t(test.MX)
head(training.MX[,1:5])
```

    ##              122 103 119  88  54
    ## Sepal.Length 5.6 7.1 7.7 6.3 5.5
    ## Sepal.Width  2.8 3.0 2.6 2.3 2.3
    ## Petal.Length 4.9 5.9 6.9 4.4 4.0
    ## Petal.Width  2.0 2.1 2.3 1.3 1.3
    ## Species      3.0 3.0 3.0 2.0 2.0

Here we can find an except for the training dataset, *notice* that *response variables* correspond to **rows**, meanwhile *samples* correspond to **columns** Now we proceed to create the deep neural network model:

``` r
dnn.model <- deepNeuralNetwork.build(x=c(1,2,4,5),y=3, outputNeurons = 1,
                                 HidenLayerNeurons = c(5,3,2,2,2,2,2),traindata=data,
                                 random.seed = 1, drawDNN = 0)
```

And now we train the deep neural network using the following code:

``` r
# 3. train model
timeNN <- system.time(
  dnn.model.trained <- deepNeuralNetwork.training(x=c(1,2,4,5),y=3, model = dnn.model, #ddn.model.in.use, 
                                              traindata=training.MX, testdata=test.MX, 
                                              iterations  = 10000, lr = 0.001, 
                                              reg = 0.001, display=500,maxError = 0.1))
```
