DeepNeuralNetworks4R
================

Implementation of *Deep Neural Networks* in R programing language.
----------------

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

We will try to predict petal length from the other parameters.

``` r
# We load the DNN algorithm:
source("./deepNN_algorithmRegressionV3.3.r")

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

    ##              137  49  85 140  90
    ## Sepal.Length 6.3 5.3 5.4 6.9 5.5
    ## Sepal.Width  3.4 3.7 3.0 3.1 2.5
    ## Petal.Length 5.6 1.5 4.5 5.4 4.0
    ## Petal.Width  2.4 0.2 1.5 2.1 1.3
    ## Species      3.0 1.0 2.0 3.0 2.0

Here we can find an except for the training dataset, *notice* that *response variables* correspond to **rows**, meanwhile *samples* correspond to **columns** .

Training the regression model
-----------------------------

First, we proceed to create the deep neural network model:

``` r
dnn.model <- deepNeuralNetwork.build(x=c(1,2,4,5),y=3, outputNeurons = 1,
                                 HidenLayerNeurons = c(30,10,3),traindata=data,
                                 random.seed = 1, drawDNN = 0)
```

**x** will specify the index positions of our explanatory variables on the matrix *data*

**HidenLayerNeurons** will specify the number of neurons that each layer will have. The number of neurons on the very first layer will be the number of variables that we will use to create the regression model.

**deepNeuralNetwork.build** will create an object of class *DeepNNModel* that will store all the information about the dnn model:

`dnn.model@dnn.structure` returns the number of neurons in each layer

    ## 4 30 10  3  3  3  1

``` r
class(dnn.model)
```

    ## "DeepNNModel"

And now we train the deep neural network using the following code:

``` r
# 3. train model
dnn.model.trained <- deepNeuralNetwork.training(x=c(1,2,4,5),y=3, model = dnn.model, #ddn.model.in.use, 
                                              traindata=training.MX, testdata=test.MX, 
                                              iterations  = 15000, lr = 0.001, 
                                              reg = 0.001, display=500,maxError = 0.1)
```

Testing the results
-------------------

Once we have the model, we will make use of **deepNeuralNetwork.predict** function to predict a feature based on the trained regression model:

``` r
petal.length.prediction <- deepNeuralNetwork.predict(model.trained = dnn.model.trained@bestDnn,
                                                     data = test.MX[-3,])
source("linearPlot.r")
mplot_lineal(tag = test.MX[3,],score = petal.length.prediction,title = "Petal length prediction using DNN regression",
             x.lim = c(1,7),y.lim = c(1,7),x.lab="petal length (observed)",y.lab = "petal length (predicted)")
```

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)

Using GPU for large datasets
----------------------------

The DeepNN algorithm has been optimized to be executed on a GPU card using R's matrix/vector arithmetic expressions.

The following example shows how to run the DNN on a GPU using CUDA with R's nvblas config file:

``` bash
sudo env LD_PRELOAD=/PATH/TO/CUDA/NVBLAS/libnvblas.so NVBLAS_CONFIG_FILE=/PATH/TO/NVBLAS.CONFIG.FILE/nvblas.conf R CMD BATCH ./regression.deepNN.GPU.r /dev/tty
```
