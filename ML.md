Practical Machine Learning :By Kshitiz Arora
================

## Loading the required packages and libraries

``` r
library(caret)
```

    ## Warning: package 'caret' was built under R version 3.6.3

    ## Loading required package: lattice

    ## Loading required package: ggplot2

    ## Warning: package 'ggplot2' was built under R version 3.6.1

``` r
library(randomForest)
```

    ## Warning: package 'randomForest' was built under R version 3.6.3

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

## Getting the data from the source

Load the data from the
url

``` r
rawtraining<- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"))

rawtesting<- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"))

dim(rawtraining)
```

    ## [1] 19622   160

``` r
dim(rawtesting)
```

    ## [1]  20 160

## Cleaning the Data

Lets check how many rows are complete, that is, rows which do not have
any NA’s

``` r
sum(complete.cases(rawtraining))
```

    ## [1] 406

``` r
sum(complete.cases(rawtesting))
```

    ## [1] 0

It is clearly visible that alot of rows in testing and training data has
NA values.

Now, lets removes NA values

``` r
rawtraining<- rawtraining[, colSums(is.na(rawtraining))==0]

rawtesting<- rawtesting[, colSums(is.na(rawtesting))==0]

dim(rawtraining)
```

    ## [1] 19622    93

``` r
dim(rawtesting)
```

    ## [1] 20 60

There are few variables which are of no use in predicting the manner in
which excercise is done. These variables just provide general
information which can be removed as per personal judgment. Lets remove
these variables

``` r
classe<-rawtraining$classe
remove<- grepl("^X|user_name|window|timestamp", names(rawtraining))
rawtraining<- rawtraining[,!remove]
train<-rawtraining[,sapply(rawtraining, is.numeric)]
train$classe<-classe

removetest<- grepl("^X|user_name|window|timestamp", names(rawtesting))
rawtesting<- rawtesting[ ,!removetest]
test<-rawtesting[,sapply(rawtesting, is.numeric)]
```

## Data Slicing

We can divide our training data into training and cross
validation(testing) data.

``` r
set.seed(12345)
inTrain<- createDataPartition(train$classe, p=0.70, list = FALSE)
training<-train[inTrain,]
testing<-train[-inTrain,]
```

## Data Modelling

We will be considering two models for our data

1)Random Forest Model

``` r
modelrf<- randomForest(classe~. ,data=training, importance=TRUE)
predict_rf<- predict(modelrf, testing)
accuracy_rf<-confusionMatrix(testing$classe, predict_rf)$overall[1]
accuracy_rf
```

    ##  Accuracy 
    ## 0.9960918

``` r
out_of_samp_err_rf<- 1-accuracy_rf
out_of_samp_err_rf
```

    ##    Accuracy 
    ## 0.003908241

The accuracy of Random Forest Model comes about 99.61% and estimated out
of sample error is 0.39%

2)  Genarlized Boosted Model

<!-- end list -->

``` r
modelgbm<- train(classe~. ,data=training, method="gbm")
predict_gbm<- predict(modelgbm, testing)
```

``` r
accuracy_gbm<-confusionMatrix(testing$classe, predict_gbm)$overall[1]
accuracy_gbm
```

    ##  Accuracy 
    ## 0.9580289

``` r
out_of_samp_err_gbm<- 1-accuracy_gbm
out_of_samp_err_gbm
```

    ##   Accuracy 
    ## 0.04197111

The accuracy for gbm model comes about 96.10% and estimated out of
sample error is 3.85%

We will go ahead with random forest model because: –Better accuracy
–GBMs are more sensitive to overfitting

## Predict on Test data

We will use random forest method to predict on original test data set

``` r
result <-predict(modelrf, test)
result
```

    ##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
    ##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
    ## Levels: A B C D E
