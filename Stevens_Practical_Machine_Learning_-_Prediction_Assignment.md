Executive Summary
=================

Using data generated from activity monitors such as Jawbone Up, Nike
FuelBand, and Fitbit. Here we analyzed this data set to predict the
manner in which people performed each exercise in the difference classes
they took.

### load the data from CSV files

    library(caret)

    ## Warning: package 'caret' was built under R version 3.4.3

    ## Loading required package: lattice

    ## Loading required package: ggplot2

    library(rpart)
    library(rpart.plot)

    ## Warning: package 'rpart.plot' was built under R version 3.4.3

    library(rattle)

    ## Warning: package 'rattle' was built under R version 3.4.3

    ## Rattle: A free graphical interface for data science with R.
    ## Version 5.1.0 Copyright (c) 2006-2017 Togaware Pty Ltd.
    ## Type 'rattle()' to shake, rattle, and roll your data.

    library(randomForest)

    ## Warning: package 'randomForest' was built under R version 3.4.3

    ## randomForest 4.6-12

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:rattle':
    ## 
    ##     importance

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

    train_url <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
    test_url  <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

    train_csv <- read.csv(url(train_url),na.strings=c("NA","#DIV/0!",""))
    test_csv  <- read.csv(url(test_url),na.strings=c("NA","#DIV/0!",""))

### remove any columns with near zero values

    nzvTraining <- nearZeroVar(train_csv, saveMetrics=TRUE)
    train_Sub <- train_csv[,nzvTraining$nzv==FALSE]

    train_Sub <- train_Sub[,7:length(colnames(train_Sub))]

### Count NAs and remove columns with greater than 50% NA as well as drop NAs from data frame

    nonnaCols <- as.vector(apply(train_Sub, 2, function(train_Sub) length(which(!is.na(train_Sub)))))
    dropNAs <- c()
    for (i in 1:length(nonnaCols)) {
      if (nonnaCols[i] > nrow(train_Sub)*.50) {
        dropNAs <- c(dropNAs, colnames(train_Sub)[i])
      }
    }

    train_Sub <- train_Sub[,(names(train_Sub) %in% dropNAs)]

### remove variable "classe" so that both the test and training data sets have matching variables

    keepCols <- colnames(train_Sub[, -53])
    test_Sub <- test_csv[keepCols] 

### Generate a training and validation data partition

    inTrain = createDataPartition(train_Sub$classe, p = 0.7, list=FALSE)
    training = train_Sub[ inTrain,]
    validation = train_Sub[-inTrain,]

Predicting the Manner in Which Excersizes are done:
===================================================

Here we test out 3 different models to determine which model predicts
with the highest accuracy on the training set. \#\#\# Decision Tree with
Cross validation

    set.seed(250)
    tc <- trainControl(method="cv", number=10)
    mod_DT_CV <- train(classe~., method="rpart", data=training, trControl = tc)

### Decision Tree: Determine Out of Sample Error on Validation data set

    pred_DT_CV <- predict(mod_DT_CV, newdata=validation)
    confusionMatrix(pred_DT_CV, validation$classe)

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1524  478  463  420  142
    ##          B   24  383   32  180  150
    ##          C  121  278  531  364  279
    ##          D    0    0    0    0    0
    ##          E    5    0    0    0  511
    ## 
    ## Overall Statistics
    ##                                          
    ##                Accuracy : 0.5011         
    ##                  95% CI : (0.4882, 0.514)
    ##     No Information Rate : 0.2845         
    ##     P-Value [Acc > NIR] : < 2.2e-16      
    ##                                          
    ##                   Kappa : 0.3484         
    ##  Mcnemar's Test P-Value : NA             
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9104  0.33626  0.51754   0.0000  0.47227
    ## Specificity            0.6431  0.91867  0.78555   1.0000  0.99896
    ## Pos Pred Value         0.5035  0.49805  0.33757      NaN  0.99031
    ## Neg Pred Value         0.9475  0.85223  0.88520   0.8362  0.89365
    ## Prevalence             0.2845  0.19354  0.17434   0.1638  0.18386
    ## Detection Rate         0.2590  0.06508  0.09023   0.0000  0.08683
    ## Detection Prevalence   0.5144  0.13067  0.26729   0.0000  0.08768
    ## Balanced Accuracy      0.7767  0.62746  0.65155   0.5000  0.73562

### Stochastic Gradient Boosting with Cross Validation

    set.seed(250)
    tc <- trainControl(method="cv", number=10)
    mod_gbm <- train(classe~., method="gbm", data=training, trControl = tc,verbose = FALSE)

### Stochastic Gradient Boosting: Determine Out of Sample Error on Validation data set

    pred_gbm <- predict(mod_gbm, newdata=validation)
    confusionMatrix(pred_gbm, validation$classe)

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1638   30    0    0    3
    ##          B   19 1075   24    5   15
    ##          C    7   30  987   26   12
    ##          D    6    2   14  923   14
    ##          E    4    2    1   10 1038
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9619          
    ##                  95% CI : (0.9567, 0.9667)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9519          
    ##  Mcnemar's Test P-Value : 1.092e-05       
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9785   0.9438   0.9620   0.9575   0.9593
    ## Specificity            0.9922   0.9867   0.9846   0.9927   0.9965
    ## Pos Pred Value         0.9803   0.9446   0.9294   0.9625   0.9839
    ## Neg Pred Value         0.9915   0.9865   0.9919   0.9917   0.9909
    ## Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
    ## Detection Rate         0.2783   0.1827   0.1677   0.1568   0.1764
    ## Detection Prevalence   0.2839   0.1934   0.1805   0.1630   0.1793
    ## Balanced Accuracy      0.9853   0.9653   0.9733   0.9751   0.9779

### Random Forest

    set.seed(250)
    tc <- trainControl(method="cv", number=10)
    mod_rf <- train(classe~., method="rf", data=training, ntree=100, trControl = tc)

### Random Forest: Determine Out of Sample Error on Validation data set

    pred_rf <- predict(mod_rf, newdata=validation)
    confusionMatrix(pred_rf, validation$classe)

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1664    5    0    0    0
    ##          B    7 1131    2    0    0
    ##          C    2    3 1021   12    5
    ##          D    0    0    3  951    4
    ##          E    1    0    0    1 1073
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9924          
    ##                  95% CI : (0.9898, 0.9944)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9903          
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9940   0.9930   0.9951   0.9865   0.9917
    ## Specificity            0.9988   0.9981   0.9955   0.9986   0.9996
    ## Pos Pred Value         0.9970   0.9921   0.9789   0.9927   0.9981
    ## Neg Pred Value         0.9976   0.9983   0.9990   0.9974   0.9981
    ## Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
    ## Detection Rate         0.2828   0.1922   0.1735   0.1616   0.1823
    ## Detection Prevalence   0.2836   0.1937   0.1772   0.1628   0.1827
    ## Balanced Accuracy      0.9964   0.9955   0.9953   0.9925   0.9956

Determine Accuracy, Error, and Results of Three Models
======================================================

Decission Tree Test Results
---------------------------

    pred_DT_test <- predict(mod_DT_CV, newdata=test_Sub)
    Error_DT <- 1 - as.numeric(confusionMatrix(pred_DT_CV, validation$classe)$overall[1])
    accuracy_DT <- postResample(pred_DT_CV, validation$classe)
    pred_DT_test

    ##  [1] C A A A A C C A A A C C C A C A A A A C
    ## Levels: A B C D E

    Error_DT

    ## [1] 0.4988955

    accuracy_DT

    ##  Accuracy     Kappa 
    ## 0.5011045 0.3484272

Stochastic Gradient Boosting Test Results
-----------------------------------------

    pred_gbm_test <- predict(mod_gbm, newdata=test_Sub)
    Error_gbm <- 1 - as.numeric(confusionMatrix(pred_gbm, validation$classe)$overall[1])
    accuracy_gbm <- postResample(pred_gbm, validation$classe)
    pred_gbm_test

    ##  [1] B A B A A E D B A A B C B A E E A B B B
    ## Levels: A B C D E

    Error_gbm

    ## [1] 0.03806287

    accuracy_gbm

    ##  Accuracy     Kappa 
    ## 0.9619371 0.9518613

Random Forest Test Results
--------------------------

    pred_rf_test <- predict(mod_rf, newdata=test_Sub)
    Error_rf <- 1 - as.numeric(confusionMatrix(pred_rf, validation$classe)$overall[1])
    accuracy_rf <- postResample(pred_rf, validation$classe)
    pred_rf_test

    ##  [1] B A B A A E D B A A B C B A E E A B B B
    ## Levels: A B C D E

    Error_rf

    ## [1] 0.007646559

    accuracy_rf

    ##  Accuracy     Kappa 
    ## 0.9923534 0.9903292

Conclusions
===========

After looking at the data we tested three different models to determine
which had the best accuracy with the lowest error and predicted the our
test data set with high benchmarks. We found that the decision tree,
although fast, was not very accurate and had a high error rate. Next we
looked at Generalized Linear Model and this gave very nice results with
high accuracy at 0.963 which was considerably better than decision
trees, but was also considerably slower. Random Forest was our third
model which ran fast and gave us an accuracy of 0.9932. It seems to show
that random forest is the preferred method for prediction and when
utilized with the test set give the proper results. Both random forest
and the gradient boosting gave the same output of predictions for the
test data set. one missing piece is the possibility of overfitting, but
the test data seems to predict well.

APPENDIX
========

### Figure 1: Decision Tree

    library(rattle)
    fancyRpartPlot(mod_DT_CV$finalModel)

![](Stevens_Practical_Machine_Learning_-_Prediction_Assignment_files/figure-markdown_strict/unnamed-chunk-15-1.png)
