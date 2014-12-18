---
title: "Practical Machine Learning  Assessment"
author: "Miseon Song"
date: "Thursday, December 18, 2014"
output: html_document
---


# Human Activity Recognition : How to predict the manner in which people did the exercise.

Peer Assessments /Prediction Assignment Writeup

## Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 

## Data 
The training data for this project are available here: 
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here: 
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. The information has been generously provided for use in the Coursera course Assignment (Practical Machine Learning )

## Data Processing

Load data


```r
# Load data
# Repalce '#DIV/0!' with an NA value
# Variable names for 2 data sets are the same excpet the last variable : 
#  classe for training, problem_id for scoring data set

training = read.csv("C:/data/pml-training.csv",na.strings=c("#DIV/0!"),row.names=1)
scoring  = read.csv("C:/data//pml-testing.csv",row.names=1)
dim(training)  
```

```
## [1] 19622   159
```

```r
dim(scoring)  
```

```
## [1]  20 159
```

For data cleaning, the variables were removed with statistic related, many NAs, spaces, high correlated( > 0.8).


```r
# Remove variables for 1-5 columns 
#    user_name raw_timestamp_part_1  raw_timestamp_part_2 cvtd_timestamp	new_window
# and variables with stat prefixes(skewness, kurtosis, min, max, avg, var, stddev)

training1=training[,-c(1:5,11:35,49:58,68:82,86:100,102:111,124:138,140:149)]
dim(training1) 
```

```
## [1] 19622    54
```

```r
# Remove high correlated variables
tmp <- cor(training1[,-54])
tmp[upper.tri(tmp)] <- 0
diag(tmp) <- 0
# Above two commands can be replaced with 
# tmp[!lower.tri(tmp)] <- 0
#
# training2 <- training1[,!apply(tmp,2,function(x) any(x > 0.8))]
training2= training1[, apply(tmp,2,function(x) all(x<=0.65))]
dim(training2)  # [1] 19622    34
```

```
## [1] 19622    34
```

```r
names(scoring)[159]='classe'
scoring2=scoring[,names(training2)]
```

Since caret was too slow, randomForest() was used for fitting the model.



```r
# Partition rows into training and testing
set.seed(1234)
library(caret)
inTrain = createDataPartition(training2$classe, p = 0.6,list=F)
training3 = training2[ inTrain,]
testing =   training2[-inTrain,]

library(randomForest)
modFit=randomForest(classe ~., data=training3, ntree=100, Importance=T)
# modFit$importance
# modFit$confusion
pred=predict(modFit,testing)
confusionMatrix(pred, testing$classe) 
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2232    5    0    0    0
##          B    0 1511   12    0    0
##          C    0    2 1355   12    0
##          D    0    0    1 1273    1
##          E    0    0    0    1 1441
## 
## Overall Statistics
##                                          
##                Accuracy : 0.9957         
##                  95% CI : (0.9939, 0.997)
##     No Information Rate : 0.2845         
##     P-Value [Acc > NIR] : < 2.2e-16      
##                                          
##                   Kappa : 0.9945         
##  Mcnemar's Test P-Value : NA             
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   0.9954   0.9905   0.9899   0.9993
## Specificity            0.9991   0.9981   0.9978   0.9997   0.9998
## Pos Pred Value         0.9978   0.9921   0.9898   0.9984   0.9993
## Neg Pred Value         1.0000   0.9989   0.9980   0.9980   0.9998
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2845   0.1926   0.1727   0.1622   0.1837
## Detection Prevalence   0.2851   0.1941   0.1745   0.1625   0.1838
## Balanced Accuracy      0.9996   0.9967   0.9942   0.9948   0.9996
```
The accuracy for training dat set was 99.57%. 

Predicting test set is as follows.


```r
table(pred,testing$classe)
```

```
##     
## pred    A    B    C    D    E
##    A 2232    5    0    0    0
##    B    0 1511   12    0    0
##    C    0    2 1355   12    0
##    D    0    0    1 1273    1
##    E    0    0    0    1 1441
```


The above result shows that the Random Forest prediction was far better than any other mode (not shown).  The confusion matrix gives an accuracy  98.57% for testing data set.

Using 20 test cases, the predicted classe are as follows.


```r
# Scoring for 20 test cases
pred_score=predict(modFit,scoring2)
```

## Conclusion

In predicting the target variable ‘classe’, the Random forest method worked well with accuracy 98.57%. 


