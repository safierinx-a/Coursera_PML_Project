---
title: "PracticalMachineLearning_CourseProject"
author: "Apoorv Saxena"
date: "27/04/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(caret)
```

## Loading Data

Let's load our training and testing data from the provided links. 
```{r}
url_train <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
url_test <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'
suppressMessages(download.file(url_train, 'train.csv'))
suppressMessages(download.file(url_test, 'test.csv'))
traindata <- read.csv('train.csv')
testdata <- read.csv('test.csv')

```

Let's take a look at our data to get a better understanding of its structure.
```{r}
str(traindata)

```
Let's delete the columns with NA values in either the train or test sets..
```{r}

s <- colnames(traindata)[colSums(is.na(traindata)) > 0]
t <- colnames(testdata)[colSums(is.na(testdata)) > 0]
del <- unique(c(s,t))
newtrain <-traindata[,!(names(traindata) %in% del)]
newtest <- testdata[,!(names(testdata) %in% del)]

```

Now, split the train data into testing and training data for cross validation.
```{r}
inTrain <- createDataPartition(y = newtrain$classe, p = 0.7, list = FALSE)
training <- newtrain[inTrain,]
testing <- newtrain[-inTrain,]
```

Let's preprocess the data a bit and reduce the predictors using PCA.
```{r}
preProc <- preProcess(training, method = "pca", pcaComp = 4)
trainPC <- predict(preProc, training)
testPC <- predict(preProc, testing)
```


Let's train a couple of models on the training data.
```{r message=FALSE, warning=FALSE, paged.print=FALSE}
rf <- train(classe~., method= "rf", data = trainPC)
#glm <- train(classe~., method= "glm", data = trainPC)
gbm <- train(classe~., method= "gbm", data = trainPC)
lda <- train(classe~., method= "lda", data = trainPC)


```
Now let's compare their accuracies.
```{r}
pred_rf <- predict(rf, testPC)
pred_gbm <- predict(rf, testPC)
pred_lda <- predict(rf, testPC)
confusionMatrix(pred_rf, testing$classe)
confusionMatrix(pred_gbm, testing$classe)
confusionMatrix(pred_lda, testing$classe)
```


Weirdly enough, all of them yield the same accuracy. Let's stack Random Forests and Bagging and see if performance improves.
```{r}
predDF <- data.frame(pred_rf, pred_gbm, classe = testing$classe)

combModFit <- train(classe~., method = "rf", data = predDF)
combPred <- predict(combModFit, predDF)
confusionMatrix(combPred, predDF$classe)
```

Still the same accuracy? I suppose I'll use this model since the Balanced Accuracies in this model are better. Let's predict values for our given test set, then.
```{r}
test <- predict(preProc, newtest)
pred_rf <- predict(rf, test)
pred_gbm <- predict(rf, test)
predDF <- data.frame(pred_rf, pred_gbm)
combPred <- predict(combModFit, predDF)
combPred

```





