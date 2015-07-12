rm(list = ls())
#problem1
stocks = read.csv("StocksCluster.csv")
str(stocks)
summary(stocks)
prop.table(table(stocks$PositiveDec))
sort(abs(cor(stocks)))
sort(colMeans(stocks))
#problem2
library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)
StocksModel = glm(PositiveDec ~ ., data = stocksTrain, family = binomial)
PredictTrain = predict(StocksModel, type="response")
t = 0.5
a = table(stocksTrain$PositiveDec, PredictTrain >= t)
accuracyTrain = (a[1] + a[4])/ sum(a)
accuracyTrain

PredictTest = predict(StocksModel, newdata = stocksTest,type="response")
t = 0.5
a = table(stocksTest$PositiveDec, PredictTest >= t)
accuracyTest = (a[1] + a[4])/ sum(a)
accuracyTest
accuracyBlTest = prop.table(table(stocksTest$PositiveDec))
accuracyBlTest

#problem3
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
colMeans(normTrain)
colMeans(normTest)
set.seed(144)
km = kmeans(normTrain, centers = 3)
km$size
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)
#problem4
stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)
stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)
mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

StocksModel1 = glm(PositiveDec ~ ., data = stocksTrain1, family = binomial)
StocksModel2 = glm(PositiveDec ~ ., data = stocksTrain2, family = binomial)
StocksModel3 = glm(PositiveDec ~ ., data = stocksTrain3, family = binomial)
summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)
sign(StocksModel1$coefficients) +
sign(StocksModel2$coefficients) +
sign(StocksModel3$coefficients)

PredAccuracy <- function(model, testdata){
        PredictTest = predict(model, newdata = testdata,type="response")
        t = 0.5
        a = table(testdata$PositiveDec, PredictTest >= t)
        accuracy = (a[1] + a[4])/ sum(a)
        accuracy
}
accuracy.SM1 = PredAccuracy(StocksModel1, stocksTest1)
accuracy.SM1
accuracy.SM2 = PredAccuracy(StocksModel2, stocksTest2)
accuracy.SM2
accuracy.SM3 = PredAccuracy(StocksModel3, stocksTest3)
accuracy.SM3
PredictTest1 = predict(StocksModel1, newdata = stocksTest1, type="response")
PredictTest2 = predict(StocksModel2, newdata = stocksTest2, type="response")
PredictTest3 = predict(StocksModel3, newdata = stocksTest3, type="response")
AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

t = 0.5
a = table(AllOutcomes, AllPredictions >= t)
accuracy.all = (a[1] + a[4])/ sum(a)
accuracy.all
