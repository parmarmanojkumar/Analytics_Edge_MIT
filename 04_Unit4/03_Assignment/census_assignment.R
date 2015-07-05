rm(list = ls())
#problem1
census = read.csv("census.csv")
str(census)
library(caTools)
set.seed(2000)
spl = sample.split(census$over50k , SplitRatio = 0.6)
train = subset(census, spl == T)
test = subset(census, spl == F)
logmodel = glm(over50k ~ . , data = train, family = "binomial")
summary(logmodel)
pred.over50k.lm = predict(logmodel, newdata= test, type = "response")
a = as.matrix(table(test$over50k, pred.over50k.lm > 0.5))
a
accuracy.lm = (a[1,1] + a[2,2]) / sum(a)
accuracy.lm
accuracy.bm = max(table(test$over50k)) / nrow(test)
accuracy.bm

library(ROCR)
ROCRpredTest.lm = prediction(pred.over50k.lm, test$over50k)
auc.lm = as.numeric(performance(ROCRpredTest.lm,"auc")@y.values)
auc.lm

#problem2
library(rpart)
library(rpart.plot)
CART.O50k = rpart(over50k ~ . , data = test, method = "class")
prp(CART.O50k)
plot(CART.O50k)
text(CART.O50k)
pred.over50k.cart = predict(CART.O50k, newdata = test, type = "class")
a = as.matrix(table(test$over50k, pred.over50k.cart))
a
accuracy.cart = (a[1,1] + a[2,2]) / sum(a)
accuracy.cart
pred.over50k.cart.p = predict(CART.O50k, newdata = test) #to compute probabilities
ROCRpredTest.cart = prediction(pred.over50k.cart.p[,2], test$over50k)
auc.cart = as.numeric(performance(ROCRpredTest.cart,"auc")@y.values)
auc.cart
ROCRperf.cart = performance(ROCRpredTest.cart, "tpr", "fpr")
ROCRperf.lm = performance(ROCRpredTest.lm, "tpr", "fpr")
# Plot ROC curve
plot(ROCRperf.cart)
plot(ROCRperf.lm)

#problem3
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
set.seed(1)
rF.O50k = randomForest(over50k ~ . , data = trainSmall)
pred.over50k.rF = predict(rF.O50k, newdata = test)
a = as.matrix(table(test$over50k, pred.over50k.rF))
a
accuracy.rF = (a[1,1] + a[2,2]) / sum(a)
accuracy.rF
vu = varUsed(rF.O50k, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(rF.O50k$forest$xlevels[vusorted$ix]))
varImpPlot(rF.O50k)

#problem4
library(caret)
set.seed(2)
cp.grid = expand.grid( .cp = seq(0.002,0.1,0.002))
tr.control = trainControl(method = "cv", number = 10)
tr = train(over50k ~ ., data = train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)
tr
CART.O50k.cp = rpart(over50k ~ . , data = test, method = "class", cp = 0.002)
pred.over50k.cart.cp = predict(CART.O50k.cp, newdata = test, type = "class")
a = as.matrix(table(test$over50k, pred.over50k.cart.cp))
a
accuracy.cart.cp = (a[1,1] + a[2,2]) / sum(a)
accuracy.cart.cp
prp(CART.O50k.cp)
CART.O50k.cp$cptable
CART.O50k$cptable
