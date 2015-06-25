#problem1
parole = read.csv("parole.csv")
str(parole)
summary(parole)
sum(parole$violator)
a = names(parole)
parole$state = factor(parole$state)
parole$crime = factor(parole$crime)
str(parole)
summary(parole)

#Problem3
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

#problem 4
parolelm = glm(violator ~ ., data = train, family= "binomial")
summary(parolelm)
exp(1.61)

testquiz = train[1,]
testquiz$male =1
testquiz$race = 1
testquiz$state = levels(testquiz$state)[1]
testquiz$age = 50
testquiz$time.served = 3
testquiz$max.sentence = 12
testquiz$multiple.offenses = 0
testquiz$crime = levels(testquiz$crime)[2]
predictq = predict(parolelm, type = "response", newdata = testquiz)
b = - log((1/predictq) - 1)
odds = exp(b)

#problem5
pred.violator = predict(parolelm, type = "response", newdata = test)
max(pred.violator)
t = 0.2
a = table(test$violator, pred.violator > t)
accuracy = (a[1] + a[4])/ sum(a)
error.rate = (a[2] + a[3]) / sum(a)
sesnitivity = a[4]/(a[2] + a[4])
specificity = a[1]/(a[1] + a[3])
false.neg.error.rate = a[2]/ (a[2] + a[4])
false.pos.error.rate = a[3]/(a[1] + a[3])

basemodel = 1 - (sum(test$violator) / nrow(test))

#ROCR
library(ROCR)
ROCRpredTest = prediction(pred.violator, test$violator)
auc = as.numeric(performance(ROCRpredTest,"auc")@y.values)
auc