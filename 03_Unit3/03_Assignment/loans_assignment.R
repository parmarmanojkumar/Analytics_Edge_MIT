rm(list = ls())
loans = read.csv ("loans.csv")
str(loans)
summary(loans)

library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
loans = read.csv("loans_imputed.csv")

library(caTools)
set.seed(144)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.70)
train = subset(loans, split==TRUE)
test = subset(loans, split == FALSE)
loanslog = glm(not.fully.paid ~ ., data = train, family = binomial)
summary(loanslog)
logita.b = -9.317e-03 * (700-710)
odda.b = exp(logita.b)
predrisk = predict(loanslog, newdata = test , type = "response")

t = 0.5
a = table(test$not.fully.paid, predrisk > t)
accuracy = (a[1] + a[4])/ sum(a)
error.rate = (a[2] + a[3]) / sum(a)
sesnitivity = a[4]/(a[2] + a[4])
specificity = a[1]/(a[1] + a[3])
false.neg.error.rate = a[2]/ (a[2] + a[4])
false.pos.error.rate = a[3]/(a[1] + a[3])
a = table(test$not.fully.paid)
baselm.accuracy = a[1] / sum(a)

library(ROCR)
ROCRPredTest = prediction(predrisk, test$not.fully.paid)
auc = as.numeric(performance(ROCRPredTest,"auc")@y.values)
auc

test$predicted.risk = predrisk

smartbllog = glm(not.fully.paid ~ int.rate, data = train, family = binomial)
summary(smartbllog)
smartbl.predrisk = predict(smartbllog, newdata = test , type = "response")
t = 0.5
a = table(test$not.fully.paid, smartbl.predrisk > t)
max(smartbl.predrisk)
ROCRPredTest = prediction(smartbl.predrisk, test$not.fully.paid)
auc = as.numeric(performance(ROCRPredTest,"auc")@y.values)
auc

roi = 10 * exp(6 * 3/100)
roi
investment = 1
test$profit = investment  * (exp(test$int.rate*3) -1)
test$profit[test$not.fully.paid == 1] = -investment
max(test$profit)
highInterest = subset(test , int.rate >0.15 )
mean(highInterest$profit)
mean(highInterest$not.fully.paid)
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(highInterest , predicted.risk <= cutoff)
mean(selectedLoans$profit)
sum(selectedLoans$not.fully.paid)
