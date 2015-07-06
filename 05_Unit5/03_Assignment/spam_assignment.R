rm(list = ls())
#problem1
emails = read.csv("emails.csv", stringsAsFactor = F)
str(emails)
table(emails$spam)
emails$text[1]
max(nchar(emails$text))
which.min(nchar(emails$text))
#problem2
tm_pre_process <- function(data){
        library(tm)
        corpusTitle = Corpus(VectorSource(data))
        corpusTitle = tm_map(corpusTitle, tolower)
        corpusTitle = tm_map(corpusTitle, PlainTextDocument)
        corpusTitle = tm_map(corpusTitle, removePunctuation)
        corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
        corpusTitle = tm_map(corpusTitle, stemDocument)
        corpusTitle
}
corpus = tm_pre_process(emails$text)
dtm = DocumentTermMatrix(corpus)
dtm
spdtm = removeSparseTerms(dtm, 0.95)
spdtm
emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))
emailsSparse$spam = emails$spam
sum(colSums(emailsSparse)>5000 && emailsSparse$spam == 1)
emailsham = subset(emailsSparse , emailsSparse$spam == 0)
sum(colSums(emailsham)>5000)
sum(colSums(subset(emailsSparse, spam == 0)) > 5000)
sum(colSums(subset(emailsSparse, spam == 1)) > 1000)
#Problem3
emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)
library(caTools)
spl = sample.split(emailsSparse$spam, 0.7)
train = subset(emailsSparse, spl == T)
test = subset(emailsSparse, spl == F)

spamLog = glm(spam ~ ., data = train, family ="binomial")
library(rpart)
library(rpart.plot)
spamCART = rpart(spam ~ ., data = train, method ="class")
library(randomForest)
set.seed(123)
spamRF = randomForest(spam ~ . , data = train )

predspam.log = predict(spamLog, type ="response")
predspam.CART = predict(spamCART)
predspam.CART = predspam.CART[,2]
predspam.RF = predict(spamRF, type = "prob")
predspam.RF = predspam.RF[,2]

sum(predspam.log < 0.00001)
sum(predspam.log  > 0.99999)
sum(predspam.log >= 0.00001 & predspam.log <= 0.99999)
summary(spamLog)

prp(spamCART)

t = 0.5
a = table(train$spam, predspam.log >= t)
accuracy.spamlog.train = (a[1] + a[4])/ sum(a)
accuracy.spamlog.train

library(ROCR)
predspam.log.train.rocr = prediction(predspam.log, train$spam)
auc.log.train = performance(predspam.log.train.rocr, "auc")@y.values
auc.log.train

t = 0.5
a = table(train$spam, predspam.CART >= t)
accuracy.spamCART.train = (a[1] + a[4])/ sum(a)
accuracy.spamCART.train

predspam.CART.train.rocr = prediction(predspam.CART, train$spam)
auc.CART.train = performance(predspam.CART.train.rocr, "auc")@y.values
auc.CART.train

t = 0.5
a = table(train$spam, predspam.RF >= t)
accuracy.spamRF.train = (a[1] + a[4])/ sum(a)
accuracy.spamRF.train

predspam.RF.train.rocr = prediction(predspam.RF, train$spam)
auc.RF.train = performance(predspam.RF.train.rocr, "auc")@y.values
auc.RF.train

#problem4
predspam.log.test = predict(spamLog, type ="response", newdata = test)
predspam.CART.test = predict(spamCART, newdata = test)
predspam.CART.test = predspam.CART.test[,2]
predspam.RF.test = predict(spamRF, type = "prob", newdata = test)
predspam.RF.test = predspam.RF.test[,2]

t = 0.5
a = table(test$spam, predspam.log.test >= t)
accuracy.spamlog.test = (a[1] + a[4])/ sum(a)
accuracy.spamlog.test

predspam.log.test.rocr = prediction(predspam.log.test, test$spam)
auc.log.test = performance(predspam.log.test.rocr, "auc")@y.values
auc.log.test

t = 0.5
a = table(test$spam, predspam.CART.test >= t)
accuracy.spamCART.test = (a[1] + a[4])/ sum(a)
accuracy.spamCART.test

predspam.CART.test.rocr = prediction(predspam.CART.test, test$spam)
auc.CART.test = performance(predspam.CART.test.rocr, "auc")@y.values
auc.CART.test

t = 0.5
a = table(test$spam, predspam.RF.test >= t)
accuracy.spamRF.test = (a[1] + a[4])/ sum(a)
accuracy.spamRF.test

predspam.RF.test.rocr = prediction(predspam.RF.test, test$spam)
auc.RF.test = performance(predspam.RF.test.rocr, "auc")@y.values
auc.RF.test

#problem6
wordCount = rowSums(as.matrix(dtm))
wordCount
hist(wordCount)
hist(log(wordCount))
emailsSparse$logWordCount = log(wordCount)
boxplot(emailsSparse$logWordCount~emailsSparse$spam)
train2 = subset(emailsSparse, spl == T)
test2 = subset(emailsSparse, spl == F)
spam2CART = rpart(spam ~ ., data = train2, method ="class")
prp(spam2CART)
set.seed(123)
spam2RF = randomForest(spam ~ . , data = train2 )
predspam.CART2.test = predict(spam2CART, newdata = test2)
predspam.CART2.test = predspam.CART2.test[,2]
predspam.RF2.test = predict(spam2RF, type = "prob", newdata = test2)
predspam.RF2.test = predspam.RF2.test[,2]

t = 0.5
a = table(test2$spam, predspam.CART2.test >= t)
accuracy.spam2CART.test = (a[1] + a[4])/ sum(a)
accuracy.spam2CART.test

predspam.CART2.test.rocr = prediction(predspam.CART2.test, test2$spam)
auc.CART2.test = performance(predspam.CART2.test.rocr, "auc")@y.values
auc.CART2.test

t = 0.5
a = table(test2$spam, predspam.RF2.test >= t)
accuracy.spam2RF.test = (a[1] + a[4])/ sum(a)
accuracy.spam2RF.test

predspam.RF2.test.rocr = prediction(predspam.RF2.test, test2$spam)
auc.RF2.test = performance(predspam.RF2.test.rocr, "auc")@y.values
auc.RF2.test
