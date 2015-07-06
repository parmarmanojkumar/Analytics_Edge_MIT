rm(list = ls())

#problem1
trials = read.csv("clinical_trial.csv", stringsAsFactor = F)
str(trials)
summary(trials)
max(nchar(trials$abstract))
sum(nchar(trials$abstract) == 0)
trials$title[which.min(nchar(trials$title))]

#problem2
library(tm)
corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))
corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)
dtmTitle = removeSparseTerms(dtmTitle, .95)
dtmAbstract = removeSparseTerms(dtmAbstract, .95)
dtmTitle
dtmAbstract
dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))
which.max(colSums(dtmAbstract))
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
str(dtmTitle)
str(dtmAbstract)
#Problem3
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial
ncol(dtm)
library(caTools)
set.seed(144)
spl = sample.split(dtm$trial, .7)
train = subset(dtm, spl ==T)
test = subset(dtm, spl == F)
max(prop.table(table(test$trial)))
library(rpart)
library(rpart.plot)
trialCART = rpart(trial ~ ., data = train, method = "class")
prp(trialCART)
predCART.train = predict(trialCART)
predCART.train = predCART.train[,2]
max(predCART.train)

t = 0.5
a = table(train$trial, predCART.train >= t)
accuracy.train = (a[1] + a[4])/ sum(a)
sensitivity.train = a[4]/(a[2] + a[4])
specificity.train = a[1]/(a[1] + a[3])

#problem 4
predCART = predict(trialCART, newdata = test)
predCART = predCART[,2]
max(predCART)

a = table(test$trial, predCART >= t)
accuracy.test = (a[1] + a[4])/ sum(a)
sensitivity.test = a[4]/(a[2] + a[4])
specificity.test = a[1]/(a[1] + a[3])

library(ROCR)
predCART.ROCR = prediction(predCART, test$trial)
auc = performance(predCART.ROCR, "auc")@y.values
auc

#problem5