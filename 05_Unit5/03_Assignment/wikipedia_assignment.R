rm(list = ls())
wiki = read.csv("wiki.csv", stringsAsFactor = F)
str(wiki)
wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)
library(tm)
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
#length(stopwords("english"))
dtmAdded
sparseAdded = removeSparseTerms(dtmAdded , 0.997)
sparseAdded
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A",colnames(wordsAdded))

corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R",colnames(wordsRemoved))
str(wordsRemoved)
wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal
library(caTools)
set.seed(123)
spl = sample.split(wikiWords$Vandal , 0.7)
train = subset(wikiWords, spl == T)
test = subset(wikiWords, spl == F)
accuracy.bm = max(prop.table(table(test$Vandal)))
library(rpart)
library(rpart.plot)
wikiCART = rpart(Vandal ~ . , data = train, method ="class")
predCART = predict(wikiCART, newdata = test, type ="class")
predCART
a = as.matrix(prop.table(table(test$Vandal, predCART)))
accuracy.CART =a[1,1] + a[2,2]
prp(wikiCART)
#Problem2
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)
wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)
wikiCART2 = rpart(Vandal ~ . , data = wikiTrain2, method ="class")
predCART2 = predict(wikiCART2, newdata = wikiTest2, type ="class")
predCART2
a = as.matrix(prop.table(table(test$Vandal, predCART2)))
accuracy.CART2 =a[1,1] + a[2,2]
prp(wikiCART2)
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)
wikiTrain3 = subset(wikiWords2, spl==TRUE)
wikiTest3 = subset(wikiWords2, spl==FALSE)
wikiCART3 = rpart(Vandal ~ . , data = wikiTrain3, method ="class")
predCART3 = predict(wikiCART3, newdata = wikiTest3, type ="class")
predCART3
a = as.matrix(prop.table(table(test$Vandal, predCART3)))
accuracy.CART3 =a[1,1] + a[2,2]
prp(wikiCART3)
#Problem3
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
wikiTrain4 = subset(wikiWords3, spl==TRUE)
wikiTest4 = subset(wikiWords3, spl==FALSE)
wikiCART4 = rpart(Vandal ~ . , data = wikiTrain4, method ="class")
predCART4 = predict(wikiCART4, newdata = wikiTest4, type ="class")
predCART4
a = as.matrix(prop.table(table(test$Vandal, predCART4)))
accuracy.CART4 =a[1,1] + a[2,2]
prp(wikiCART4)
