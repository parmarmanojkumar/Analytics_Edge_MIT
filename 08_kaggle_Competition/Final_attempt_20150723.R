# KAGGLE COMPETITION - DEALING WITH THE TEXT DATA
setwd("~/Documents/01_Courses/13_Analytics_edge_MIT/08_kaggle_Competition")
rm(list = ls())
library(randomForest)
library(flexclust)
library(ROCR)
library(rpart)
library(tm)
library(caret)
library(caTools)
library(rpart.plot)
library(reshape2)
library(SnowballC)
tokenize_ngrams <- function(x, n=2) return(rownames(as.data.frame(unclass(textcnt(x,method="string",n=n)))))
convert_fac <- function (df.data, df.desc, levelsdata, switch1= 0) {
        df = df.data
        df$description = NULL
        colnames(df)[3] <- "Ipad.Condition"
        df$Ipad.Condition  = as.factor(df$Ipad.Condition)
        df$biddable = as.factor(df$biddable)
        df$cellular = as.factor(df$cellular)
        df$carrier = as.factor(df$carrier)
        df$color = as.factor(df$color)
        df$storage = as.factor(df$storage)
        df$productline = factor(df$productline , levelsdata, labels = levelsdata)
        if (switch1 == 1){
                df = cbind(df.desc, df)
        }
        else{
                df$WordCount = df.desc$WordCount
        }
        df
}
as.numeric.factor <- function(x) {seq_along(levels(x))[x]}

convert_num <-function (df){
        df$Ipad.Condition  = as.numeric.factor(df$Ipad.Condition)
        df$biddable = NULL
        df$cellular = as.numeric.factor(df$cellular)
        df$carrier = as.numeric.factor(df$carrier)
        df$color = as.numeric.factor(df$color)
        df$storage = as.numeric.factor(df$storage)
        df$productline = as.numeric.factor(df$productline)
        df$UniqueID = NULL
        df$sold = NULL
        df
}

options(warn=-1)


eBayTrain = read.csv("eBayiPadTrain.csv", stringsAsFactors=FALSE)
eBayTest = read.csv("eBayiPadTest.csv", stringsAsFactors=FALSE)

combinedata = 1
sparseratio = 0.97
node.size = 5





controldata = cbind(combinedata,sparseratio,node.size)

CorpusDescription = Corpus(VectorSource(c(eBayTrain$description, eBayTest$description)))
CorpusDescription = tm_map(CorpusDescription, content_transformer(tolower), lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, PlainTextDocument, lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, removePunctuation, lazy = T)
CorpusDescription = tm_map(CorpusDescription, removeWords, c(stopwords("english"),"ipad","condition"), lazy = T)
CorpusDescription = tm_map(CorpusDescription, stemDocument, lazy = T )
dtm = DocumentTermMatrix(CorpusDescription)
dtm

set.seed(2)
sparse = removeSparseTerms(dtm, sparse = sparseratio )
sparse

DescriptionWords = as.data.frame(as.matrix(sparse))
colnames(DescriptionWords) = paste0("D_",colnames(DescriptionWords))


colnames(DescriptionWords) = make.names(colnames(DescriptionWords))

DescriptionWordsTrain = head(DescriptionWords, nrow(eBayTrain))

DescriptionWordsTest = tail(DescriptionWords, nrow(eBayTest))
DescriptionWordsTrainCloud = DescriptionWordsTrain
DescriptionWordsTestCloud = DescriptionWordsTest
wordcloud(colnames(DescriptionWordsTrainCloud), colSums(DescriptionWordsTrainCloud), scale=c(2, .25))
wordcloud(colnames(DescriptionWordsTestCloud), colSums(DescriptionWordsTestCloud), scale=c(2, .25))

DescriptionWordsTrain$WordCount = rowSums(head(DescriptionWords, nrow(eBayTrain))) 
DescriptionWordsTest$WordCount = rowSums(tail(DescriptionWords, nrow(eBayTest))) 

levelsdata = levels(as.factor(rbind(eBayTrain$productline, eBayTest$productline)))



Train = convert_fac(eBayTrain,DescriptionWordsTrain, levelsdata,switch1 = combinedata )
Test  = convert_fac(eBayTest,DescriptionWordsTest, levelsdata,switch1 = combinedata)

#split in B0 and B1
TrainB0 = subset(Train, Train$biddable == 0)
TrainB1 = subset(Train, Train$biddable == 1)
 
TestB0 = subset(Test, Test$biddable == 0)
TestB1 = subset(Test, Test$biddable == 1)

#convert Data
TrainB0.con = convert_num(TrainB0)
TrainB1.con = convert_num(TrainB1)
TestB0.con  = convert_num(TestB0)
TestB1.con  = convert_num(TestB1)
#Kmeans partition
#preprocess
preprocB0         = preProcess(TrainB0.con)
preprocB1         = preProcess(TrainB1.con)
normTrainB0       = predict(preprocB0,TrainB0.con)
normTrainB1       = predict(preprocB1,TrainB1.con)
normTestB0        = predict(preprocB0,TestB0.con)
normTestB1        = predict(preprocB1,TestB1.con)

#plot clustering error
NumClusters = seq(2,10,1)
set.seed(2)
SumWithinssB0 = sapply(NumClusters, function(x) sum(kmeans(normTrainB0, centers=x, iter.max=1000)$withinss))
set.seed(2)
SumWithinssB1 = sapply(NumClusters, function(x) sum(kmeans(normTrainB1, centers=x, iter.max=1000)$withinss))
set.seed(2)
SumWithinssTestB0 = sapply(NumClusters, function(x) sum(kmeans(normTestB0, centers=x, iter.max=1000)$withinss))
set.seed(2)
SumWithinssTestB1 = sapply(NumClusters, function(x) sum(kmeans(normTestB1, centers=x, iter.max=1000)$withinss))

df <- data.frame(NumClusters, SumWithinssB0, SumWithinssB1,SumWithinssTestB0,SumWithinssTestB1)
df2 <- melt(data = df, id.vars = "NumClusters")
SumWithin.gp = ggplot(data = df2, aes(x = NumClusters, y = value, colour = variable)) + geom_line()
remove(df,df2)
print(SumWithin.gp)

#decide about cluster 
B0.k = 4
B1.k = 4
set.seed(2)
kmB0 = kmeans(normTrainB0, centers = B0.k, iter.max=1000)
print(kmB0$size)
set.seed(2)
kmB1 = kmeans(normTrainB1, centers = B1.k,iter.max=1000)
print(kmB1$size)

#convert it into prediction model
kmB0.kcca  = as.kcca(kmB0, normTrainB0)
kmB1.kcca  = as.kcca(kmB1, normTrainB1)
CluTrainB0 = predict(kmB0.kcca)
CluTrainB1 = predict(kmB1.kcca)
CluTestB0  = predict(kmB0.kcca, newdata = normTestB0)
CluTestB1  = predict(kmB1.kcca, newdata = normTestB1)
print(prop.table(table(CluTrainB0)))
print(prop.table(table(CluTestB0)))
print(prop.table(table(CluTrainB1)))
print(prop.table(table(CluTestB1)))

#split data
TrainB01 = subset(TrainB0, CluTrainB0 == 1)
TrainB02 = subset(TrainB0, CluTrainB0 == 2)
TrainB03 = subset(TrainB0, CluTrainB0 == 3)
TrainB04 = subset(TrainB0, CluTrainB0 == 4)
TrainB11 = subset(TrainB1, CluTrainB1 == 1)
TrainB12 = subset(TrainB1, CluTrainB1 == 2)
TrainB13 = subset(TrainB1, CluTrainB1 == 3)
TrainB14 = subset(TrainB1, CluTrainB1 == 4)

TestB01  = subset(TestB0, CluTestB0 == 1)
TestB02  = subset(TestB0, CluTestB0 == 2)
TestB03  = subset(TestB0, CluTestB0 == 3)
TestB04  = subset(TestB0, CluTestB0 == 4)
TestB11  = subset(TestB1, CluTestB1 == 1)
TestB12  = subset(TestB1, CluTestB1 == 2)
TestB13  = subset(TestB1, CluTestB1 == 3)
TestB14  = subset(TestB1, CluTestB1 == 4)

Train_temp = convert_num(TrainB04)
Train_temp$sold = TrainB04$sold
#Train Model
set.seed(2); TrainRFB01 = randomForest(sold ~ . - UniqueID , data=TrainB01, nodesize = node.size, replace =  T, corr.bias = T, ntree = 0800); TrainRFB01
set.seed(2); TrainRFB02 = randomForest(sold ~ . - UniqueID , data=TrainB02, nodesize = node.size, replace =  F, corr.bias = T, ntree = 0400); TrainRFB02
set.seed(2); TrainRFB03 = randomForest(sold ~ . - UniqueID , data=TrainB03, nodesize = node.size, replace =  T, corr.bias = T, ntree = 1000); TrainRFB03
set.seed(2); TrainRFB04 = randomForest(sold ~ . - UniqueID , data=TrainB04, nodesize = node.size, replace =  T, corr.bias = T, ntree = 0050); TrainRFB04
set.seed(2); TraingmB04 = glm(sold ~ .   , data=Train_temp,family = binomial); summary(TraingmB04)


set.seed(2); TrainRFB11 = randomForest(sold ~ . - UniqueID , data=TrainB11, nodesize = node.size, replace =  F, corr.bias = T, ntree = 0500); TrainRFB11
set.seed(2); TrainRFB12 = randomForest(sold ~ . - UniqueID , data=TrainB12, nodesize = node.size, replace =  F, corr.bias = T, ntree = 0600); TrainRFB12
set.seed(2); TrainRFB13 = randomForest(sold ~ . - UniqueID , data=TrainB13, nodesize = node.size, replace =  T, corr.bias = T, ntree = 0700); TrainRFB13
set.seed(2); TrainRFB14 = randomForest(sold ~ . - UniqueID , data=TrainB14, nodesize = node.size, replace =  T, corr.bias = T, ntree = 1100); TrainRFB14

#predictdata
PredTestB01 = predict(TrainRFB01, newdata=TestB01, type="response")
PredTestB02 = predict(TrainRFB02, newdata=TestB02, type="response")
PredTestB03 = predict(TrainRFB03, newdata=TestB03, type="response")
PredTestB04 = predict(TrainRFB04, newdata=TestB04, type="response")
PredTestB04gm = predict(TraingmB04, newdata=convert_num(TestB04), type="response")

PredTestB11 = predict(TrainRFB11, newdata=TestB11, type="response")
PredTestB12 = predict(TrainRFB12, newdata=TestB12, type="response")
PredTestB13 = predict(TrainRFB13, newdata=TestB13, type="response")
PredTestB14 = predict(TrainRFB14, newdata=TestB14, type="response")

#patching predicted data
TestB01$PredTest = PredTestB01
TestB02$PredTest = PredTestB02
TestB03$PredTest = PredTestB03
TestB04$PredTest = PredTestB04

TestB11$PredTest = PredTestB11
TestB12$PredTest = PredTestB12
TestB13$PredTest = PredTestB13
TestB14$PredTest = PredTestB14

#combining data
TestPred = rbind(TestB01,TestB02,TestB03,TestB04,
                 TestB11,TestB12,TestB13,TestB14)
TestPred = TestPred[order(TestPred$UniqueID),]

#submission of data
MySubmission = data.frame(UniqueID = TestPred$UniqueID, Probability1 = TestPred$PredTest)
write.csv(MySubmission, "SplitB_44_rf_20150724.csv", row.names=FALSE)
options(warn=0)