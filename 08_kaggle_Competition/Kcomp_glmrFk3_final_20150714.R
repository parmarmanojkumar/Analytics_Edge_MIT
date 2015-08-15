# KAGGLE COMPETITION - DEALING WITH THE TEXT DATA
options(warn=-1)
rm(list = ls())
library(randomForest)
library(flexclust)
library(ROCR)
library(rpart)
library(tm)
library(glm2)
library(caret)
tokenize_ngrams <- function(x, n=2) return(rownames(as.data.frame(unclass(textcnt(x,method="string",n=n)))))
convert_fac <- function (df.data, df.desc, levelsdata) {
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
  df = cbind(df.desc, df)
  df
}
as.numeric.factor <- function(x) {seq_along(levels(x))[x]}

convert_num <-function (df){
  df$Ipad.Condition  = as.numeric.factor(df$Ipad.Condition)
  df$biddable = as.numeric.factor(df$biddable)
  df$cellular = as.numeric.factor(df$cellular)
  df$carrier = as.numeric.factor(df$carrier)
  df$color = as.numeric.factor(df$color)
  df$storage = as.numeric.factor(df$storage)
  df$productline = as.numeric.factor(df$productline)
  df
}



# This script file is intended to help you deal with the text data provided in the competition data files

# If you haven't already, start by reading the data into R
# Make sure you have downloaded these files from the Kaggle website, and have navigated to the directory where you saved the files on your computer 

# We are adding in the argument stringsAsFactors=FALSE, since we have some text fields

eBayTrain = read.csv("eBayiPadTrain.csv", stringsAsFactors=FALSE)

eBayTest = read.csv("eBayiPadTest.csv", stringsAsFactors=FALSE)

# Now, let's load the "tm" package.

library(tm)

# Then create a corpus from the description variable. You can use other variables in the dataset for text analytics, but we will just show you how to use this particular variable. 
# Note that we are creating a corpus out of the training and testing data.

CorpusDescription = Corpus(VectorSource(c(eBayTrain$description, eBayTest$description)))

# You can go through all of the standard pre-processing steps like we did in Unit 5:

CorpusDescription = tm_map(CorpusDescription, content_transformer(tolower), lazy=TRUE)

# Remember this extra line is needed after running the tolower step:

CorpusDescription = tm_map(CorpusDescription, PlainTextDocument, lazy=TRUE)

CorpusDescription = tm_map(CorpusDescription, removePunctuation, lazy=TRUE)

CorpusDescription = tm_map(CorpusDescription, removeWords, stopwords("english"), lazy=TRUE)

CorpusDescription = tm_map(CorpusDescription, stemDocument, lazy=TRUE)

# Now we are ready to convert our corpus to a DocumentTermMatrix, remove sparse terms, and turn it into a data frame. 
# We selected one particular threshold to remove sparse terms, but remember that you can try different numbers!

dtm = DocumentTermMatrix(CorpusDescription)
dtm
sparse = removeSparseTerms(dtm, 0.995)
sparse

DescriptionWords = as.data.frame(as.matrix(sparse))
colnames(DescriptionWords) = paste0("D_",colnames(DescriptionWords))

# Let's make sure our variable names are okay for R:

colnames(DescriptionWords) = make.names(colnames(DescriptionWords))

# Now we need to split the observations back into the training set and testing set.
# To do this, we can use the head and tail functions in R. 
# The head function takes the first "n" rows of DescriptionWords (the first argument to the head function), where "n" is specified by the second argument to the head function. 
# So here we are taking the first nrow(eBayTrain) observations from DescriptionWords, and putting them in a new data frame called "DescriptionWordsTrain"

DescriptionWordsTrain = head(DescriptionWords, nrow(eBayTrain))

# The tail function takes the last "n" rows of DescriptionWords (the first argument to the tail function), where "n" is specified by the second argument to the tail function. 
# So here we are taking the last nrow(eBayTest) observations from DescriptionWords, and putting them in a new data frame called "DescriptionWordsTest"

DescriptionWordsTest = tail(DescriptionWords, nrow(eBayTest))

# Note that this split of DescriptionWords works to properly put the observations back into the training and testing sets, because of how we combined them together when we first made our corpus.

# Before building models, we want to add back the original variables from our datasets. We'll add back the dependent variable to the training set, and the WordCount variable to both datasets. You might want to add back more variables to use in your model - we'll leave this up to you!


DescriptionWordsTrain$WordCount = log2(rowSums(head(DescriptionWords, nrow(eBayTrain))) + 1)
DescriptionWordsTest$WordCount = log2(rowSums(tail(DescriptionWords, nrow(eBayTest))) + 1)

levelsdata = levels(as.factor(rbind(eBayTrain$productline, eBayTest$productline)))


Train = convert_fac(eBayTrain,DescriptionWordsTrain, levelsdata)
Test = convert_fac(eBayTest,DescriptionWordsTest, levelsdata)

Train$bidStartPrice = (as.numeric(Train$biddable)-1) * log(Train$startprice)
Test$bidStartPrice = (as.numeric(Test$biddable)-1) * log(Test$startprice)

#Train$bidNocarrier = as.numeric((Train$carrier == "None") &  (Train$biddable == 1))
#Test$bidNocarrier = as.numeric((Test$carrier == "None") &  (Test$biddable == 1))
#Train$bidNocellular = as.numeric((Train$cellular == 0) &  (Train$biddable == 1))
#Test$bidNocellular = as.numeric((Test$cellular == 0) &  (Test$biddable == 1))
#is.na(DescriptionWordsTrain) <- sapply(DescriptionWordsTrain, is.infinite)
#is.na(DescriptionWordsTest) <- sapply(DescriptionWordsTest, is.infinite)
#DescriptionWordsTrain[is.na(DescriptionWordsTrain)] <- 0
#DescriptionWordsTest[is.na(DescriptionWordsTest)] <- 0

# Remember that you can always look at the structure of these data frames to understand what we have created


KTrain = convert_num(Train)
KTrain$sold = NULL
KTrain$UniqueID = NULL
KTest = convert_num(Test)
KTest$UniqueID = NULL
preproc = preProcess(KTrain)
normTrain = predict(preproc, KTrain)
normTest = predict(preproc, KTest)
colMeans(normTrain)
colMeans(normTest)
#SCREEPlot to decide size
set.seed(47)
NumClusters = seq(2,10,1)
SumWithinss = sapply(NumClusters, function(x) sum(kmeans(KTrain, centers=x, iter.max=1000)$withinss))
plot(NumClusters, SumWithinss, type="b")
k = 3
set.seed(47)
km = kmeans(normTrain, centers = k)
km$size

km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)
table(clusterTrain)

Train1 = subset(Train, clusterTrain == 1)
Train2 = subset(Train, clusterTrain == 2)
Train3 = subset(Train, clusterTrain == 3)
#Train4 = subset(Train, clusterTrain == 4)

Test1 = subset(Test, clusterTest == 1)
Test2 = subset(Test, clusterTest == 2)
Test3 = subset(Test, clusterTest == 3)
#Test4 = subset(Test, clusterTest == 4)


#Now build specific logistic model
Trainglm1 = glm(sold ~ . - UniqueID - WordCount, data=Train1, family=quasibinomial)
set.seed(47)
TrainRF1 = randomForest(sold ~ . - UniqueID- WordCount , data=Train1)
Trainglm2 = glm(sold ~ . - UniqueID - WordCount  , data=Train2, family=binomial)
set.seed(47)
TrainRF2 = randomForest(sold ~ . - UniqueID - WordCount , data=Train2, nodesize = 7)
Trainglm3 = glm(sold ~ . - UniqueID , data=Train3, family=binomial)
set.seed(47)
TrainRF3 = randomForest(sold ~ . - UniqueID, data=Train3, nodesize = 11, do.trace = 5)
#Trainglm4 = glm(sold ~ . - UniqueID , data=Train4, family=binomial)

#predict data
PredTest1 = predict(TrainRF1, newdata=Test1, type="response")
PredTest2 = predict(Trainglm2, newdata=Test2, type="response")
PredTest3 = predict(Trainglm3, newdata=Test3, type="response")
PredTest1.RF = predict(TrainRF1, newdata=Test1, type="response")
PredTest2.RF = predict(TrainRF2, newdata=Test2, type="response")
PredTest3.RF = predict(TrainRF3, newdata=Test3, type="response")
#PredTest4 = predict(Trainglm4, newdata=Test4, type="response")
PredTrain1 = predict(TrainRF1, type="response")
PredTrain2 = predict(Trainglm2, type="response")
PredTrain3 = predict(Trainglm3, type="response")
PredTrain1.RF = predict(TrainRF1, type="response")
PredTrain2.RF = predict(TrainRF2, type="response")
PredTrain3.RF = predict(TrainRF3, type="response")
#PredTrain4 = predict(Trainglm4, type="response")
Test1$PredTest = PredTest1
Test2$PredTest = PredTest2
Test3$PredTest = PredTest3
Test1$PredTest.RF = PredTest1.RF
Test2$PredTest.RF = PredTest2.RF
Test3$PredTest.RF = PredTest3.RF
AllTestPred  = rbind(Test1,Test2 )
AllTestPred  = rbind(AllTestPred,Test3)
colnames(AllTestPred) = make.names(colnames(AllTestPred))
AllTestPred = AllTestPred[order(AllTestPred$UniqueID),] 

AlloutcomesTrain = c(Train1$sold, Train2$sold,Train3$sold )
AllPredictionsTrain = c(PredTrain1,PredTrain2,PredTrain3)
AllPredictionsTrain.RF = c(PredTrain1,PredTrain2.RF,PredTrain3.RF)

t = 0.5
a = table(AlloutcomesTrain, AllPredictionsTrain>= t)
accuracy.Trainglmk3 = (a[1] + a[4])/ sum(a)
print(accuracy.Trainglmk3)
a = table(AlloutcomesTrain, AllPredictionsTrain.RF>= t)
accuracy.TrainRFk3 = (a[1] + a[4])/ sum(a)
print(accuracy.TrainRFk3 )

rocr.Trainglmk3 = prediction(AllPredictionsTrain,AlloutcomesTrain)
auc.Trainglmk3 = performance(rocr.Trainglmk3, "auc")@y.values
print(auc.Trainglmk3)

rocr.TrainRFk3 = prediction(AllPredictionsTrain.RF,AlloutcomesTrain)
auc.TrainRFk3 = performance(rocr.TrainRFk3, "auc")@y.values
print(auc.TrainRFk3)

ROCRperf.Trainglmk3 = performance(rocr.Trainglmk3, "tpr", "fpr")
plot(ROCRperf.Trainglmk3)
# Now we can prepare our submission file for Kaggle:
hist(AllPredictionsTrain, breaks = 20)
hist(AllTestPred$PredTest, breaks = 20)
plot(ecdf(AllPredictionsTrain))
plot(ecdf(AllTestPred$PredTest))
#MySubmission = data.frame(UniqueID = AllTestPred$UniqueID, Probability1 = AllTestPred$PredTest)

#write.csv(MySubmission, "Sub_glmk3_final_20150714.csv", row.names=FALSE)

#MySubmission = data.frame(UniqueID = AllTestPred$UniqueID, Probability1 = AllTestPred$PredTest.RF)

#write.csv(MySubmission, "Sub_RFk3_final_20150714.csv", row.names=FALSE)


options(warn=0)

# You should upload the submission "SubmissionDescriptionLog.csv" on the Kaggle website to use this as a submission to the competition

# This script file was just designed to help you get started - to do well in the competition, you will need to build better models!
