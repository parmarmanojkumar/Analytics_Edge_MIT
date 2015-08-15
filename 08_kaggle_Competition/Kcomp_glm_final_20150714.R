# KAGGLE COMPETITION - DEALING WITH THE TEXT DATA
rm(list = ls())
library(randomForest)

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

sparse = removeSparseTerms(dtm, 0.995)

DescriptionWords = as.data.frame(as.matrix(sparse))

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


DescriptionWordsTrain$WordCount = (rowSums(head(DescriptionWords, nrow(eBayTrain))))
DescriptionWordsTest$WordCount = (rowSums(tail(DescriptionWords, nrow(eBayTest))))

levelsdata = levels(as.factor(rbind(eBayTrain$productline, eBayTest$productline)))


Train = convert_fac(eBayTrain,DescriptionWordsTrain, levelsdata)
Test = convert_fac(eBayTest,DescriptionWordsTest, levelsdata)


#is.na(DescriptionWordsTrain) <- sapply(DescriptionWordsTrain, is.infinite)
#is.na(DescriptionWordsTest) <- sapply(DescriptionWordsTest, is.infinite)
#DescriptionWordsTrain[is.na(DescriptionWordsTrain)] <- 0
#DescriptionWordsTest[is.na(DescriptionWordsTest)] <- 0

# Remember that you can always look at the structure of these data frames to understand what we have created



# Now let's create a logistic regression model using all of the variables:

#DescriptionWordsLog = glm(sold ~ ., data=DescriptionWordsTrain, family=binomial)
Trainglm = glm(sold ~ . - UniqueID , data=Train, family=quasibinomial)
print(summary(Trainglm))
# And make predictions on our test set:

PredTest = predict(Trainglm, newdata=Test, type="response")
PredTrain = predict(Trainglm, type="response")

t = 0.5
a = table(Train$sold, PredTrain>= t)
accuracy.Trainglm = (a[1] + a[4])/ sum(a)
print(accuracy.Trainglm)

rocr.Trainglm = prediction(PredTrain,Train$sold)
auc.Trainglm = performance(rocr.Trainglm, "auc")@y.values
print(auc.Trainglm)
# Now we can prepare our submission file for Kaggle:

MySubmission = data.frame(UniqueID = eBayTest$UniqueID, Probability1 = PredTest)

write.csv(MySubmission, "Sub_glm_final_20150714.csv", row.names=FALSE)

# You should upload the submission "SubmissionDescriptionLog.csv" on the Kaggle website to use this as a submission to the competition

# This script file was just designed to help you get started - to do well in the competition, you will need to build better models!
