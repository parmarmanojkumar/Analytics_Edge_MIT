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




eBayTrain = read.csv("eBayiPadTrain.csv", stringsAsFactors=FALSE)
eBayTest = read.csv("eBayiPadTest.csv", stringsAsFactors=FALSE)

splratio = 0.75
combinedata = 1
sparseratio = 0.98
node.size = 4
considersold = 0
runglm = 0

# rec_run <- function (combinedata,splratio,sparseratio,node.size, considersold,runglm,eBayTrain, eBayTest){
        options(warn=-1)  


        controldata = cbind(combinedata,splratio,sparseratio,node.size, considersold,runglm )
        
        CorpusDescription = Corpus(VectorSource(c(eBayTrain$description, eBayTest$description)))
        CorpusDescription = tm_map(CorpusDescription, content_transformer(tolower), lazy=TRUE)
        CorpusDescription = tm_map(CorpusDescription, PlainTextDocument, lazy=TRUE)
        CorpusDescription = tm_map(CorpusDescription, removePunctuation, lazy=TRUE)
        CorpusDescription = tm_map(CorpusDescription, removeWords, stopwords("english"), lazy=TRUE)
        CorpusDescription = tm_map(CorpusDescription, stemDocument, lazy=TRUE)
        dtm = DocumentTermMatrix(CorpusDescription)
        dtm
        
        sparse = removeSparseTerms(dtm, sparse = sparseratio )
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
        
        #AUC 0.9489 & 0.8627 ; cor with test 0.9950 & 0.9190
        #DescriptionWordsTrain$WordCount = log2(rowSums(head(DescriptionWords, nrow(eBayTrain))) + 2)
        #DescriptionWordsTest$WordCount = log2(rowSums(tail(DescriptionWords, nrow(eBayTest))) + 2)
        
        #AUC 0.8930 & 0.8631 ; cor with test 0.9949 & 0.9413
        #DescriptionWordsTrain$WordCount = log2(rowSums(head(DescriptionWords, nrow(eBayTrain))) + 1)
        #DescriptionWordsTest$WordCount = log2(rowSums(tail(DescriptionWords, nrow(eBayTest))) + 1)
        
        #AUC 0.9472 & 0.8600 ; cor with test 0.9939 & 0.9096
        DescriptionWordsTrain$WordCount = rowSums(head(DescriptionWords, nrow(eBayTrain))) 
        DescriptionWordsTest$WordCount = rowSums(tail(DescriptionWords, nrow(eBayTest))) 
        
        levelsdata = levels(as.factor(rbind(eBayTrain$productline, eBayTest$productline)))
        
        
        
        Train = convert_fac(eBayTrain,DescriptionWordsTrain, levelsdata,switch1 = combinedata )
        Test = convert_fac(eBayTest,DescriptionWordsTest, levelsdata,switch1 = combinedata)
        
        #print(combinedata)
        #Train$bidStartPrice = (as.numeric(Train$biddable)-1) * log(Train$startprice)
        #Test$bidStartPrice = (as.numeric(Test$biddable)-1) * log(Test$startprice)
        
        #Train$bidNocarrier = as.numeric((Train$carrier == "None") &  (Train$biddable == 1))
        #Test$bidNocarrier = as.numeric((Test$carrier == "None") &  (Test$biddable == 1))
        #Train$bidNocellular = as.numeric((Train$cellular == 0) &  (Train$biddable == 1))
        #Test$bidNocellular = as.numeric((Test$cellular == 0) &  (Test$biddable == 1))
        #is.na(DescriptionWordsTrain) <- sapply(DescriptionWordsTrain, is.infinite)
        #is.na(DescriptionWordsTest) <- sapply(DescriptionWordsTest, is.infinite)
        #DescriptionWordsTrain[is.na(DescriptionWordsTrain)] <- 0
        #DescriptionWordsTest[is.na(DescriptionWordsTest)] <- 0
        
        # Remember that you can always look at the structure of these data frames to understand what we have created
        Train.Origin = Train
        Train.Origin$WordCount = DescriptionWordsTrain$WordCount
        Test.Final = Test
        Test.Final$WordCount = DescriptionWordsTest$WordCount 
        set.seed(155)
        
        splt = sample.split(Train.Origin$sold , SplitRatio = splratio)
        #print(splratio)
        #rm(Train, Test)
        Train = subset(Train.Origin, splt == T)
        Test = subset(Train.Origin, splt == F)
        KTrain = convert_num(Train)
        if (considersold == 0){
                KTrain$sold = NULL
        }
#         KTrain$startprice = log(KTrain$startprice + 1)
        KTrain$UniqueID = NULL
        KTrain$carrier = NULL
        KTrain$color = NULL
        #KTrain$storage = NULL
        KTrain.Origin = convert_num(Train.Origin)
        if (considersold == 0){
                KTrain.Origin$sold = NULL
        }
#         KTrain.Origin$startprice = log(KTrain.Origin$startprice + 1)
        KTrain.Origin$UniqueID = NULL
        KTrain.Origin$carrier = NULL
        KTrain.Origin$color = NULL
        #KTrain$storage = NULL
        KTest = convert_num(Test)
        KTest$UniqueID = NULL
#         KTest$startprice = log(KTest$startprice + 1)
        KTest$carrier = NULL
        KTest$color = NULL
        KTest$sold = mean(Train.Origin$sold)
        #KTest$storage = NULL
        #KTest$sold = NULL
        if (considersold == 1){
                KTest$sold = as.numeric(rnorm(n = nrow(KTest), mean = mean(Train.Origin$sold), sd = sd(Train.Origin$sold) ) > mean(Train.Origin$sold))
                KTest$sold = mean(Train.Origin$sold)
        }else{
                KTest$sold = NULL
        }
        Test_final = read.csv("Test_Final.csv")
        Test_final$sold = as.numeric(Test_final$Probability1 > 0.5)
        KTest.Final = convert_num(Test.Final)
        KTest.Final$UniqueID = NULL
#         KTest.Final$startprice = log(KTest.Final$startprice + 1)
        KTest.Final$carrier = NULL
        KTest.Final$color = NULL
        #KTest.Final$storage = NULL
        if (considersold == 1){
                KTest.Final$sold = mean(Train.Origin$sold)
        }else{
                KTest.Final$sold = NULL
        }
#         if (considersold != 0){
#                 KTest.Final$sold =as.numeric(rnorm(n = 798, mean = mean(Train.Origin$sold), sd = sd(Train.Origin$sold) ) > 5)
#         }
        preproc = preProcess(KTrain)
        normTrain = predict(preproc, KTrain)
        normTrain.Origin = predict(preproc, KTrain.Origin)
        normTest = predict(preproc, KTest)
        normTest.Final = predict(preproc, KTest.Final)
        colMeans(normTrain)
        colMeans(normTest)
        #SCREEPlot to decide size
        set.seed(2)
        NumClusters = seq(2,10,1)
        SumWithinss = sapply(NumClusters, function(x) sum(kmeans(KTrain, centers=x, iter.max=1000)$withinss))
        plot(NumClusters, SumWithinss, type="b")
        k = 4
        set.seed(39)
        km = kmeans(normTrain, centers = k,nstart = 8)
        km$size
        
        km.kcca = as.kcca(km, normTrain)
        clusterTrain = predict(km.kcca)
         
        clusterTest = predict(km.kcca, newdata=normTest)
        clusterTest.Final = predict(km.kcca, newdata = normTest.Final) 
        clusterTrain.Origin = predict(km.kcca, newdata= normTrain.Origin)
        print(prop.table(table(clusterTest)))
        
        print(prop.table(table(clusterTrain)))
#         pause
        Train.Origin = convert_fac(eBayTrain,DescriptionWordsTrain, levelsdata,1)
        Train.Origin$WordCount = DescriptionWordsTrain$WordCount
        
        Train = subset(Train.Origin, splt == T)
        Test = subset(Train.Origin, splt == F)
        Test.Final = convert_fac(eBayTest,DescriptionWordsTest, levelsdata,1)
        Test.Final$WordCount = DescriptionWordsTest$WordCount 
        
        
        Train1 = subset(Train.Origin, clusterTrain.Origin  == 1)
        Train2 = subset(Train.Origin, clusterTrain.Origin  == 2)
        Train3 = subset(Train.Origin, clusterTrain.Origin  == 3)
        Train4 = subset(Train.Origin, clusterTrain.Origin  == 4)
        #Train4 = subset(Train, clusterTrain == 4)
        #Train5 = subset(Train, clusterTrain == 5)
        #Train6 = subset(Train, clusterTrain == 6)
        
        
        
        Test1 = subset(Test, clusterTest == 1)
        Test2 = subset(Test, clusterTest == 2)
        Test3 = subset(Test, clusterTest == 3)
        Test4 = subset(Test, clusterTest == 4)
        #Test5 = subset(Test, clusterTest == 5)
        #Test6 = subset(Test, clusterTest == 6)
        
        Test.Final1 = subset(Test.Final, clusterTest.Final == 1)
        Test.Final2 = subset(Test.Final, clusterTest.Final == 2)
        Test.Final3 = subset(Test.Final, clusterTest.Final == 3)
        Test.Final4 = subset(Test.Final, clusterTest.Final == 4)
        # Test.Final1 = subset(Test.Final, clusterTest == 1)
        # Test.Final2 = subset(Test.Final, clusterTest == 2)
        # Test.Final3 = subset(Test.Final, clusterTest == 3)
        # Test.Final4 = subset(Test.Final, clusterTest == 4)
        #Test.Final5 = subset(Test.Final, clusterTest == 5)
        #Test.Final6 = subset(Test.Final, clusterTest == 6)
        
        #Now build specific logistic model
        #Trainglm1 = glm(sold ~ . - UniqueID , data=Train1, family=quasibinomial)
        
        set.seed(47)
        TrainRF1 = randomForest(sold ~ . - UniqueID , data=Train1, nodesize = node.size)
        set.seed(47)
        TrainRF2 = randomForest(sold ~ . - UniqueID , data=Train2, nodesize = node.size)
        set.seed(47)
        TrainRF3 = randomForest(sold ~ . - UniqueID , data=Train3, nodesize = node.size)
        set.seed(47)
        TrainRF4 = randomForest(sold ~ . - UniqueID , data=Train4, nodesize = node.size)
        
        if(runglm == 1){
        Trainglm1 = glm(sold ~ . - UniqueID , data=Train1, family ="binomial")
        Trainglm2 = glm(sold ~ . - UniqueID  , data=Train2, family ="binomial")
        Trainglm3 = glm(sold ~ . - UniqueID, data=Train3, family ="binomial")
        Trainglm4 = glm(sold ~ . - UniqueID , data=Train4, family ="binomial")
        }
        PredTest1.RF = predict(TrainRF1, newdata=Test1, type="response")
        PredTest2.RF = predict(TrainRF2, newdata=Test2, type="response")
        PredTest3.RF = predict(TrainRF3, newdata=Test3, type="response")
        PredTest4.RF = predict(TrainRF4, newdata=Test4, type="response")
        if(runglm == 1){
        PredTest1.glm = predict(Trainglm1, newdata=Test1, type="response")
        PredTest2.glm = predict(Trainglm2, newdata=Test2, type="response")
        PredTest3.glm = predict(Trainglm3, newdata=Test3, type="response")
        PredTest4.glm = predict(TrainRF4, newdata=Test4, type="response")
        }
        PredTest.Final1.RF = predict(TrainRF1, newdata=Test.Final1, type="response")
        PredTest.Final2.RF = predict(TrainRF2, newdata=Test.Final2, type="response")
        PredTest.Final3.RF = predict(TrainRF3, newdata=Test.Final3, type="response")
        PredTest.Final4.RF = predict(TrainRF4, newdata=Test.Final4, type="response")
        if(runglm == 1){
        PredTest.Final1.glm = predict(Trainglm1, newdata=Test.Final1, type="response")
        PredTest.Final2.glm = predict(Trainglm2, newdata=Test.Final2, type="response")
        PredTest.Final3.glm = predict(Trainglm3, newdata=Test.Final3, type="response")
        PredTest.Final4.glm = predict(Trainglm4, newdata=Test.Final4, type="response")
        }
        Test1$PredTest.RF = PredTest1.RF
        Test2$PredTest.RF = PredTest2.RF
        Test3$PredTest.RF = PredTest3.RF
        Test4$PredTest.RF = PredTest4.RF
        if(runglm == 1){
        Test1$PredTest.glm = PredTest1.glm
        Test2$PredTest.glm = PredTest2.glm
        Test3$PredTest.glm = PredTest3.glm
        Test4$PredTest.glm = PredTest4.glm
        }
        
        Test.Final1$PredTest.Final.RF = PredTest.Final1.RF
        Test.Final2$PredTest.Final.RF = PredTest.Final2.RF
        Test.Final3$PredTest.Final.RF = PredTest.Final3.RF
        Test.Final4$PredTest.Final.RF = PredTest.Final4.RF
        if(runglm == 1){
        Test.Final1$PredTest.Final.glm = PredTest.Final1.glm
        Test.Final2$PredTest.Final.glm = PredTest.Final2.glm
        Test.Final3$PredTest.Final.glm = PredTest.Final3.glm
        Test.Final4$PredTest.Final.glm = PredTest.Final4.glm
        }
        
        AllTestPred  = rbind(Test1,Test2 )
        AllTestPred  = rbind(AllTestPred,Test3)
        AllTestPred  = rbind(AllTestPred,Test4)

        
        AllTest.FinalPred  = rbind(Test.Final1,Test.Final2 )
        AllTest.FinalPred  = rbind(AllTest.FinalPred,Test.Final3)
        AllTest.FinalPred  = rbind(AllTest.FinalPred,Test.Final4)

        
        colnames(AllTest.FinalPred) = make.names(colnames(AllTest.FinalPred))
        AllTest.FinalPred = AllTest.FinalPred[order(AllTest.FinalPred$UniqueID),] 
        
        
        t = 0.5
        a = table(AllTestPred$sold, AllTestPred$PredTest.RF >= t)
        accuracy.Train.rFk4 = (a[1] + a[4])/ sum(a)
        controldata = cbind(controldata, accuracy.Train.rFk4)
        if(runglm == 1){
        a = table(AllTestPred$sold, AllTestPred$PredTest.glm >= t)
        accuracy.Train.glmk4 = (a[1] + a[4])/ sum(a)
        controldata = cbind(controldata, accuracy.Train.glmk4)
        }
        rocr.Train.rFk4 = prediction(AllTestPred$PredTest.RF,AllTestPred$sold )
        auc.Train.rFk4 = performance(rocr.Train.rFk4, "auc")@y.values
        controldata = cbind(controldata, auc.Train.rFk4)
        if(runglm == 1){
        rocr.Train.glmk4 = prediction(AllTestPred$PredTest.glm,AllTestPred$sold )
        auc.Train.glmk4 = performance(rocr.Train.glmk4, "auc")@y.values
        controldata = cbind(controldata, auc.Train.glmk4)
        }
        #ROCRperf.auc.Train.rFk4= performance(rocr.auc.Train.rFk4, "tpr", "fpr")
        #plot(ROCRperf.auc.Train.rFk4)

        MySubmission = data.frame(UniqueID = AllTest.FinalPred$UniqueID, Probability1 = AllTest.FinalPred$PredTest.Final.RF)
        write.csv(MySubmission, "Sub_rfk4_final_20150723.csv", row.names=FALSE)
        
        options(warn=0)
        print(controldata)

# }

# a = rec_run(combinedata,splratio,sparseratio,node.size, considersold,runglm,eBayTrain, eBayTest)

# print(a)



# You should upload the submission "SubmissionDescriptionLog.csv" on the Kaggle website to use this as a submission to the competition

# This script file was just designed to help you get started - to do well in the competition, you will need to build better models!
