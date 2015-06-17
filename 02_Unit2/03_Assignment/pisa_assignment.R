pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")
str(pisaTrain)
summary(pisaTrain)
str(pisaTest)
summary(pisaTest)
tapply(pisaTrain$readingScore,pisaTrain$male,mean)
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
str(pisaTrain)
summary(pisaTrain)
str(pisaTest)
summary(pisaTest)
pisaTest$raceeth = relevel(pisaTest$raceeth,"White")
pisaTrain$raceeth = relevel(pisaTrain$raceeth,"White")

#build model
lmscore = lm(readingScore ~ ., data = pisaTrain)
summary(lmscore)
SSE= sum(lmscore$residuals ^2)
RMSE = sqrt(SSE/nrow(pisaTrain))
RMSE = sqrt(mean(lmScore$residuals^2))


#test model
predReadingScore = predict(lmscore,newdata = pisaTest)
summary(predReadingScore)
SSE_M = sum((predReadingScore - pisaTest$readingScore)^2, na.rm = T)
RMSE_M = sqrt(SSE_M/(nrow(pisaTest) - sum(is.na(predReadingScore))))
RMSE_M

basePredict = mean(pisaTrain$readingScore)
SST_M = sum((pisaTest$readingScore - basePredict)^2)
SST_M= sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
SST_M = sum((basePredict - pisaTest$readingScore)^2, na.rm = T)
baseline = mean(pisaTrain$readingScore)
sum((baseline-pisaTest$readingScore)^2)

R2 = 1- SSE_M / SST_M 
R2
