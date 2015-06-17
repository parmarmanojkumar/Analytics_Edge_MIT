FluTrain = read.csv("FluTrain.csv")
FluTest = read.csv("FluTest.csv")
str(FluTrain)
summary(FluTrain)
str(FluTest)
summary(FluTest)
FluTrain[which.max(FluTrain$ILI),]
hist(FluTrain$ILI)
plot(log(FluTrain$ILI),FluTrain$Queries)

#build Model
FluTrend1 = lm(log(ILI) ~ Queries, data=FluTrain)
summary(FluTrend1)
#for single variable R2= corr(Indvar1, DepVar2) ^ 2

#Testing model
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
#determine 2012/03/11 week prediction
weekindex = which(FluTest$Week == "2012-03-11 - 2012-03-17")
predval= PredTest1[weekindex]
actval= FluTest$ILI[weekindex]
relerr= (actval - predval)/actval
SSE = sum((PredTest1 - FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))
RMSE

#Time series modeling of dependent variable
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(ILILag2)
plot(log(FluTrain$ILILag2), log(FluTrain$ILI))
FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)


#Testing Model
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
FluTest$ILILag2[1:2] = FluTrain$ILI[(nrow(FluTrain)-1) : nrow(FluTrain)]
FluTest$ILILag2[1:2]
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE = sum((PredTest2- FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))
RMSE
