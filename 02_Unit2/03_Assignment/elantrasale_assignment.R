Elantra = read.csv("elantra.csv")
str(Elantra)
ElantraTrain = subset(Elantra, Year <= 2012)
ElantraTest = subset(Elantra, Year > 2012)
summary(ElantraTrain)
summary(ElantraTest)
Elantralm = lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all, 
         data = ElantraTrain)
summary(Elantralm)
ElantralmMonth = lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + Month, 
               data = ElantraTrain)
summary(ElantralmMonth)
110.69 * 2
110.69 * 4
ElantraTrain$Month.F = as.factor(ElantraTrain$Month)
ElantraTest$Month.F = as.factor(ElantraTest$Month)
str(ElantraTrain)
ElantralmMonth.F = lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + Month.F, 
                    data = ElantraTrain)
summary(ElantralmMonth.F)

cor(ElantraTrain$CPI_energy,ElantraTrain$Month)
cor(ElantraTrain$CPI_energy,ElantraTrain$Unemployment)
cor(ElantraTrain$CPI_energy,ElantraTrain$Queries)
cor(ElantraTrain$CPI_energy,ElantraTrain$CPI_all)
cor(ElantraTrain[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])
cor(ElantraTrain[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])

ElantralmMonthRed = lm(ElantraSales ~ Unemployment + CPI_energy + CPI_all + Month.F, 
                      data = ElantraTrain)
summary(ElantralmMonthRed)

predictSale = predict(ElantralmMonthRed, newdata = ElantraTest)
SSE = sum((predictSale - ElantraTest$ElantraSales)^2)
SSE
baselm = mean(ElantraTrain$ElantraSales)
baselm
SST = sum((baselm - ElantraTest$ElantraSales)^2)
SST
R2 = 1- (SSE/SST)
R2
ElantraTest$err = (abs(predictSale - ElantraTest$ElantraSales))
maxabserr = max(ElantraTest$err)
ElantraTest[which.max(ElantraTest$err),]
