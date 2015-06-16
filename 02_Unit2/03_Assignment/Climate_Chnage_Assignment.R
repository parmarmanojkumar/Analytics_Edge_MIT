#set workspace correctly
ClimateAll = read.csv("climate_change.csv")
summary(ClimateAll)
#generate training set
ClimTrain = subset(ClimateAll, Year <= 2006)
summary(ClimTrain)
#generate testing set
ClimTest = subset(ClimateAll, Year > 2006)
summary(ClimTest)

#Next, build a linear regression model to predict the dependent variable Temp, 
#using MEI, CO2, CH4, N2O, CFC.11, CFC.12, TSI, and Aerosols as independent variables
#(Year and Month should NOT be used in the model). 
#Use the training set to build the model
ClimModel = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols,
               data = ClimTrain)
summary(ClimModel)
cor(ClimTrain)


#Given that the correlations are so high, let us focus on the N2O variable and 
#build a model with only MEI, TSI, Aerosols and N2O as independent variables. 
#Remember to use the training set to build the model.
ClimModelRed = lm(Temp ~ MEI + N2O + TSI + Aerosols, data = ClimTrain)
summary(ClimModelRed)

#Step function to generate better model using AIC method
ClimModelImp = step(ClimModel)
summary(ClimModelImp)

#Test over data
ClimPredict = predict(ClimModelImp, newdata = ClimTest)
summary(ClimPredict)

#Compute R^2
SSE = sum((ClimTest$Temp - ClimPredict)^2)
SST = sum((ClimTest$Temp - mean(ClimTrain$Temp))^2)
1 - SSE/SST
