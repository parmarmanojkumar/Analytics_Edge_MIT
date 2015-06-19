statedata = read.csv("statedata.csv")
str(statedata)
summary(statedata)
plot(statedata$x,statedata$y)
tapply(statedata$HS.Grad, statedata$state.region, mean)
which.max(tapply(statedata$HS.Grad, statedata$state.region, mean))
boxplot(Murder~state.region, data = statedata)
statedataNE = subset(statedata,statedata$state.region == "Northeast")
statedataNE[which.max(statedataNE$Murder),]
LEM = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + 
           Frost + Area, data = statedata)
summary(LEM)
plot(statedata$Income, statedata$Life.Exp)
LEM1 = lm(Life.Exp ~ Income + Murder + HS.Grad + 
                 Frost, data = statedata)
summary(LEM1)
LEM2 = lm(Life.Exp ~ Illiteracy + Murder + HS.Grad + 
                  Frost, data = statedata)
summary(LEM2)
LEM3 = lm(Life.Exp ~ Population + Murder + HS.Grad + 
                  Frost, data = statedata)
summary(LEM3)

predictLE = predict(LEM3)
statedata$predictLE = predictLE
statedata$state.name[which.min(statedata$predictLE)]
statedata$state.name[which.min(statedata$Life.Exp)]

statedata$state.name[which.max(statedata$predictLE)]
statedata$state.name[which.max(statedata$Life.Exp)]
statedata$lediff = abs(statedata$predictLE - statedata$Life.Exp)

statedata$state.name[which.min(statedata$lediff)]
statedata$state.name[which.max(statedata$lediff)]

