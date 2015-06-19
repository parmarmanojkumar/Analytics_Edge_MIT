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
