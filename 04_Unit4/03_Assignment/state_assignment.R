rm(list = ls())
#Problem1
data(state)
statedata = data.frame(state.x77)
str(statedata)
summary(statedata)
linmodel = lm(Life.Exp ~., data = statedata)
summary(linmodel)
linmodel.SSE = sum(linmodel$residuals^2)
linmodel.SSE
linmodel2 = lm(Life.Exp ~Population + Murder + HS.Grad + Frost, data = statedata)
summary(linmodel2)
linmodel2.SSE = sum(linmodel2$residuals^2)
linmodel2.SSE
cor(statedata)
#Problem2
cartmodel = rpart(Life.Exp ~., data = statedata)
prp(cartmodel)
pred.LifeExp.cart = predict(cartmodel)
cartmodel.SSE = sum((statedata$Life.Exp - pred.LifeExp.cart)^2)

cartmodel2 = rpart(Life.Exp ~., data = statedata, minbucket = 5)
prp(cartmodel2)
pred.LifeExp.cart2 = predict(cartmodel2)
cartmodel2.SSE = sum((statedata$Life.Exp - pred.LifeExp.cart2)^2)

cartmodel3 = rpart(Life.Exp ~ Area, data = statedata, minbucket = 1)
prp(cartmodel3)
pred.LifeExp.cart3 = predict(cartmodel3)
cartmodel3.SSE = sum((statedata$Life.Exp - pred.LifeExp.cart3)^2)

#Problem3
library(caret)
set.seed(111)
cp.grid = expand.grid( .cp = seq(0.01,0.5,0.01))
tr.control = trainControl(method = "cv", number = 10)
tr = train(Life.Exp ~ ., data = statedata, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)
tr
cartmodel.cp = rpart(Life.Exp ~ . , data = statedata, cp = 0.12)
prp(cartmodel.cp)
pred.LifeExp.cart.cp = predict(cartmodel.cp)
cartmodel.cp.SSE = sum((statedata$Life.Exp - pred.LifeExp.cart.cp)^2)

set.seed(111)
tr.cp2 = train (Life.Exp ~ Area, data = statedata,method = "rpart", trControl = tr.control, tuneGrid = cp.grid)
tr.cp2
cartmodel.cp2 = rpart(Life.Exp ~ Area , data = statedata, cp = tr.cp2$bestTune)
cartmodel.cp2$cptable
prp(cartmodel.cp2)
pred.LifeExp.cart.cp2 = predict(cartmodel.cp2)
cartmodel.cp2.SSE = sum((statedata$Life.Exp - pred.LifeExp.cart.cp2)^2)
