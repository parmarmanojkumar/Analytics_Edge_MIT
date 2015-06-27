rm(list = ls())
baseball = read.csv("baseball.csv")
str(baseball)
summary(baseball)
nrow(table(baseball$Year))
baseball = subset(baseball, Playoffs == 1)

table(table(baseball$Year))
PlayoffTable = table(baseball$Year)
names(PlayoffTable)
PlayoffTable[c("1990", "2001")]
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]
baseball$NumCompetitors
table(baseball$NumCompetitors)
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
table(baseball$WorldSeries)

mod01 = glm(WorldSeries ~ Year , data = baseball , family = binomial)
mod02 = glm(WorldSeries ~ RS , data = baseball , family = binomial)
mod03 = glm(WorldSeries ~ RA , data = baseball , family = binomial)
mod04 = glm(WorldSeries ~ W , data = baseball , family = binomial)
mod05 = glm(WorldSeries ~ OBP , data = baseball , family = binomial)
mod06 = glm(WorldSeries ~ SLG , data = baseball , family = binomial)
mod07 = glm(WorldSeries ~ BA , data = baseball , family = binomial)
mod08 = glm(WorldSeries ~ RankSeason , data = baseball , family = binomial)
mod09 = glm(WorldSeries ~ OOBP , data = baseball , family = binomial)
mod10 = glm(WorldSeries ~ OSLG , data = baseball , family = binomial)
mod11 = glm(WorldSeries ~ NumCompetitors , data = baseball , family = binomial)
mod12 = glm(WorldSeries ~ League , data = baseball , family = binomial)

summary(mod01)
summary(mod02)
summary(mod03)
summary(mod04)
summary(mod05)
summary(mod06)
summary(mod07)
summary(mod08)
summary(mod09)
summary(mod10)
summary(mod11)
summary(mod12)

finalmod = glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors , data = baseball , family = binomial)
summary(finalmod)

a = cor(baseball[c("WorldSeries", "Year", "RA", "RankSeason" , "NumCompetitors")])
abs(a) > 0.8 & abs(a) < 1

mod13 = glm(WorldSeries ~ Year + RA , data = baseball , family = binomial)
mod14 = glm(WorldSeries ~ Year + RankSeason, data = baseball , family = binomial)
mod15 = glm(WorldSeries ~ Year + NumCompetitors, data = baseball , family = binomial)
mod16 = glm(WorldSeries ~ RA + RankSeason, data = baseball , family = binomial)
mod17 = glm(WorldSeries ~ RA + NumCompetitors, data = baseball , family = binomial)
mod18 = glm(WorldSeries ~ RankSeason + NumCompetitors, data = baseball , family = binomial)


library(ROCR)
pred.winner = predict(mod18, type = "response")
ROCRpredTest = prediction(pred.winner, baseball$WorldSeries)
auc = as.numeric(performance(ROCRpredTest,"auc")@y.values)
auc

aucfunc <- function(model){
        pred.winner = predict(model, type = "response")
        ROCRpredTest = prediction(pred.winner, baseball$WorldSeries)
        auc = as.numeric(performance(ROCRpredTest,"auc")@y.values)
        model$aic 
}
aucfunc(mod13)
aucfunc(mod14)
aucfunc(mod15)
aucfunc(mod16)
aucfunc(mod17)
aucfunc(mod18)
aucfunc(mod01)
aucfunc(mod03)
aucfunc(mod08)
aucfunc(mod11)

