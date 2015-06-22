#problem1
parole = read.csv("parole.csv")
str(parole)
summary(parole)
sum(parole$violator)
a = names(parole)
parole$state = factor(parole$state)
parole$crime = factor(parole$crime)
str(parole)
summary(parole)

#Problem3
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

#problem 4
