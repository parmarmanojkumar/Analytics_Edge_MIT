# Unit 3, Modeling the Expert


# Video 4

# Read in dataset
quality = read.csv("quality.csv")

# Look at structure
str(quality)

# Table outcome
table(quality$PoorCare)

# Baseline accuracy
98/131

# Install and load caTools package
install.packages("caTools")
library(caTools)

# Randomly split data
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
split

# Create training and testing sets
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)

# Logistic Regression Model
QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
summary(QualityLog)

# Make predictions on training set
predictTrain = predict(QualityLog, type="response")

# Analyze predictions
summary(predictTrain)
tapply(predictTrain, qualityTrain$PoorCare, mean)

#Quiz
QualityLogq = glm(PoorCare ~ StartedOnCombination + ProviderCount, 
                  data=qualityTrain, family=binomial)
summary(QualityLogq)

# Video 5

# Confusion matrix for threshold of 0.5
table(qualityTrain$PoorCare, predictTrain > 0.5)

# Sensitivity and specificity
10/25
70/74

# Confusion matrix for threshold of 0.7
table(qualityTrain$PoorCare, predictTrain > 0.7)

# Sensitivity and specificity
8/25
73/74

# Confusion matrix for threshold of 0.2
table(qualityTrain$PoorCare, predictTrain > 0.2)

# Sensitivity and specificity
16/25
54/74



# Video 6

# Install and load ROCR package
install.packages("ROCR")
library(ROCR)

# Prediction function
ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.2), text.adj=c(-0.2,1.7))

#Prediction
predictTest = predict(QualityLog, type = "response", newdata = qualityTest)
t = 0.2
a = table(qualityTest$PoorCare, predictTest > t)
accuracy = (a[1] + a[4])/ sum(a)
error.rate = (a[2] + a[3]) / sum(a)
sesnitivity = a[4]/(a[2] + a[4])
specificity = a[1]/(a[1] + a[3])
false.neg.error.rate = a[2]/ (a[2] + a[4])
false.pos.error.rate = a[3]/(a[1] + a[3])

t
a
accuracy 
error.rate 
sesnitivity
specificity
false.neg.error.rate
false.pos.error.rate

#ROCR
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest,"auc")@y.values)
auc
