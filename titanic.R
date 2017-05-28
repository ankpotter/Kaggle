#import required packages
library(caret)
library(randomForest)
library(fields)
library(e1071)

#set working directory
setwd('D:/Kaggle/Titanic')

#import data
trainSet <- read.table("train.csv", sep = ",", header = TRUE)
testSet <- read.table("test.csv", sep = ",", header = TRUE)

#get a summary of data
summary(trainSet)
#age has NA's but no other variable does

table(trainSet[,c("Survived", "Pclass")])
#class seems like it is influencial
table(trainSet$Survived,trainSet$SibSp)
table(trainSet$Survived,trainSet$Parch)
bplot.xy(trainSet$Survived, trainSet$Age)
#age doesn't seem to make difference
bplot.xy(trainSet$Survived, trainSet$Fare)
#the plots are vastly different for survived and died people

trainSet$Survived <- factor(trainSet$Survived)
set.seed(42)

# Train the model using a "random forest" algorithm
model <- train(Survived ~ Pclass + Sex + SibSp + Embarked + Parch + Fare,data = trainSet,method = "rf",trControl = trainControl(method = "cv",number = 5))

#model summary
model

#predict values for test set
testSet$Survived <- predict(model, newdata = testSet)
#gives error

summary(testSet)

#fare has an NA value, so we replace it by the average fare value, other methods could also be used like median, mode, zero etc.
testSet$Fare <- ifelse(is.na(testSet$Fare), mean(testSet$Fare, na.rm = TRUE), testSet$Fare)

#predict for test set
testSet$Survived <- predict(model, newdata = testSet)
