rm(list = ls())
train = read.csv("titanic_train.csv")
test = read.csv("titanic_test.csv")
library(ggplot2)
library(scales)
library(dplyr)
library(randomForest)
library(caTools)
str(train)
colnames(train)
#logistic model
model_1 = glm(Survived ~ Pclass+Age+Fare,data =train,family = binomial)#creating logistin function
summary(model_1)
model_2 = glm(Survived ~ Pclass+Age,data =train,family = binomial)
summary(model_2)
predictions = predict(model_2, type="response", newdata=test)
test$predictions= predictions
titanicpredDF = data.frame(test,predictions)#creating data and adding predictions in dataset
titanicpredDF$titanicprediction = titanicpredDF$predictions>0.5
predictions = predict(model_2, type="response", newdata=train)
train$predictions= predictions
titanicpredDF = data.frame(train,predictions)#creating data and adding predictions in dataset
titanicpredDF$titanicprediction = titanicpredDF$predictions>0.5

#confusion matrix -  categorize the predictions against the actual values

cf = table(titanicpredDF$Survived,titanicpredDF$titanicprediction)# above 0.5 will be TRUE and below will be FALSE as per prediction values

cf = as.data.frame.matrix(table(titanicpredDF$Survived,titanicpredDF$titanicprediction))# to check if the function is a data frame

View(cf)# view table of Cf
accuracy = (347+150)/(347+150+77+140)# evaluating the performance of the model 
truepositive = (347)/(347+150+77+140)
truenegative = (150)/(347+150+77+140)
Falsepositive = (140)/(347+150+77+140) # also called type1 error
Falsenegative = (77)/(347+150+77+140) # also called type2 error

#random forest
colnames(train)
set.seed(7)
model_3 = randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Ticket + Cabin + Sex, data = train)
