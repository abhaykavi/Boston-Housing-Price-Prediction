#Set working directory
setwd("Directory here")

#Dataframe
housing<-read.csv("BostonHousing.csv")

#Libraries
library(vtreat)
library(MASS)
library(ggplot2)
library(dplyr)
library(forecast)
library(caret)
library(ModelMetrics)
splitpercent <- round(nrow(housing)%*%.9)
set.seed(1234)
idx<-sample(1:nrow(housing),splitpercent)

trainSet<-housing[idx,]
testSet<-housing[-idx,]


fit<-lm(MEDV~ CRIM+CHAS+RM+0,trainSet)
fit

fit2<-lm(MEDV~CRIM+CHAS+RM,trainSet)
fit2

medpred<-predict(fit,trainSet)
head(medpred)
head(trainSet$MEDV)

newdata<-subset(housing,CHAS==0&CRIM<=0.1&RM>=5.5&RM<=6.5)
head(newdata)
newpred<-predict(fit,newdata)
head(newpred)
head(newdata$MEDV)
plot(newpred,newdata$MEDV)

head(housing)
cor(housing)
pairs(housing)
housing$TAX<-NULL
housing$INDUS<-NULL
housing$NOX<-NULL
testSet$TAX<-NULL
testSet$INDUS<-NULL
testSet$NOX<-NULL
trainSet$TAX<-NULL
trainSet$INDUS<-NULL
trainSet$NOX<-NULL

reg<-lm(MEDV ~.,data = trainSet)
reg
stepback<-step(reg,direction = "backward")
summary(stepback)

reg<-lm(MEDV ~1,data = trainSet)
reg

stepfor<-step(reg,direction = "forward")
summary(stepfor)

stepboth<-step(reg,direction = "both")
summary(stepboth)

regpredback<-predict(stepback,testSet)
forecast::accuracy(regpredback, testSet$MEDV)

regpredfor<-predict(stepfor,testSet)
forecast::accuracy(regpredfor, testSet$MEDV)

regpredboth<-predict(stepboth,testSet)
forecast::accuracy(regpredboth, testSet$MEDV)

#End