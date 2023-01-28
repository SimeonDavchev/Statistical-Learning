##Preliminary notes in the dataset:

#Data Set 1: The features are independent and normally distributed within each target class, with variances either being 1 or 10. Are the (population) covariance matrices the same across the target classes?
#logistic
#Data Set 2: The log odds ratio is a linear function of the features. Are the features normally distributed?
#logistic
#Data Set 3: The log odds ratio is a highly nonlinear - not quadratic- function of the features.
#forest 
#Data Set 4: The sample covariance matrices are close to the population covariance matrices in each target class. The features are normally distributed within each target class. Are the (population) covariance matrices the same across the target classes?
#QDA
#Data Set 5: The population covariance matrices among features are not equal but are very close across target classes. The features are normally distributed within each target class. 
#logistic

#Iniliase some varibales 
rm(list = ls())
setwd("~/Documents/Github/Statistical-Learning/ass2 /")
answer_sheet=read.csv("Answer_Sheet.csv")

#Dataset 1----
library(ISLR)
dt1= read.csv("dataset1.csv")
log_fit1= glm(target~.,data=dt1,family=binomial)
log_pred1= predict(log_fit1,dt1)
answer_sheet[,2]= log_pred1

#Dataset 2----
dt2= read.csv("dataset2.csv")
log_fit2= glm(target~.,data=dt2,family=binomial)
log_pred2= predict(log_fit2,dt2)
answer_sheet[,3]= log_pred2

#Dataset 3----
library(randomForest)
library(car)
dt3= read.csv("dataset3.csv")
set.seed(2023)
rf_fit= randomForest(target~.,data=dt3)
rf_predict_logit= logit(predict(rf_fit,dt3))
answer_sheet[,4]= rf_predict_logit

#Dataset 4----
library(MASS)
dt4= read.csv("dataset4.csv")
qda_fit= qda(target~., data=dt4)
qda_predict= predict(qda_fit,dt4)
qda_predict_logit= logit(qda_predict$posterior[,2],adjust=0)
answer_sheet[,5]= qda_predict_logit

#Dataset 5----
dt5= read.csv("dataset5.csv")
log_fit5= glm(target~.,data=dt5,family=binomial)
log_pred5= predict(log_fit5,dt5)
answer_sheet[,6]= log_pred5

write.csv(answer_sheet,"~/Documents/Github/Statistical-Learning/ass2 /answers.csv")

