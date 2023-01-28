#CL_4_SL

#Logistic Regression----
library(ISLR)
attach(Smarket)
plot(Volume)

glm_fit=glm(Direction~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket ,family=binomial)
summary(glm_fit)
coef(glm_fit)

glm_probs=predict(glm_fit,type="response") 
#this gives the prediction for direction of the market 
#if no new data is supplied it will be applied on the training data 

constrasts(Direction) #this will give us the coding of the varibles 

#now to predict using logistic regression we will create a new vector
glm_pred=rep("Down",1250)
glm_pred[glm_probs >.5]="Up"

table(glm_pred,Direction)
#use table to create a confusion matrix and determine how mnay values 
#were misclassified

mean(glm_pred==Direction) 
# this wil give what proportion of the values was correctly calssified




#Linear Discriminant Analysis (LDA)----

library(MASS)#data and lda function 
train= Year<2005
Smarket_2005= Smarket[!train,]
lda_fit= lda(Direction~Lag1+Lag2,data=Smarket,subset=train) # we can also do subset=(Year<2005)
lda_fit
#prior probabilites indicates the π_1 and π_2
plot(lda_fit)

lda_pred=predict(lda_fit,Smarket_2005)
names(lda_pred)

lda_class=lda_pred$class
Direction_2005=Direction[!train]
table(lda_class,Direction_2005)
#this is the confunsion table 
mean(lda_class==Direction_2005)

#setting posterior probability to 50%
sum(lda_pred$posterior[,1]>=.5)
#Quadratic Discriminant Analysis----
qda_fit= qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda_fit

qda_class=predict (qda_fit,Smarket_2005)$class
table(qda_class,Direction_2005)
mean(qda_class==Direction_2005)

#K-nearest neighbours (KNN)----
library(class)
train_X=cbind(Lag1,Lag2)[train,]
test_X=cbind(Lag1,Lag2)[!train,]
train_direction=Direction[train]

set.seed(1)
knn_pred=knn(train_X,test_X,train_direction,k=1)
table(knn_pred,Direction_2005)
mean(knn_pred==Direction_2005)

