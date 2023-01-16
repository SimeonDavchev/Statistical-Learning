#Ass_1

#Initial Notes:----

#Dataset_1 All features are useful.
#Are the features (highly) correlated? No, so no pcr
#What can we learn from benchmark tuning parameters?
#both pcr and fwd use 3 para, so use ridge.

#Dataset_2 Only a few features are useful.  
#Is variable selection a good idea? 
#lasso 

#Dataset_3 All features are relevant for prediction,
#and their impacts are comparable. so no lasso 
#Is variable selection a good idea? Ridge 

#Dataset_4 A small group of regressors has relatively 
#large coefficients. Is variable selection a good idea?
#forward 

#Dataset_5 A few latent factors drive all the features, 
#so the features are strongly correlated. 
#Is dimension reduction a good idea?
#pcr


#Dataset_1----

dt1=read.csv("dataset1.csv")
cor(dt1[,2:4]) 
#the highest corr is 0.12387498 between 2 and 3,
#so no, there is no significant correlation between the features

#First try ridge regression 
x=model.matrix(target~.,dt1)[,-1]
y=dt1$target
library(glmnet)
grid=10^seq(10,-2,length=100)
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y_test=y[test]
ridge_mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
cv_out=cv.glmnet(x[train,],y[train],aplha=0)
plot(cv_out)
bestlam=cv_out$lambda.min
bestlam #this gives us the lambda for which the cv error is the smallest
ridge_pred=predict(ridge_mod,s=bestlam,newx=x[test,])
mean((ridge_pred-y_test)^2) # this gives the test mse

ridge_ans=predict(ridge_mod,s=1.5199111,newx=x[test,])
mean((ridge_ans-y_test)^2)
#27.43525

lm_mod= lm(target~., data=dt1)
lm_pred= predict(lm_mod)
mean((lm_pred-y_test)^2)
#26.30288

#Dataset_2----
dt2=read.csv("dataset2.csv")
x=model.matrix(target~.,dt2)[,-1]
y=dt2$target
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y_test=y[test]
library(glmnet)
lasso_mod=glmnet(x[train,],y[train],alpha = 1)
plot(lasso_mod)
set.seed(1)
cv_out=cv.glmnet(x[train,],y[train],aplha=1)
plot(cv_out)
bestlam=cv_out$lambda.min
lasso_pred=predict(lasso_mod,s=bestlam,newx=x[test,])
mean((lasso_pred-y_test)^2)

#this is with his lambda 
lasso_pred=predict(lasso_mod,s=0.4977024
,newx=x[test,])
mean((lasso_pred-y_test)^2)

#Dataset_3----
dt3= read.csv("dataset3.csv")
library(glmnet)
x= model.matrix(target~., dt3)[,-1]
y=dt3$target
grid=10^seq(10,-2,length=100)
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y_test=y[test]
ridge_mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridgw_pred=predict(ridge_mod,s=6.1359073,newx = x[test,])
mean((ridge_pred-y_test)^2)
#16.42686

#Dataset_4----
dt4=read.csv("dataset4.csv")
library(leaps)
library(glmnet)
x= model.matrix(target~., dt4)[,-1]
y=dt4$target
regfit_fwd=regsubsets(target~., data=dt4,nvmax = 19,method="forward")
coef(regfit_fwd,id=6)

fwd_pred=predict(regfit_fwd,dt4[train,],id=6)


