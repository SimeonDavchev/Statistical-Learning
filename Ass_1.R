#Ass_1

#Initial Notes:----

#Dataset_1 All features are useful.
#Are the features (highly) correlated? No, so no pcr
#What can we learn from benchmark tuning parameters?
#both pcr and fwd use 3 para, so use ridge.

#Dataset_2 Only a few features are useful.  
#Is variable selection a good idea? 
#lasso or pcr 

#Dataset_3 All features are relevant for prediction,
#and their impacts are comparable. 
#Is variable selection a good idea?

#Dataset_4 A small group of regressors has relatively 
#large coefficients. Is variable selection a good idea?

#Dataset_5 A few latent factors drive all the features, 
#so the features are strongly correlated. 
#Is dimension reduction a good idea?


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

ridge_ans=predict(ridge_mod,s=1.519911083,newx=x[test,])
mean((ridge_ans-y_test)^2)
