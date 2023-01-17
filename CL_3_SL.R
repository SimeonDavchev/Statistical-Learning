##CL_3_SL

##Subset Selection Methods ---- 
#Best Subset Selection---- 
library(ISLR)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters))

Hitters=na.omit(Hitters) #remove the rows with NAs 
library(leaps)
regfit_full=regsubsets(Salary~.,Hitters) 
summary(regfit_full)
#we can use nvmax=19 to change the number of variables in the biggest model 
#from the deafault 8 to ...

regfit_fill=regsubsets(Salary~.,data=Hitters,nvmax=19)
reg_summary=summary(regfit_fill)
reg_summary$rsq #This gives R^2
#other possible: adjr2,rss,bic,cp



#Forward and Backwards Stepwise Selection----
regfit_forward= regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
#alternativelly set method="backward" for backwards selection

##Choosing Among Models Using the Validation Set Approach and Cross-Validation----

#Validation Set Approach----
set.seed(1)
train=sample(c(TRUE,FALSE),nrow(Hitters),rep=TRUE)#this will sample between true and false
test=(!train)#this will flip the true/false
library(leaps)
regfit_best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)
#this performs best subset selection
#to compute validation set error, create a model matrix from the test data
test.mat=model.matrix(Salary~.,data=Hitters[test,])
#IMPORTANT test.mat needs to be with a ".", don't ask...
#The model.matrix() function is used in many regression packages for building
#an “X” matrix from data. Now we run a loop, and for each size i, we extract 
#the coefficients from regfit.best for the best model of that size, multiply 
#them into the appropriate columns of the test model matrix to form the 
#predictions, and compute the test MSE.
val_errors=rep(NA,19)
for (i in 1:19){
  coefi=coef(regfit_best,id=i) #id is the number of variables
  pred=test.mat[,names(coefi)]%*%coefi
  val_errors[i]=mean((na.omit(Hitters$Salary[test])-pred)^2)
}
val_errors
which.min(val_errors) #this will give us the model with the least error




#k-fold CV----
predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}#this fixes the problem with predict not having a regsubset method

k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv_errors=matrix(NA,k,19,dimnames=list(NULL,paste(1:19)))

for (j in 1:k){
  best_fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for (i in 1:19){
    pred=predict(best_fit,Hitters[folds==j,],id=i)
    cv_errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
  }
}
#this will output a matrix where the entries are the test MSE for the i-th CV fold

mean_cv_errors=apply(cv_errors,2,mean)
mean_cv_errors
plot(mean_cv_errors ,type="b")

reg_best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(reg_best,11)

#Ridge Regression ----
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

library(glmnet)
grid=10^seq(10,-2,length=100)
#this will perform ridge regression over alpha= 10^10 to 10^-2
#note that standardize=TRUE by default
ridge_mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge_mod)) 
#this will give num of predictors + 1 rows and num of lambda cols

#Now the actual example:
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary
library(glmnet)
grid=10^seq(10,-2,length=100)
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y_test=y[test]

ridge_mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge_pred=predict(ridge_mod,s=4,newx=x[test,])
#s sets lambda to 4
mean((ridge_pred-y_test)^2)
#however we want to test among different lambdas 
set.seed(1)
cv_out=cv.glmnet(x[train,],y[train],aplha=0)
plot(cv_out)
bestlam=cv_out$lambda.min
bestlam #this gives us the lambda for which the cv error is the smallest
ridge_pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge_pred-y_test)^2) # this gives the test mse



#Lasso----
library(glmnet)
lasso_mod= glmnet(x[train,],y[train],alpha=1 )
plot(lasso_mod)
set.seed(1)
cv_out=cv.glmnet(x[train,],y[train],aplha=1)
plot(cv_out)
bestlam=cv_out$lambda.min
lasso_pred=predict(lasso_mod,s=bestlam,newx=x[test,])
mean((leasso_pred-y_test))
#PCR----
library(ISLR)
Hitters=na.omit(Hitters)
x= model.matrix(Salary~.,data=Hitters)[,-1]
y=Hitters$Salary
library(pls)
set.seed(2)
pcr_fit=pcr(Salary~.,data=Hitters,scale=TRUE,validation="CV")
summary(pcr_fit)
validationplot(pcr_fit,val.type="MSEP")
#we can examine the plot to see where the smallest pcr occurs 

#now perform PCR on the training data and evaluate its test set performance
set.seet(1)
pcr_fit=pcr(Salary~.,data=Hitters,subset=train,scale=TRUE,validation="CV")
validationplot(pcr_fit,val.type = "MSEP")
pcr_pred=predict(pcr_fit,c[test,],ncomp = 7)
mean((pcr_pred-y_test)^2)

prc_fit= pcr(y~x,scale=TRUE,npcomp=5)
summary(prc_fit)

answer_sheet[1,2]=1

