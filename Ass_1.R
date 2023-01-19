#Ass_1

#Initial Notes:----

#Dataset_1 All features are useful.
#Are the features (highly) correlated? No, so no pcr
#What can we learn from benchmark tuning parameters?
#both pcr and fwd use 3 para, so use ridge
#as fwd will use all the parameters and will possibly overfit

#Dataset_2 Only a few features are useful.  
#Is variable selection a good idea? 
#We have a large amount of features with p>n, so lasso

#Dataset_3 All features are relevant for prediction,
#and their impacts are comparable. so no lasso as it will force some to 0  
#Is variable selection a good idea? Ridge 

#Dataset_4 A small group of regressors has relatively 
#large coefficients. Is variable selection a good idea?
#lasso may not be a good idea because of the small amount of features
#also because judging from the optimal parameters it will get almost the same 
#parameters but will introduce a lot of bias, so perhaps forward?

#Dataset_5 A few latent factors drive all the features, 
#so the features are strongly correlated. 
#Is dimension reduction a good idea?
#latent factors and strongly correlated ==> pcr

rm(list = ls())
answer_sheet=read.csv("Answer_Sheet.csv")
best=read.csv("bestpara.csv")

#Dataset_1----
dt1=read.csv("dataset1.csv")
x=model.matrix(target~.,dt1)[,-1]
y=dt1$target
library(glmnet)
ridge_mod=glmnet(x,y,alpha=0,lambda=best[2,2],thresh=1e-12)
answer_sheet[,2]=predict(ridge_mod,newx=x)

#Dataset_2----
dt2=read.csv("dataset2.csv")
x=model.matrix(target~.,dt2)[,-1]
y=dt2$target
set.seed(1)
lasso_mod=glmnet(x,y,alpha = 1,lambda = best[3,3])
answer_sheet[,3]=predict(lasso_mod,s=best[3,3],newx=x)


#Dataset_3----
dt3= read.csv("dataset3.csv")
x= model.matrix(target~., dt3)[,-1]
y=dt3$target
ridge_mod=glmnet(x,y,alpha=0,lambda=best[2,4],thresh=1e-12)
answer_sheet[,4]=predict(ridge_mod,newx = x)


#Dataset_4----
library(leaps)
dt4=read.csv("dataset4.csv")
data.mat=model.matrix(target~.,data=dt4)
fwd_mod= regsubsets(target~.,data=dt4,nvmax=best[1,5],method="forward")
fwd_coefi=coef(fwd_mod,best[1,5])
answer_sheet[,5]=data.mat[,names(fwd_coefi)]%*%fwd_coefi


#Dataset_5----
library(pls)
dt5= read.csv("dataset5.csv")
set.seed(1)
pcr_fit=pcr(target~.,data=dt5,scale=TRUE,validation="CV",ncomp = best[4,6])
answer_sheet[,6]=predict(pcr_fit,ncomp = best[4,6])

write.csv(answer_sheet,"~/Documents/Github/Statistical-Learning/answers.csv")

