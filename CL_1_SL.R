##Session 1: Introduction to R 
#Q1----
#Replicate results of 2.4.8
dt<-read.csv("College.csv")
summary(dt)
rownames(dt)<-dt[,1]
dt<-dt[,-1]
dt["Private"]<-dt["Private"]=="Yes"
#dt["Private"]<-as.factor(dt["Private"])
#dt<-as.factor(dt) idk 
par(mar=rep(1,4))
pairs(dt[1:10])

boxplot(Outstate~Private, data=dt,xlab="Are you a rick f**k?",ylab="Oustate distribution")

library(ggplot2)
dev.off() # this will reset the figures and settings for plotting
graphs= list()
for (i in 1:4){
   graphs[[i]]=ggplot(data = dt, aes(x=Apps)) +
    geom_histogram(bins=i*5)+
    xlab(paste0(i*5," Bins"))+
    ylab("")
}


library(gridExtra)
#grid.arrange(graphs[[1]],graphs[[2]],graphs[[3]],graphs[[4]],ncol=2,top="Histogram of Apps with different number of bins")
do.call(grid.arrange,graphs) #this automates the one above

#Q2----
#Validation Set Approach----
library(ISLR)
set.seed(1)
train=sample(392,196)#(number of observations, how many we choose for the training set)
lm_fit=lm(mpg~horsepower,data=Auto,subset=train)# we fit the data 
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)# this is the MSE 

lm_fit2= lm(mpg~poly(horsepower,2),data=Auto,subset=train)
#poly gives a polynomal model with poly(x-variable, degree of the polynomial)
#subset gives the data used for the training 
mean((mpg-predict(lm_fit2,Auto))[-train]^2)
#we perform MSE here and predict is used to get f_hat
#[-train ] gives the test set

  
#Leave-One-Out Cross-Validation (LOOCV)----
glm_fit= glm(mpg~horsepower,data=Auto)
coef(glm_fit)
#whithout family= argument it just performs linear regression like lm()r

library(boot)
cv_err=cv.glm(Auto,glm_fit)
cv_err$delta #these are the cross-validation results 

#change the polynomial fits
cv_error=rep(0,5)
for (i in 1:5){
  glm_fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv_error[i]=cv.glm(Auto,glm_fit)$delta[1]
}
cv_error


#k-Fold Cross-Validation----
set.seed(17)
cv_error_10=rep(0,10)
for (i in 1:10){
  glm_fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv_error_10[i]= cv.glm(Auto,glm_fit,K=10)$delta[1]
}#for the delta the first is the k-fold CV estimate and the second is the bias corrected
cv_error_10

#Bootstrap----
alpha_fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

set.seed(1)
alpha_fn(Portfolio,sample(100,100,replace = TRUE))#and we repeat many times
#now use bootstrap to automate this process
boot(Portfolio,alpha_fn,R=1000)#(DataFrame,FUN,how many times we want to repeat it) 
#in the output original is the alpha hat and the bootstrap estimate for se(alpha hat) is in std. error

#Now use more variables
boot_fn=function(data,index){
  coefficients(lm(mpg ~ horsepower+I(horsepower^2),data=data,subset=index))
}
set.seed(1)
boot(Auto,boot_fn,1000)
#the results are t1* is intercept, t2* is horsepower and t3* is hpw^2

















