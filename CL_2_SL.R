##CL_2_SL

#Q5.4.8
#Generate the data
set.seed(2)
y=rnorm(100)
x=rnorm(100)
y=x-2*x^2+rnorm(100)

#visualise
plot(x,y)

#Compute LOOCV for fitting different models 
library(ISLR)
library(boot)
df=data.frame(x,y)
cv_error=rep(0,4)
for (i in 1:4){
  glm_fit=glm(y~poly(x,i),data=df)
  cv_error[i]=cv.glm(data=df,glmfit=glm_fit)$delta[1]
}
cv_error
#first output with seet 1 
#[1] 5.890979 1.086596 1.102585 1.114772 1.131163
#second output with seed 2
#[1] 6.140266 1.169795 1.191309 1.180095 1.186823