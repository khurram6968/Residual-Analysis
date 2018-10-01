#Residula Analysis
#Here i use cars data in R
lm_model=lm(dist~speed,data=cars)
lm_model
#We can check heterosedasticity in the models there are two ways:
#1-Graphically
#2-Through Statistical Test
###1
par(mfrow=c(2,2))#to set graph in one panel 2row and 2col.
plot(lm_model)
#If there is no hetroscedasticity in data 
#the red line is flat and 
#there points is completely random(no pattern) and 
#equal distribution throughout range of X-axis.
####But in our case the red line is slightly curved and 
#the residuals increase as the fitted Y values.
#This means Heteroscedasticity in our data.
###2
#We use Breusch-Pagan Test and NCV-Test to check heteroscedasticity in our data.
library(lmtest)# for bptest() function 
bptest=bptest(lm_model)
bptest
library(car)# for ncvTest() function
ncv_test=ncvTest(lm_model)
ncv_test
#Null Hypothesis H0=Variance of Residuals is constant(no Heteroscedasticity)
###Rectify
#Now we use Box_Cox_Transformation is a mathmetical method a continous variable to transform 
#into normal distribution.
library(caret)# for BoxCoxTrans() function
dist_Boxcox_trans=BoxCoxTrans(cars$dist)
dist_Boxcox_trans
dist_new=predict(dist_Boxcox_trans,cars$dist)
cars=cbind(cars,dist_new)
#make new model and check graphically and through statistical test
new_lm.model=lm(dist_new~speed,data = cars)
plot(new_lm.model)
new_bptest=bptest(new_lm.model)
new_bptest
new_ncvtest=ncvTest(new_lm.model)
new_ncvtest
#So Null Hypothesis accepted.there is no heteroscedasicty.
#Why remove heteroscedasticity in model because for better prediction.
