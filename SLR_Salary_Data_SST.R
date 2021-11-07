
#############################

## Simple Linear Regression Assignment ##
## Sarang Thakare ##

## Salary Data and Employee Experience Problem


#############################

#Load Dataset

library(readr)
sal_data<-read.csv("C:/Users/320101002/Documents/Data_Science/4_Simple_Linear_Regression/Input_Data/Salary_Data.csv")

View(sal_data)

#############################


#Business Problem
#To determine if the experience has any correlation to salary of the employees
# y ~ Salary x - Years of Experience
# Ho = Years opf Experience has NO effect on the Salary
# Ha = Years opf Experience does have effect on the Salary

#############################

#Exploratory Data Anslysis

#############################

#Exploratory Data Anslysis

attach(sal_data) # attaching data frame so that feastures can be directly referred
summary(sal_data) #To see Q1, Median, Q3 etc.,

# Both Salary and Experience data have slight skewness towards left  

sal<-Salary #Rename features with short names X Variable
yr_exp<-YearsExperience  # Y Variable

boxplot(sal,horizontal = "TRUE")
hist(sal)

boxplot(yr_exp,horizontal = "TRUE")
hist(yr_exp)
#Boxplot and Histogram of both Delivery Time and Sorting Time appear normal

# Check normality of the data
#Ho : Data normal 
#Ha : Data not normal

qqnorm(sal) # Normal Probability plot to check normality. Appears Bimodal distribution
qqline(sal)

qqnorm(yr_exp)
qqline(yr_exp)


shapiro.test(sal) # To check P value. P is less than 0.05 [95% confidence], null to go, meaning data is normal
shapiro.test(yr_exp) # P is high, Null should fly, Implies Data normal

plot(yr_exp,sal) # Plot scatter plot x=Sorting Time and y = Delivery Time
cor(yr_exp,sal) # check correlation before transforming Weight gained data Correlation coefficent 0.825


# We will have to transform data into Normal. Salary data apperaded to be bimodal

shapiro.test(log(sal))
shapiro.test(log(yr_exp))

# With Log we got P vaues greater than 0.05, implying data top be normal

cor(log(yr_exp),log(sal))


#############################


#Data will NOT be split into Training and testing data
# We will use Residual RMSE and R2 value to check model accuracy

#############################


#Simple Linear Regression on transformed log data 

reg_log <-lm(log(sal)~log(yr_exp))
summary(reg_loglog)

reg_second <-lm(log(sal)~(yr_exp)+I(yr_exp^2))
summary(reg_second) # Shows Slope, Intercept, std error and R2 value. R2 adjusted is 0.9486 which is fairly goof
# P is very low, indicating that Null should be rejected implying Calories have effect on Weight

# We will explore a few other models before proceeding with prediction model

reg_lm=lm(sal~yr_exp)
summary(reg_lm)

# Linear Model would give the best R2 value


#############################

## Exploring other models to improve R2 value

# Quadratic Model

# Y ~ Del and X = sort * sort

cor(log(yr_exp)*log(yr_exp),log(sal))

reg_quad<-lm(log(sal)~log(yr_exp)*log(yr_exp))
 summary(reg_quad)

# Exponential model

# Y ~ Exp(Log(sal) and X = Log(yr)exp)


reg_exp<-lm(exp(log(sal)))~exp(log(yr_exp))
summary(reg_exp)
# Second degree equation

# Y ~ del and X = (sort) + sort*sort

reg_second<-lm(del~sort+I(sort*sort))
summary(reg_second)

# R square value for Log - Log is maximum 0.77 

#Hence we will proceed with log model

###########################

cor(log(yr_exp),log(sal))
reg_loglog=lm(log(sal)~log(yr_exp))

reg_loglog$residuals  # shows residuals

sum(reg_loglog$residuals)

RMSE=sqrt(mean(sum(reg_loglog$residuals^2)))
RMSE

reg_loglog$coefficients

confint(reg_loglog,level=0.95) # Y is Log(wt)
predict(reg_loglog,interval="predict") # Predicted Y is innterms of Log of Weight

install.packages("ggplot2")
library(ggplot2) #

pred<-predict(reg_loglog)

ggplot(data = sal_data, aes(x = log(yr_exp), y = log(sal))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sal_data, aes(x=log(yr_exp),y=pred))

summary(reg_loglog)

reg_loglog$coefficients

# Prediction equation 
# Predicted Log (sal) = 10.32804 + 0.56209*log(yr_exp)
#  Taking Antilogs both sides
#Predicted sal = = EXP(10.32804 + 0.56209*log(yr_exp))

# This model will be deployed using R Shiny

#####################################################################


# Simple Linear regression without transforming into normal 

#####################################################################

cor(exp,sal)
reg_lm=lm(sal~yr_exp)
summary(reg_lm)

pred<-predict(reg_lm)

reg_lm$residuals  # shows residuals

sum(reg_lm$residuals)

RMSE=sqrt(mean(sum(reg_lm$residuals^2)))
RMSE

reg_lm$coefficients

confint(reg_lm,level=0.95) # Y is Log(wt)
predict(reg_lm,interval="predict") # Predicted Y is innterms of Log of Weight

install.packages("ggplot2")
library(ggplot2)

ggplot(data = sal_data, aes(x = yr_exp, y = sal)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sal_data, aes(x=yr_exp,y=pred))

summary(reg_lm)

reg_lm$coefficients

RMSE=sqrt(mean(sum(reg_lm$residuals^2)))
RMSE


# Prediction equation 
# Predicted Weight = 25792.2 + 9449.962 * yr_exp

# We need to test the stability of these two models and deploy using R Shiny


#####################################################################
