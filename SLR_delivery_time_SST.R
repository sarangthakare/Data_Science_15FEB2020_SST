
#############################

## Simple Linear Regression Assignment ##
## Sarang Thakare ##

## Sorting Time Vs Delivery Time problem


#############################

#Load Dataset

library(readr)
deltime_sorttime<-read.csv("C:/Users/320101002/Documents/Data_Science/4_Simple_Linear_Regression/Input_Data/delivery_time.csv")

View(deltime_sorttime)

#############################


#Business Problem
#To determine if the Delivery Time [Y] gets impacted due to sorting Time [X] 
# Ho = Sorting Time has NO effect on Delivery Time
# Ha = Sorting Time has an effect on Delivery Time

#############################

#Exploratory Data Anslysis

#############################

#Exploratory Data Anslysis

attach(deltime_sorttime) # attaching data frame so that feastures can be directly referred
summary(deltime_sorttime) #To see Q1, Median, Q3 etc.,

# Both Delivery Time and Sorting Time appear symmetric 

sort<-Sorting_Time #Rename features with short names X Variable
del<-Delivery_Time  # Y Variable

boxplot(del,horizontal = "TRUE")
hist(del)

boxplot(sort,horizontal = "TRUE")
hist(sort)
#Boxplot and Histogram of both Delivery Time and Sorting Time appear normal

# Check normality of the data
#Ho : Data normal 
#Ha : Data not normal

qqnorm(del) # Normal Probability plot to check normality
qqline(del)

qqnorm(sort)
qqline(sort)


shapiro.test(del) # To check P value. P is greater than 0.05 [95% confidence], null go, meaning data is normal
shapiro.test(sort) # P is high, Null should fly, Implies Data normal

plot(sort,del) # Plot scatter plot x=Sorting Time and y = Delivery Time
cor(sort,del) # check correlation before transforming Weight gained data Correlation coefficent 0.825

# We will check simple Linear  regression between Delivery Time [Y] ~ Sorting Time [X]

#############################


#Data will NOT be split into Training and testing data
# We will use Residual RMSE and R2 value to check model accuracy

#############################


#Simple Linear Regression of y as a function of x

reg_lm<-lm(del~sort)
summary(reg_lm) # Shows Slope, Intercept, std error and R2 value. R2 adjusted is 0.867 which is fairly goof
# P is very low, indicating that Null should be rejected implying Calories have effect on Weight

# We will explore a few other models before proceeding with prediction model



#############################

## Exploring other models to improve R2 value

# Quadratic Model

# Y ~ Del and X = sort * sort

cor(sort*sort,del)

reg_quad<-lm(del~sort*sort)
summary(reg_quad)

# Log - Log Model Model

# Y ~ Log (del) and X = Log(sort)

cor(log(sort),log(del))

reg_loglog<-lm(log(del)~log(sort))
summary(reg_loglog)

# Second degree equation

# Y ~ del and X = (sort) + sort*sort

reg_second<-lm(del~sort+I(sort*sort))
summary(reg_second)

# R square value for Log - Log is maximum 0.77 

#Hence we will proceed with log model

###########################

pred<-predict(reg_loglog)

reg_loglog$residuals  # shows residuals

sum(reg_loglog$residuals)

RMSE=sqrt(mean(sum(reg_loglog$residuals^2)))
RMSE

reg_loglog$coefficients

confint(reg_loglog,level=0.95) # Y is Log(wt)
predict(reg_loglog,interval="predict") # Predicted Y is innterms of Log of Weight

install.packages("ggplot2")
library(ggplot2)

ggplot(data = deltime_sorttime, aes(x = log(sort), y = log(del))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = deltime_sorttime, aes(x=log(sort),y=pred))

summary(reg_loglog)

reg_loglog$coefficients

# Prediction equation 
# Predicted Log (delivery time) = 1.741987 + 0.59752*log(sorting time)
#  Taking Antilogs both sides
#Predicted Delivery Time = = EXP(1.741987 + 0.59752* Log(Sorting Time))

# This model will be deployed using R Shiny

#####################################################################


