
##----------------------------------------------------------------------------------##
##--- SIMPLE LINEAR REGRESSION MODEL building ------##
##----------------------------------------------------------------------------------##


##--- Step 1: Clear environment variables ------------------------------------------##
rm(list=ls(all = TRUE))

##__________________________________________________________________________________##


##--- Step 2: Set working Directory ------------------------------------------------##
getwd()
##setwd

##__________________________________________________________________________________##


##--- Step 3: Read the data from the csv file --------------------------------------##
cars_data <- read.csv(file = "Toyota_SimpleReg.csv", header= TRUE)
names(cars_data)
row.names(cars_data)
summary(cars_data)
str(cars_data)
View(cars_data)

##__________________________________________________________________________________##

# dropping id and model  as we are doing simple regression - we can do the simple regression only on numerical data
##--- Step 4: Perform Exploratory Data Analysis and Data Pre-processing-------------##
## Drop any irrelevant attribute(s):
cars_data <-cars_data[,-c(1,2)]

## Summary of the data and look for any missing values:
str(cars_data)
summary(cars_data)
# No missing values

## Correlation and Covariance between the attributes: detremines the relationship bet 2 varibale
cov(cars_data)
# the covariance of the age of car and price is -59136.11
#It indicate a negative linear relationshop between the two variables
# this relation could be observed from the scteer plot also
plot(cars_data$Age_06_15 , cars_data$Price)
plot(cars_data$Age_06_15 , cars_data$Price , xlab = "Age of the car" , ylab = "Price in ($)" , pch= 18, col = "blue")

#Describe how the covarainace and correlation coefficients 
cor(cars_data)
cor(cars_data$Age_06_15,cars_data$Price)

# how the variables are related is correlation
# since the value is close to 1 and has a -ve sign we can vonclude that the variable are strongly negative correlated.
#Do the attributes have a good enough correlation coefficient to support linear regres  sion model building?

##__________________________________________________________________________________##


##--- Step 5: Split the data into train and test datasets --------------------------##
#Split in (train:test) in (80:20) ratio
rows = seq(1,nrow(cars_data),1)
set.seed(123)
trainRows <- sample(rows,(70*nrow(cars_data))/100)
cars_train <- cars_data[trainRows,]
cars_test <- cars_data[-trainRows,]

trainRows1 <- sample(rows,(80*nrow(cars_data))/100)
cars_train1 <- cars_data[trainRows,]
cars_test1 <- cars_data[-trainRows,]


trainRows2 <- sample(rows,(90*nrow(cars_data))/100)
cars_train2 <- cars_data[trainRows,]
cars_test2 <- cars_data[-trainRows,]

##__________________________________________________________________________________##


##--- Step 6: Linear regression model building--------------------------------------##
LinReg <- lm(Price ~ Age_06_15, data = cars_train)
coefficients(LinReg)
LinReg1 <- lm(Price ~ Age_06_15, data = cars_train1)
coefficients(LinReg)
LinReg2 <- lm(Price ~ Age_06_15, data = cars_train2)
coefficients(LinReg)
## Summary of model:
summary(LinReg)
plot(LinReg$residuals)
summary(LinReg1)
summary(LinReg2)

#optional info
#to extract the coefficients
coefficients(LinReg)
coefficients(LinReg)[1]
coefficients(LinReg)[2]
# to extract the residuals
LinReg$residuals
LinReg$rank
# to extract the train predictions
LinReg$fitted.values


##__________________________________________________________________________________##


##--- Step 7: Check for validity of linear regression assumptions ------------------##
#HINT: plot the 4 graphs to check. Write your comments
par(mfrow = c(2,2))
plot(LinReg)
par(mfrow = c(1,1))

##__________________________________________________________________________________##


##--- Step 8: Predict on testdata --------------------------------------------------##
test_prediction <- predict(LinReg, cars_test)
test_actual = cars_test$Price

##__________________________________________________________________________________##


##--- Step 9: Error Metrics --------------------------------------------------------##
library(DMwR)

#Error verification on train data
regr.eval(cars_train$Price,LinReg$fitted.values)


#Error verification on test data
regr.eval(test_actual,test_prediction)


##__________________________________________________________________________________##


##--- Step 10: Confidence and Prediction Intervals----------------------------------##
#Find the confidence and prediction intervals and plot them for the WHOLE dataset





##__________________________________________________________________________________##
#-----------------------end---------------------------------------------------------##