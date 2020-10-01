#clear the environment
rm(list=ls(all = TRUE))
# logistic regression
# read in filer response data
fileresponse <- read.csv("FlierResponse.csv")
fileresponse
str(fileresponse)
glimpse(fileresponse)
fileresponse$Response <- as.factor(fileresponse$Response)
summary(fileresponse)

#fileresponseglm <- glm(Response~Age , data = fileresponse , family = "binomial")
#fileresponseglm
#summary(fileresponseglm)
#logLik(fileresponseglm)
#deviance(fileresponseglm)
#AIC(fileresponseglm)

# case study the framingham heart study

#read in the framingham dataset

framingham = read.csv("framingham.csv")
str(framingham)
summary(framingham)

# load the library ca tools
library(caTools)
library(car)  # helps to find out rel
#library(DAAG)
#library(rms)

# randomly split the data into training nd testing sets

set.seed(1000)
split = sample.split(framingham$TenYearCHD , SplitRatio = 0.70)

# splitup the data using  subset
train = subset(framingham , split ==TRUE)
test = subset(framingham,split==FALSE)



#logistics regression model

framinghamLog = glm(TenYearCHD~., data = train , family = binomial)
summary(framinghamLog)
car::vif(framinghamLog)
#rms::vif(framinghamLog)
#DAAG::vif(framinghamLog)
# aic lower the aic better the model
#Accuracy on the training set
predictTrain <- predict(framinghamLog , type = "response", newdata = train)
predictTrain
# confusion matirx with threshold of 0.5
table(train$TenYearCHD , predictTrain >0.5)
# accuracy on the train set (all correct/all) = TP+TN/TP+TN+FP+FN
(2170+30)/(2170+30+357+9)
#precision (true positives/predicted positive) = TP/TP+FP
2170/(2170+375)
#sensititvity aka recall(true positives/all actual positives)= TP/TP+FN
2170/(2170+9)
#specificity(true negatives/all actual negatives  )= TN/TN+FP
30/(30+357)
# predictions on the test set
predictTest <- predict(framinghamLog,type= "response",newdata = test)
#confusion matrix with threshold of 0.5
table(test$TenYearCHD , predictTest >0.5)
#Test set AUC
library(ROCR)
ROCRpred = pre








