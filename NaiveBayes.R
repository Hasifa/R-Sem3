install.packages("mlbench")
library(mlbench)
data(HouseVotes84)

#barplots for specific issue
plot(as.factor(HouseVotes84[,2]))
title(main="votes cast for issue1",xlab = "vote",ylab = "Num reps")

#by party
HouseVotes84$Class
Repub <- HouseVotes84$Class=="republican"
Democrat <- HouseVotes84$Class=="democrat"
Repub
plot(as.factor(HouseVotes84[Repub,2]))
title(main="Repulican votes cast for issue1",xlab = "vote",ylab = "Num reps")
plot(as.factor(HouseVotes84[Democrat,2]))
title(main="Democrat votes cast for issue1",xlab = "vote",ylab = "Num reps")     
# we have lot of NA we are going to impute the values
# function needed for imputation 
#function to return number of NAs by vote and class(democrat or republican)
na_by_col_class <-function(col,cls){
  return(sum(is.na(HouseVotes84[,col]) & HouseVotes84$Class == cls))
}
na_by_col_class
#function to compute the conditional probability that a member of a party will cast a "yes" vote for a
# particular issue. the probability is based on all members of th eparty who 
#actually cast a vote on issue
p_y_col_class<- function(col,cls){
  sum_y<-sum(HouseVotes84[,col]=="y"& HouseVotes84$Class == cls,na.rm = TRUE)
  sum_n<-sum(HouseVotes84[,col]=="n"& HouseVotes84$Class == cls,na.rm = TRUE)
  return(sum_y/(sum_y+sum_n))
}


#check the probability of yes vote by a democrat in issue 5
p_y_col_class(5,"democrat")
#check the prob of yes vote by a republican in issue 5
p_y_col_class(5,"republican")
#check the na for both democrat and republican
na_by_col_class(2,"democrat")
na_by_col_class(2,"republican")


#impute the missing values,
#If the republican congresman didn't vote then we are allocating 'y' or 'n' based on if their 
#party voted 'y'  or 'n'
for(i in 2:ncol(HouseVotes84)){
  if(sum(is.na(HouseVotes84[,i])>0)){
    c1 <- which(is.na(HouseVotes84[,i])&HouseVotes84$Class == "democrat",arr.ind = TRUE )
    c2 <- which(is.na(HouseVotes84[,i])&HouseVotes84$Class == "repiblican",arr.ind = TRUE )
    HouseVotes84[c1,i] <-
      ifelse(runif(na_by_col_class(i,"democrat"))<p_y_col_class(i,"democrat"),"y","n")
    HouseVotes84[c2,i] <-
      ifelse(runif(na_by_col_class(i,"republican"))<p_y_col_class(i,"republican"),"y","n")
  }
}


#divide the data into test and training sets
#create new col "train" and assign 1 or 0 in 80/20 proportion via random uniform dist
HouseVotes84[,"train"] <- ifelse(runif(nrow(HouseVotes84))<0.80,1,0)
#get col number of train /test indicator column (needed later)
trainColNum <- grep("train",names(HouseVotes84))
#separate training and test sets and remove training column before modeling
trainHouseVotes84 <- HouseVotes84[HouseVotes84$train == 1 , -trainColNum]
testHouseVotes84 <- HouseVotes84[HouseVotes84$train == 0 , -trainColNum]


#load e1071 library and invoke naiveBayes method
library(e1071)
nb_model <- naiveBayes(Class~.,data = trainHouseVotes84)
#In this notation the dependent variable to be predicted appears on the left hand side of the ~ and the independent variable in right hand side
nb_model
summary(nb_model)
str(nb_model)
#lets test the model
nb_test_redict <- predict(nb_model,testHouseVotes84[,-1])
#fraction of correct predictions
mean(nb_test_redict==testHouseVotes84$Class)
#confusion matrix
table(pred=nb_test_redict ,true= testHouseVotes84$Class)
#fraction of correct predictions
mean(nb_test_redict==testHouseVotes84$Class)
### we'll create a function which takes the number of times the model should be run and the training fraction as input

#function to create run and record model results
nb_multiple_runs <- function(train_fraction,n){
  fraction_correct <- rep(NA,n)
  for(i in 1:n){
    HouseVotes84[,'train']<-ifelse(runif(nrow(HouseVotes84))<train_fraction,1,0)
    trainColNum <- grep('train',names(HouseVotes84))
    trainHouseVotes84 <- HouseVotes84[HouseVotes84$train ==1,-trainColNum]
    testHouseVotes84 <- HouseVotes84[HouseVotes84$train ==0,-trainColNum]
    nb_model <-naiveBayes(Class~.,data = trainHouseVotes84)
   fraction_correct[i] <- mean(nb_test_redict == testHouseVotes84$Class) 
  }
  return(fraction_correct)
}






