# Today, I'll try to make a model higher AR socre more than 0.85
# Then I'm going to submit to the predicttion data.

# preparation
setwd("~/development/R/kaggle/titanic")
train <- read.csv("./data/train.csv", header=TRUE)
test <- read.csv("./data/test.csv", header=TRUE)

source("./lib/categolizedCabin.R")
source("./lib/categolizedTicket.R")
source("./lib/categolizedName.R")
source("./lib/predictNaAge.R")
source("./lib/calcAR.R")


# as factor
train$Survived = factor(train$Survived)

train$Pclass = factor(train$Pclass)
test$Pclass = factor(test$Pclass)

train$SibSp = factor(train$SibSp)
test$SibSp = factor(test$SibSp)


# all categolization
train <- categolizeCabin(train)
#train <- categolizedTicket(train)
train <- categolizedName(train)

train$Cabin = NULL
train$Ticket = NULL
train$Name = NULL


# predict NA
#train <- predictNaAge(train)

library('rpart')
library("rpart.plot")
source("./lib/calcAR.R")

# split data to train & valid
data_train <- train[1:700,]
data_valid <- train[701:891,]

####### modeling by all train
train.tree <- rpart(Survived ~., data=data_train, cp=-1, maxdepth=6)
rpart.plot(train.tree,type=4,extra=1)

# AR value of train 
train.prob <- predict(train.tree, newdata=data_train, type="prob")
calcAR(X=train.prob[, 2], y=data_train$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)

# AR value of valid
valid.prob <- predict(train.tree, newdata=data_valid, type="prob")
calcAR(X=valid.prob[, 2], y=data_valid$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)




#summary(data_train$categolizedName)
#summary(data_valid$categolizedName)
#=> factor categolizedName has new levels Capt, Jonkheer, the Countess

subset(data_valid, categolizedName == "Capt")　# Captain
train$categolizedName[746] <- factor("Mr")
subset(data_valid, categolizedName == "Jonkheer") # Noble man
train$categolizedName[823] <- factor("Mr")
subset(data_valid, categolizedName == "the Countess") # Noble woman
train$categolizedName[760] <- factor("Mrs")


####### remodeling by all train
train.re_tree <- rpart(Survived ~., data=train, cp=-1, maxdepth=6)
rpart.plot(train.re_tree,type=4,extra=1)

# AR value of train 
train.re_prob <- predict(train.re_tree, newdata=train, type="prob")
calcAR(X=train.re_prob[, 2], y=train$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)


# for test
test <- categolizeCabin(test)
#test <- categolizedTicket(test)
test <- categolizedName(test)
subset(test, categolizedName == "Dona") # lady in spanish
test$categolizedName <- "Mrs"

# predict NA
#test <- predictNaAge(test)

test$Cabin = NULL
test$Ticket = NULL
test$Name = NULL

# prediction
test$Survived <- predict(train.re_tree, newdata=test, type="class")

test.df <- data.frame(PassengerId=test$PassengerId,Survived=test$Survived)
row.names(test.df)<-1:nrow(test.df)
write.csv(test.df,file="./data/20140307_predict.csv",row.names=FALSE)

# I should check test data, why id didn't much this model