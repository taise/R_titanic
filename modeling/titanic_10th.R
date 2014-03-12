# check the difference of the best socore model and others.

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

######## the best one

train.best <- train
train.best$PassengerId = NULL
train.best$Name = NULL
train.best$Ticket = NULL

train.best <- categolizeCabin(train.best)

data_train <- train.best[1:700,]
data_valid <- train.best[701:891,]


# rpart
train.best.tree <- rpart(Survived ~., data=data_train, cp=-1, maxdepth=5)
rpart.plot(train.best.tree,type=4,extra=1)


# AR value of train data
data_train.prob <- predict(train.best.tree, newdata=data_train, type="prob")
calcAR(X=data_train.prob[, 2], y=data_train$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)

# AR value of valid data
data_valid.prob <- predict(train.best.tree, newdata=data_valid, type="prob")
calcAR(X=data_valid.prob[, 2], y=data_valid$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)

# chech this model
train.best.tree
capture.output(summary(train.best.tree),file="./modeling/titanic_3rd_model.txt")
