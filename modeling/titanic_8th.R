
setwd("~/development/R/kaggle/titanic")
train <- read.csv("./data/train.csv", header=TRUE)
test <- read.csv("./data/test.csv", header=TRUE)


# Survival, Pclass, SibSp to factor
train$Survived = factor(train$Survived)

train$Pclass = factor(train$Pclass)
test$Pclass = factor(test$Pclass)

train$SibSp = factor(train$SibSp)
test$SibSp = factor(test$SibSp)

# remove unnecessary columns
train$PassengerId = NULL
train$Ticket = NULL

train$Name = NULL
test$Name = NULL

#############################################
# transform Cabin to be category 
#############################################

categolizeCabin <- function(df) {
  nRow <- nrow(df)
  for(i in c(1:nRow)) {
    df[i, "Cabin_factor"] <- ifelse(df[i, "Cabin"] == "" ,
                                    switch(df[i, "Pclass"],"X", "Y", "Z"),
                                    substr(df[i, "Cabin"] , 1 ,1)
    )
  }
  df$Cabin_factor <- factor(df$Cabin_factor)
  df$Cabin = NULL
  return(df)
}

train <- categolizeCabin(train)

data_train <- train[1:700,]
data_valid <- train[701:891,]

# 11th train tree model 
train.tree_11 <- rpart(Survived ~., data=data_train, cp=-1, maxdepth=7)
rpart.plot(train.tree_11,type=4,extra=1)

# AR value of train 
train.prob11 <- predict(train.tree_11, newdata=data_train, type="prob")
source("./lib/calcAR.R")
calcAR(X=train.prob11[, 2], y=data_train$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)

# AR value of valid
valid.prob11 <- predict(train.tree_11, newdata=data_valid, type="prob")
calcAR(X=valid.prob11[, 2], y=data_valid$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)


# remodeling
train.tree_r_11 <- rpart(Survived ~., data=train, cp=-1, maxdepth=7)
rpart.plot(train.tree_r_11,type=4,extra=1)

train.prob11 <- predict(train.tree_r_11, newdata=train, type="prob")
calcAR(X=train.prob11[, 2], y=train$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)

# prepare to test data
test <- categolizeCabin(test)

test$Survived <- predict(train.tree_r_11, newdata=test, type="class")
summary(test)

test.df <- data.frame(PassengerId=test$PassengerId,Survived=test$Survived)
row.names(test.df) <- 1:nrow(test.df)
write.csv(test.df,file="./data/20140303_1_predict.csv",row.names=FALSE)

#=> over fitting