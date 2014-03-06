# I tried to categorize to Ticket $ Cabin
# So now I'll try to model by randomforest
# ref ~> http://mjin.doshisha.ac.jp/R/32/32.html

library("randomForest")

setwd("~/development/R/kaggle/titanic")
train <- read.csv("./data/train.csv", header=TRUE)
test <- read.csv("./data/test.csv", header=TRUE)

summary(train)
summary(test)
str(train$)

# Survival, Pclass, SibSp to factor
train$Survived = factor(train$Survived)

train$Pclass = factor(train$Pclass)
test$Pclass = factor(test$Pclass)

train$SibSp = factor(train$SibSp)
test$SibSp = factor(test$SibSp)


# remove unnecessary columns
train$PassengerId = NULL
train$Name = NULL
test$Name = NULL

#############################################
# transform Ticket to be category 
#############################################

categolizeTicket <- function(df){
  df$gsub_ticket <- gsub(pattern="\\.", replacement="", df$Ticket)
  df$gsub_ticket <- gsub("/", "", df$gsub_ticket)
  df$Ticket_factor <- factor(substr(df$gsub_ticket, 1,1))
  df$Ticket = NULL
  df$gsub_ticket = NULL
  return(df)
}

train <- categolizeTicket(train)


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

#############################################
# randomForest modeling
#############################################

train.rf <- randomForest(Survived ~., data=train ,na.action="na.omit")
# 以下にエラー randomForest.default(m, y, ...) : 
#  外部関数の呼び出し (引数 1) 中に NA/NaN/Inf があります 
# 追加情報:  警告メッセージ: 
#  In data.matrix(x) :  強制変換により NA が生成されました 

# confirm to NA columns
summary(train)

# I must predict NA Age columns when I use randomforest
# The Age have 177 NA columns

#############################################
# predict NA Age
#############################################

train.na <- subset(train, is.na(Age))
age_train <- subset(train, !is.na(Age))
age_train$Survived = NULL # test data doesn't have this

# check linearity
names(age_train)
plot(age_train$Pclass, age_train$Age)
plot(age_train$Sex, age_train$Age)
plot(age_train$SibSp, age_train$Age)
plot(age_train$Parch, age_train$Age)
plot(age_train$Fare, age_train$Age)
plot(age_train$Embarked, age_train$Age)
plot(age_train$Ticket_factor, age_train$Age)
plot(age_train$Cabin_factor, age_train$Age)
#=> looks like non linear

library("rpart")
library("rpart.plot")

nrow(age_train)
age_train.train <- age_train[1:500,]
age_train.valid <- age_train[501:714,]

age_train.tree <- rpart(Age ~., data=age_train, cp=-1, maxdepth=5)
#rpart.plot(age_train.tree,type=4,extra=1)

# AR value of train 
age_train.train.prob<- predict(age_train.tree, newdata=age_train.train)
# AR value of valid
age_train.valid.prob <- predict(age_train.tree, newdata=age_train.valid)


age_train.valid$pred_Age <- predict(age_train.tree, newdata=age_train.valid)

age_train.valid$pred_Age
age_train.valid$Age
# => LGTM

predictNaAge <- function(df) {
  nRow <- nrow(df)
  for(i in c(1:nRow)) {
    df[i, "Age"] <- ifelse(is.na(df[i, "Age"]) ,
                           predict(age_train.tree, newdata=df[i,]),
                           df[i, "Age"])
  }
  return(df)
}

train <- predictNaAge(train)
summary(train$Age)


#############################################
# retry randomForest modeling
#############################################

train.rf <- randomForest(Survived ~., data=train, type="class")
print(train.rf)
summary(train.rf)
importance(train.rf)

# predict test data
test <- categolizeTicket(test)
test <- categolizeCabin(test)
test <- predictNaAge(test)
#predict(train.rf, newdata=test, type="class")
#以下にエラー predict.randomForest(train.rf, test) : 
#  Type of predictors in new data do not match that of the training data.

summary(train)
summary(test)


# Then, I'll try to model by rpart, predicted NA Age data.


data_train <- train[1:700,]
data_valid <- train[701:891,]

# 10th train tree model 
train.tree_10 <- rpart(Survived ~., data=data_train, cp=-1, maxdepth=5)
rpart.plot(train.tree_10,type=4,extra=1)

# AR value of train 
train.prob10 <- predict(train.tree_10, newdata=data_train, type="prob")
source("./lib/calcAR.R")
calcAR(X=train.prob10[, 2], y=data_train$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)

# AR value of valid
valid.prob10 <- predict(train.tree_10, newdata=data_valid, type="prob")
calcAR(X=valid.prob10[, 2], y=data_valid$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)


# remodeling
train.tree_r_10 <- rpart(Survived ~., data=data_train, cp=-1, maxdepth=5)
rpart.plot(train.tree_r_10,type=4,extra=1)

test$Survived <- predict(train.tree_r_10, newdata=test, type="class")
summary(test)

test.df <- data.frame(PassengerId=test$PassengerId,Survived=test$Survived)
row.names(test.df) <- 1:nrow(test.df)
write.csv(test.df,file="./data/20140302_3_predict.csv",row.names=FALSE)
