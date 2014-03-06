library("rpart")
library("rpart.plot")

setwd("~/development/R/kaggle/titanic")
train <- read.csv("./data/train.csv", header=TRUE)
test <- read.csv("./data/test.csv", header=TRUE)

ncol(train)
(train.nRow <- nrow(train))

# Survival to factor
train$Survived = factor(train$Survived)

# remove unnecessary columns
train$PassengerId = NULL
train$Name = NULL
test$Name = NULL


summary(train)
str(train)

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
  data_frame <- df
  nRow <- nrow(data_frame)
  for(i in c(1:nRow)) {
    data_frame[i, "Cabin_factor"] <- ifelse(data_frame[i, "Cabin"] == "" ,
                                            switch(data_frame[i, "Pclass"],"X", "Y", "Z"),
                                            substr(data_frame[i, "Cabin"] , 1 ,1)
    )
  }
  data_frame$Cabin_factor <- factor(data_frame$Cabin_factor)
  data_frame$Cabin = NULL
  return(data_frame)
}

train <- categolizeCabin(train)



# separate to train & valid
#data_train <- train[1:700,]
#data_valid <- train[701:891,]
data_train <- train[c(1:450,501:891),]
data_valid <- train[451:500,]


data_train.nRow <- nrow(train)
data_valid.nRow <- nrow(train)

# 9th train tree model 
train.tree_9 <- rpart(Survived ~., data=data_train, cp=-1, maxdepth=5)
rpart.plot(train.tree_9,type=4,extra=1)

# AR value of train 
train.prob9 <- predict(train.tree_9, newdata=data_train, type="prob")
source("./lib/calcAR.R")
calcAR(X=train.prob9[, 2], y=data_train$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)

# AR value of valid
valid.prob9 <- predict(train.tree_9, newdata=data_valid, type="prob")
calcAR(X=valid.prob9[, 2], y=data_valid$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)


# remodeling using all train 
train.tree_9 <- rpart(Survived ~., data=train, cp=-1, maxdepth=5)
rpart.plot(train.tree_9,type=4,extra=1)
train.prob9 <- predict(train.tree_9, newdata=data_train, type="prob")
source("./lib/calcAR.R")
calcAR(X=train.prob9[, 2], y=data_train$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)



# my prediction write to csv 
test <- categolizeTicket(test)
test <- categolizeCabin(test)
test$Survived <- predict(train.tree_9, newdata=test, type="class")

test.df <- data.frame(PassengerId=test$PassengerId,Survived=test$Survived)
row.names(test.df) <- 1:nrow(test.df)
write.csv(test.df,file="./data/20140301_3_predict.csv",row.names=FALSE)

