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


train$Ticket[1:10]
train$Ticket_factor <- as.factor(substr(train$Ticket, 1, 4))
sort(unique(train$Ticket_factor))
#=> A./5 A.5. A/4  A/4. A/5  A/5. A/S  A4.  C 17 C 40 C 70 C.A. CA 2 CA.
#   F.C. Fa 2 LINE P/PP PC 1 PP 4 PP 9 S.C. S.O. S.P. S.W. SC 1 SC/A SC/P SCO/ 
#   SO/C SOTO STON SW/P W./C W.E. W/C  WE/P

# similarity factores make grouping more

# A./5, A.5. A/5 => A5
# A/4  A/4.  A4. => A4
# C.A. CA 2 CA.  => CA
# P/PP PP 4 PP 9 => PP
# S.C. SC 1 SC/A SC/P => SC
# SO/C SOTO => SO
# W./C W/C => WC
# W.E. WE/P => WE

  
train$gsub_ticket <- gsub(pattern="\\.", replacement="", train$Ticket)
train$gsub_ticket <- gsub("/", "", train$gsub_ticket)
train$Ticket_factor <- as.factor(substr(train$gsub_ticket, 1,2))
sort(unique(train$Ticket_factor))
#=> Ticket_factor has 53 levels

# remove original Ticket, gsub_ticket
train$Ticket = NULL
train$gsub_ticket = NULL

categolizeTicket <- function(df){
  df$gsub_ticket <- gsub(pattern="\\.", replacement="", df$Ticket)
  df$gsub_ticket <- gsub("/", "", df$gsub_ticket)
  df$Ticket_factor <- as.factor(substr(df$gsub_ticket, 1,2))
  df$Ticket = NULL
  df$gsub_ticket = NULL
  return(df)
}




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
data_train <- train[1:700,]
data_valid <- train[701:891,]

data_train.nRow <- nrow(train)
data_valid.nRow <- nrow(train)

# 9th train tree model 
train.tree_9 <- rpart(Survived ~., data=data_train, cp=-1, maxdepth=5)
#rpart.plot(train.tree_9,type=4,extra=1)
# summary(train.tree_9)

# AR value of train 
train.prob9 <- predict(train.tree_9, newdata=data_train, type="prob")
source("./lib/calcAR.R")
calcAR(X=train.prob9[, 2], y=data_train$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)

# AR value of valid
valid.prob9 <- predict(train.tree_9, newdata=data_valid, type="prob")
calcAR(X=valid.prob9[, 2], y=data_valid$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)



# my prediction write to csv 
test <- categolizeTicket(test)

# raise this error in test data 
# " factor Ticket_factor has new levels 68, 79, A , AQ, LP"
summary(train$Ticket_factor)
# "34" is the biggest level.
# set these new levels to "34"
new_level_index <- grep("^68|79|A |AQ|LP$", test$Ticket_factor)
test$Ticket_factor[new_level_index] <- "34"

test <- categolizeCabin(test)
test$Survived <- predict(train.tree_9, newdata=test, type="class")

test.df <- data.frame(PassengerId=test$PassengerId,Survived=test$Survived)
row.names(test.df) <- 1:nrow(test.df)
write.csv(test.df,file="./data/20140301_predict.csv",row.names=FALSE)
