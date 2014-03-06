# Titanic
# at 2014/01/05

### 改善ポイント検討
# 
# 1. Cabinの欠損を他の説明変数を使って補ってみる
# 
# 2. Ageの追加の仕方を複数の説明変数を使って補ってみる
# # やらずに終わった


#### データ事前準備

library("rpart")
library("rpart.plot")

setwd("~/development/R/kaggle/titanic")
data_all <- read.csv("./data/train.csv", header=TRUE)
test <- read.csv("./data/test.csv", header=TRUE)

ncol(data_all)
(data_all.nRow <- nrow(data_all))

# Survivalをfactorに変更する
data_all$Survived = factor(data_all$Survived)

# 分析に使わないデータは削除する
data_all$PassengerId = NULL
data_all$Name = NULL
data_all$Ticket = NULL

data_train <- data_all[1:700,]
data_valid <- data_all[701:891,]

data_train.nRow <- nrow(data_train)
data_valid.nRow <- nrow(data_valid)


##################################################################
# 1. Cabinの欠損を他の説明変数を使って補ってみる
##################################################################

# PclassとCabinの関係を見てみる
plot(data_train$CabinGroup, data_train$Pclass)
table(data_train$CabinGroup, data_train$Pclass)

# A~Eまでは1等、Fは2等が多め、Gは3等が多め
# とはいえ、F,G,Tはサンプルが少なすぎて参考にできない
# 客室等級に応じた仮の客室(X~Z)で欠損を埋める 

fillCabinGroup <- function(df) {
  data_frame <- df
  nRow <- nrow(data_frame)
  for(i in c(1:nRow)) {
    data_frame[i, "fillCabinGroup"] <- ifelse(data_frame[i, "Cabin"] == "" ,
                                              switch(data_frame[i, "Pclass"],"X", "Y", "Z"),
                                              substr(data_frame[i, "Cabin"] , 1 ,1)
    )
  }
  data_frame$fillCabinGroup <- factor(data_frame$fillCabinGroup)
  return(data_frame)
}
data_train <- fillCabinGroup(data_train)

str(data_train$fillCabinGroup)
table(data_train$fillCabinGroup, exclude = NULL)
table(data_train$fillCabinGroup, data_train$Survived,exclude = NULL)
plot(data_train$fillCabinGroup, data_train$Survived)


# Cabinを消す
data_train$Cabin = NULL
data_validCabin = NULL

# Cabinを改善したモデル
train.tree_7 <- rpart(Survived ~., data=data_train, cp=-1, maxdepth=4)
rpart.plot(train.tree_7,type=4,extra=1)

# 学習データのAR値
train.prob7 <- predict(train.tree_7, newdata=data_train, type="prob")
source("./lib/calcAR.R")
calcAR(X=train.prob7[, 2], y=data_train$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)

#検証データのAR値
data_valid <- fillCabinGroup(data_valid)
valid.prob7 <- predict(train.tree_7, newdata=data_valid, type="prob")
calcAR(X=valid.prob7[, 2], y=data_valid$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)

# 検証データのほうのAR値がグンと上がったので、もう少し深く探索させる
train.tree_8 <- rpart(Survived ~., data=data_train, cp=-1, maxdepth=5)
rpart.plot(train.tree_8,type=4,extra=1)

# 学習データのAR値
train.prob8 <- predict(train.tree_8, newdata=data_train, type="prob")
source("./lib/calcAR.R")
calcAR(X=train.prob8[, 2], y=data_train$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)

#検証データのAR値
valid.prob8 <- predict(train.tree_8, newdata=data_valid, type="prob")
calcAR(X=valid.prob8[, 2], y=data_valid$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)



# 書き出し
test <- fillCabinGroup(test)
test$Survived <- predict(train.tree_8, newdata=test, type="class")

test.df <- data.frame(PassengerId=test$PassengerId,Survived=test$Survived)
row.names(test.df)<-1:nrow(test.df)
write.csv(test.df,file="./data/20140105/predict.csv",row.names=FALSE)

