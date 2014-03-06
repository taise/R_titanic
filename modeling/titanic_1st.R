# Titanic
# at 2014/01/03

library("rpart")
library("rpart.plot")

setwd("~/development/R/kaggle/titanic")
data.all <- read.csv("./data/train.csv", header=TRUE)
test <- read.csv("./data/test.csv", header=TRUE)

ncol(data.all)
nrow(data.all)

data.train <- data.all[1:700,]
data.valid <- data.all[701:891,]

names(data.train)
summary(data.train)
str(data.train)
data.train[1:10,]

# 変数名  値	説明
# survival	0 = No; 1 = Yes	生存したか否か．予測変数
# pclass	1 = 1st; 2 = 2nd; 3 = 3rd	客室のグレード
# name	文字列	乗客名
# sex	male = 男性; female = 女性	性別
# age	0以上の数字	年齢．小数点以下の値があるものは推測値．欠損あり
# sibsp	0以上の数字	国外に住む兄弟または配偶者の数
# parch	0以上の数字	国外に住む親または子どもの数
# ticket	英数字	チケット番号
# fare	0以上の数字	運賃
# cabin	文字列	客室番号．欠損多数
# embarked	C = Cherbourg; Q = Queenstown; S = Southampton	乗船した港

# Survivalをfactorに変更する
data.train$Survived = factor(data.train$Survived)
data.valid$Survived = factor(data.valid$Survived)

# 分析に使わないデータは削除する
data.train$PassengerId = NULL
data.train$Name = NULL
data.train$Ticket = NULL

ncol(data.train)
plot(data.train[1:4])
plot(data.train[c(1, 5:8)])

# 一定の結果得るために乱数セット
set.seed(2)

train.tree_1 <- rpart(Survived ~., data=data.train)
train.tree_1
par(ps=20) #フォント調整
rpart.plot(train.tree_1,type=4,extra=1)

# Cabinデータがわからないので、今回はNULLをいれる
data.train[1:20,]$Cabin
data.train$Cabin = NULL

train.tree_2 <- rpart(Survived ~., data=data.train)
train.tree_2
par(ps=40) #フォント調整
rpart.plot(train.tree_2,type=4,extra=1)


# それっぽいモデル
train.tree_3 <- rpart(Survived ~., data=data.train, cp=-1, maxdepth=3)
train.tree_3
par(ps=20) #フォント調整
rpart.plot(train.tree_3,type=4,extra=1)

# 検証(学習データ)
train.prob3 <- predict(train.tree_3, newdata=data.train, type="prob")
source("./lib/calcAR.R")
calcAR(X=train.prob3[, 2], y=data.train$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)

# 検証(テストデータ)
valid.prob3 <- predict(train.tree_3, newdata=data.valid, type="prob")
calcAR(X=valid.prob3[, 2], y=data.valid$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)

# 書き出し
test <- read.csv(file="./data/test.csv",header=T,stringsAsFactors=FALSE)
test$Survived <- predict(train.tree_3, newdata=test, type="class")

test.df <- data.frame(PassengerId=test$PassengerId,Survived=test$Survived)
row.names(test.df)<-1:nrow(test.df)
#write.csv(test.df,file="./data/20140103/predict.csv",row.names=FALSE)
