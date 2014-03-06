# Titanic
# at 2014/01/04

### 改善ポイント検討
# 
# 1. 説明変数の欠損値
#   Age, Cabinの欠損が多い
#   (1). それぞれの説明変数がどれだけ決定木に影響を与えているか
#   (2). 影響が大きければ欠損値を埋める/削る判断をする
#   (3). 修正したデータでモデル作る
#
# 2. AR値の更なる改善 
#
# AR: 2014/01/03 data_train$Survived
# 0 vs. 1 0.7088828
#
# AR: 2014/01/03 data_valid$Survived
# 0 vs. 1 0.7776995
# 
# 検証データの方が結果が0.07も向上している
#  => もう少しモデルをフィットさせても良さそう
#

library("rpart")
library("rpart.plot")

setwd("~/development/R/kaggle/titanic")
data.all <- read.csv("./data/train.csv", header=TRUE)
test <- read.csv("./data/test.csv", header=TRUE)

ncol(data.all)
nrow(data.all)

# Survivalをfactorに変更する
data.all$Survived = factor(data.all$Survived)

data_train <- data.all[1:700,]
data_valid <- data.all[701:891,]

data_train.nRow <- nrow(data_train)
data_valid.nRow <- nrow(data_valid)


names(data_train)
summary(data_train)
str(data_train)

# http://smrmkt.hatenablog.jp/entry/2013/01/04/192628
# 変数名  値  説明
# survival	0 = No; 1 = Yes	生存したか否か．予測変数
# pclass	1 = 1st; 2 = 2nd; 3 = 3rd	客室のグレード
# name	文字列	乗客名
# age	0以上の数字	年齢．小数点以下の値があるものは推測値．欠損あり
# sibsp	0以上の数字	国外に住む兄弟または配偶者の数
# parch	0以上の数字	国外に住む親または子どもの数
# fare	0以上の数字	運賃
# cabin	文字列	客室番号．欠損多数
# embarked	C = Cherbourg; Q = Queenstown; S = Southampton	乗船した港


# http://homepage1.nifty.com/Titanic/real/aboutTitanic.htm
# 分析に使わないデータは削除する
data_train$PassengerId = NULL
data_train$Name = NULL
data_train$Ticket = NULL

# データの中身を集計する
sapply(data_train, table, exclude=NULL)

# 一定の結果得るために乱数セット
set.seed(2)

train.tree_1 <- rpart(Survived ~., data=data_train)
train.tree_1
par(ps=20) #フォント調整
rpart.plot(train.tree_1,type=4,extra=1)
# CabinとAgeが説明変数として使われているので欠損値を対応する

##############################################################
# Cabinを再カテゴリ化
##############################################################
Cabins <- data.frame()
for( i in c(1:data_train.nRow) ) {
  if (data_train[i,"Cabin"] != "") { 
    Cabins <- rbind(Cabins, data_train[i,c("Survived", "Cabin")])
  }  
}
plot(Cabins$Cabin, Cabins$Survived)

# 客室番号の数字を取り除いてA~Tの数値にする
# 1カラムに"Bxxx Bxxx"というものもあるけど気にしない 
Cabins$CabinGroup <- factor(substr(Cabins$Cabin, 1, 1))
table(Cabins$CabinGroup)

cabinPattern <- sort(unique(Cabins$CabinGroup)) # 使わなかった・・・
plot(Cabins$CabinGroup, Cabins$Survived)

# Cabin A ~ Tによって生存率にばらつきがあるので、カテゴリ変数を追加することにする
data_train <- transform(data_train, CabinGroup=substr(Cabin, 1 ,1))
data_train$CabinGroup <- factor(data_train$CabinGroup)

# 検証データにも
data_valid <- transform(data_valid, CabinGroup=substr(Cabin, 1 ,1))
data_valid$CabinGroup <- factor(data_valid$CabinGroup)


# Cabinを改善したモデル
train.tree_4 <- rpart(Survived ~., data=data_train, cp=-1, maxdepth=3)
rpart.plot(train.tree_4,type=4,extra=1)

# 検証(学習データ)
train.prob4 <- predict(train.tree_4, newdata=data_train, type="prob")
source("./lib/calcAR.R")
calcAR(X=train.prob4[, 2], y=data_train$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)

# 検証(テストデータ)
valid.prob4 <- predict(train.tree_4, newdata=data_valid, type="prob")
calcAR(X=valid.prob4[, 2], y=data_valid$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)

# 結果が悪化したが、前回のモデルで使われていたPClassが使えていない
# maxdepthを増やすと改善するかも


##############################################################
# Ageの値を推定する
##############################################################

# Ageの中身を見てみる
table(data_train$Age, exclude=NULL)
# 小数点は丸める(round関数は四捨五入じゃない!)
# http://cse.naro.affrc.go.jp/takezawa/r-tips/r/37.html
data_train$ageInt <- round(data_train$Age)
hist(data_train$ageInt)
data_train$ageInt = NULL

# 子供料金があったかもしれないので運賃を確かめてみる
plot(data_train$ageInt, data_train$Fare)
summary(data_train$ageInt)
data_train$ageGroup <- cut(data_train$ageInt, breaks = seq(0, 80, by = 10))
plot(data_train$ageGroup, data_train$Fare)

# 飛び抜けた外れ値を消す
table(data_train$Fare, exclude=NULL)

plotdata <- data.frame()
for(i in c(1:data_train.nRow)) {
  if(data_train[i,"Fare"] > 500) {
    print(i)
    print(data_train[i,"Fare"])
  } else {
    plotdata <- rbind(plotdata, data_train[i,])
  }
}
summary(plotdata$Fare)
plot(plotdata$ageGroup, plotdata$Fare)

# 子供料金はなさそうだが、30歳未満は中央値が低いことがわかる
# 40代以上は箱ひげ図の上側が、つまり運賃が高くなっている
# うまい方法が見つからないので、別の方法を試す
# 線形モデルを考えてみてもいいかも

# 今回は等級と年齢の関係を見てみる
table(plotdata$Pclass)
plot(plotdata$ageGroup, plotdata$Pclass)
# 30歳以下は3等寄り、31~50歳は2等が中央値で1~3等バランスよい,51歳以上は1等寄り
# 今回はざっくりと年齢を当てはめる
# 3等は15歳、2等は40歳、1等は60歳
data_train$fillAge <- round(data_train$Age)
for(i in c(1:data_train.nRow)) {
  if(is.na(data_train[i,"fillAge"])) {
    switch(data_train[i,"Pclass"],
           "1" = data_train[i, "fillAge"] <- 60,
           "2" = data_train[i, "fillAge"] <- 40,
           "3" = data_train[i, "fillAge"] <- 15,
           print(i) # Pclassがなかった場合
           )
  }
}

# validでもやる
data_valid$fillAge <- round(data_valid$Age)
for(i in c(1:data_valid.nRow)) {
  if(is.na(data_valid[i,"fillAge"])) {
    switch(data_valid[i,"Pclass"],
           "1" = data_valid[i, "fillAge"] <- 60,
           "2" = data_valid[i, "fillAge"] <- 40,
           "3" = data_valid[i, "fillAge"] <- 15,
           print(i) # Pclassがなかった場合
    )
  }
}

# 欠損値が埋まったのでageGroupを作り直す
data_train$ageGroup <- cut(data_train$fillAge, breaks = seq(0, 80, by = 10))
data_valid$ageGroup <- cut(data_valid$fillAge, breaks = seq(0, 80, by = 10))

# 当然だが、15歳が飛び抜けて多くなっている・・・
table(data_train$fillAge, exclude=NULL)


# Ageを改善したモデル
train.tree_5 <- rpart(Survived ~., data=data_train, cp=-1, maxdepth=3)
rpart.plot(train.tree_4,type=4,extra=1)

# 検証(学習データ)
train.prob5 <- predict(train.tree_5, newdata=data_train, type="prob")
source("./lib/calcAR.R")
calcAR(X=train.prob4[, 2], y=data_train$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)

# 検証(テストデータ)
valid.prob5 <- predict(train.tree_5, newdata=data_valid, type="prob")
calcAR(X=valid.prob5[, 2], y=data_valid$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)


##############################################################
# maxdepthを増やす
##############################################################

# カテゴリも増えたので、それも使って推定する
# maxdepth=5でやったらoverfittingっぽい
train.tree_6 <- rpart(Survived ~., data=data_train, cp=-1, maxdepth=4)
rpart.plot(train.tree_6,type=4,extra=1)

# 検証(学習データ)
train.prob6 <- predict(train.tree_6, newdata=data_train, type="prob")
source("./lib/calcAR.R")
calcAR(X=train.prob4[, 2], y=data_train$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)

# 検証(テストデータ)
valid.prob6 <- predict(train.tree_6, newdata=data_valid, type="prob")
calcAR(X=valid.prob6[, 2], y=data_valid$Survived, TARGET="1", plotCAP=TRUE, plotpr=TRUE)



# 書き出し
test <- read.csv(file="./data/test.csv",header=T,stringsAsFactors=FALSE)

# カテゴリを追加する
### CabinGroup
test <- transform(test, CabinGroup=substr(Cabin, 1 ,1))
test$CabinGroup <- factor(test$CabinGroup)

### fillAge
test.nRow <- nrow(test)
test$fillAge <- round(test$Age)
for(i in c(1:test.nRow)) {
  if(is.na(test[i,"fillAge"])) {
    switch(test[i,"Pclass"],
           "1" = test[i, "fillAge"] <- 60,
           "2" = test[i, "fillAge"] <- 40,
           "3" = test[i, "fillAge"] <- 15,
           print(i) # Pclassがなかった場合
    )
  }
}

### ageGroup
test$ageGroup <- cut(test$fillAge, breaks = seq(0, 80, by = 10))


test$Survived <- predict(train.tree_6, newdata=test, type="class")

test.df <- data.frame(PassengerId=test$PassengerId,Survived=test$Survived)
row.names(test.df)<-1:nrow(test.df)
write.csv(test.df,file="./data/20140104/predict.csv",row.names=FALSE)
