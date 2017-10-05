require(caret)
require(dplyr)

# 指数表示の回避
options(scipen=10)

#
# データの読み込み
#
all <- data.table::fread("~/Desktop/train.csv"
                         ,stringsAsFactors = FALSE
                         ,sep = ","
                         ,data.table = FALSE
                         ,encoding = "UTF-8"
                         )


test <- data.table::fread("./data/test20170913.csv"
                         ,stringsAsFactors = FALSE
                         ,sep = ","
                         ,data.table = FALSE
                         ,encoding = "UTF-8"
)


# 目的変数名を response に変更
names(all)[names(all) == "target"] <- "response"

# 文字列を factor に変換
#variable.is.character <- sapply(all, is.character)
#variable.character <- names(variable.is.character[variable.is.character == TRUE])
#all <- dplyr::mutate_at(all, variable.character, funs(factor)) 

#variable.is.character <- sapply(test, is.character)
#variable.character <- names(variable.is.character[variable.is.character == TRUE])
#test <- dplyr::mutate_at(test, variable.character, funs(factor)) 

#
# Dummy 変数なし
#

# all から train/test のデータを抽出
all.train <- all
# all.train$response <- as.factor(all.train$response)
# all.test <- all[which(all$data == "test"),]

# 再現性のため乱数シードを固定
#set.seed(10)
set.seed(100)

# 訓練データと検証データに分割する
# Train 用の列番号を作成
trainIndex <- caret::createDataPartition(all.train$response, p = .8, 
                                  list = FALSE, 
                                  times = 1)
train.train <- all.train[trainIndex, ]
train.test <- all.train[-trainIndex,]

# dim(train.train)
# [1] 476170     59

# dim(train.test)
# [1] 119042     59

#
# Dummy 変数化
#
noNames <- caret::dummyVars(~., data = subset(all, select = -c(id, response, date)), fullRank = FALSE  )
all.dummy <- as.data.frame(predict(noNames, all))
all.dummy <- data.frame(id = all$id, date = all$date, all.dummy, response = all$response)

#
# Dummy ( 前処理なし )
#


# all から train/test のデータを抽出
all.train.dummy <- all.dummy

# 再現性のため乱数シードを固定
set.seed(100)

# 訓練データと検証データに分割する
# Train 用の列番号を作成
trainIndex <- caret::createDataPartition(all.train.dummy$response, p = .8, 
                                  list = FALSE, 
                                  times = 1)
train.dummy.train <- all.train.dummy[trainIndex, ]
train.dummy.test <- all.train.dummy[-trainIndex,]

dim(train.dummy.train)
# [1] 1682    18

dim(train.dummy.test)
# [1] 419   18

noNames <- caret::dummyVars(~., data = subset(test, select = -c(id, date)), fullRank = FALSE  )
test.dummy <- as.data.frame(predict(noNames, test))
test.dummy <- data.frame(id = test$id, date = test$date, test.dummy)
