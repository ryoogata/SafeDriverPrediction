require(caret)
require(dplyr)

# 指数表示の回避
options(scipen=10)

#
# データの読み込み
#
all <- data.table::fread("./data/div10_train.csv"
#all <- data.table::fread("./data/train.csv"
                         ,stringsAsFactors = FALSE
                         ,sep = ","
                         ,data.table = FALSE
                         ,encoding = "UTF-8"
                         )


test <- data.table::fread("./data/test.csv"
                         ,stringsAsFactors = FALSE
                         ,sep = ","
                         ,data.table = FALSE
                         ,encoding = "UTF-8"
)


# 目的変数名を response に変更
names(all)[names(all) == "target"] <- "response"

# 目的変数の factor 化
all[all$response == 1,"response"] <- "yes"
all[all$response == 0,"response"] <- "no"
#all[which(all$data == "test"),"y"] <- "none"
all$response <- as.factor(all$response)


#
# Dummy 変数なし
#

# all から train/test のデータを抽出
all.train <- all

# 再現性のため乱数シードを固定
set.seed(100)

# 訓練データと検証データに分割する
# Train 用の列番号を作成
trainIndex <- caret::createDataPartition(all.train$response, p = .8, 
                                         list = FALSE, 
                                         times = 1)
train.train <- all.train[trainIndex, ]
train.test <- all.train[-trainIndex,]

# 分割したデータの保存
# write.table(
#   all #出力データ
#   ,"./data/div10_train.csv" #出力先
#   ,quote = FALSE #文字列を「"」で囲む有無
#   ,col.names = TRUE #変数名(列名)の有無
#   ,row.names = FALSE #行番号の有無
#   ,sep = "," #区切り文字の指定
# )