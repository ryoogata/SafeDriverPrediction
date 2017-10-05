# 共通ライブラリ
require(caret)
require(caretEnsemble)
require(doParallel)
require(dplyr)

# 固有ライブラリ
require(ranger)

source("script/R/fun/tools.R", encoding = "utf8")
source("script/R/fun/normalizedGini.R", encoding = "utf8")

# 保存した結果の読み込み
RESULT_DF <- "result.ranger.df"

if ( file.exists(paste0("result/", RESULT_DF, ".data")) ) {
  assign(RESULT_DF, readRDS(paste0("result/", RESULT_DF, ".data")))
}


#
# 前処理
#
source("script/R/fun/Data-pre-processing.R", encoding = "utf8")

#my_preProcess <- c("center", "scale")
my_preProcess <- NULL

data_preProcess <- "none"

if ( data_preProcess == "none") {
  TRAIN <- all.train
  TRAIN.TRAIN <- train.train
  TRAIN.TEST <- train.test
  TEST <- test
} else if ( data_preProcess == "nzv") {
  TRAIN <- all.nzv.train
  TRAIN.TRAIN <- train.nzv.train
  TRAIN.TEST <- train.nzv.test
  TEST <- test
} else if ( data_preProcess == "dummy") {
  TRAIN <- all.train.dummy
  TRAIN.TRAIN <- train.dummy.train
  TRAIN.TEST <- train.dummy.test
  TEST <- test.dummy
} else if ( data_preProcess == "dummy.nzv.highlyCorDescr") {
  TRAIN <- train.dummy.nzv.highlyCorDescr
  TRAIN.TRAIN <- train.dummy.nzv.highlyCorDescr.train
  TRAIN.TEST <- train.dummy.nzv.highlyCorDescr.test
  TEST <- test.dummy.nzv.highlyCorDescr
}


#
# ranger
#

# 変数指定 ( 共通設定 )
nresampling <- 10
n_repeats <- 10
METHOD <- "cv" # "repeatedcv", "boot"

# 変数指定 ( モデル個別 )
MTRY <- c(5:58)

# seeds の決定
set.seed(123)
SEEDS <- vector(mode = "list", length = n_repeats * nresampling)
for(i in 1:(n_repeats * nresampling)) SEEDS[[i]] <- sample.int(1000, length(MTRY))
SEEDS[[n_repeats * nresampling + 1]] <- sample.int(1000, 1)

# 再抽出方法毎に INDEX を作成
set.seed(123)
if ( METHOD == "cv" ) {
  INDEX <- createFolds(TRAIN.TRAIN$response, k = nresampling, returnTrain = TRUE)
} else if ( METHOD == "repeatedcv" ){
  INDEX <- createMultiFolds(TRAIN.TRAIN$response, k = nresampling, times = n_repeats)
} else if ( METHOD == "boot" ){
  INDEX <- createResample(TRAIN.TRAIN$response, n_repeats)
}

set.seed(123)
doParallel <- trainControl(
  method = METHOD
  ,number = nresampling
  ,repeats = n_repeats
  ,classProbs = TRUE
  ,allowParallel = TRUE
  ,verboseIter = TRUE
  #,summaryFunction = twoClassSummary
  ,summaryFunction = normalizedGini
  ,savePredictions = "final"
  ,index = INDEX
  ,seeds = SEEDS
)


# 説明変数一覧の作成
explanation_variable <- names(subset(TRAIN, select = -c(id, response)))

cl <- makeCluster(detectCores(), type = 'PSOCK', outfile = " ")
registerDoParallel(cl)

model_list <- caretList(
  x = TRAIN.TRAIN[,explanation_variable]
  ,y = TRAIN.TRAIN$response
  ,trControl = doParallel
  ,preProcess = my_preProcess
  ,tuneList = list(
    fit = caretModelSpec(
      method = "ranger"
      ,metric = "Gini" 
      ,tuneGrid = expand.grid(
        mtry = MTRY
      )
      ,importance = 'impurity'
    )
  )
)

stopCluster(cl)
registerDoSEQ()

model_list[[1]]$times
model_list[[1]]$finalModel

#
# モデル比較
#
if (is.null(model_list[[1]]$preProcess)){
  # preProcess を指定していない場合
  TRAIN.SELECTED <- TRAIN.TEST 
} else {
  # preProcess を指定している場合
  TRAIN.SELECTED <- preProcess(
    subset(TRAIN.TEST, select = explanation_variable)
    ,method = my_preProcess
  ) %>%
    predict(., subset(TRAIN.TEST, select = explanation_variable))
}

allPrediction <- caret::extractProb(
                              list(model_list[[1]])
                              ,testX = subset(TRAIN.SELECTED, select = explanation_variable)
                              ,testY = unlist(subset(TRAIN.TEST, select = c(response)))
                 )

# dataType 列に Test と入っているもののみを抜き出す
testPrediction <- subset(allPrediction, dataType == "Test")
tp <- subset(testPrediction, object == "Object1")

# 精度確認 ( 正規化ジニ係数: Normalized Gini Coefficient )
normalizedGini(tp)

# 結果の保存
if (exists(RESULT_DF)){
  assign(RESULT_DF, dplyr::bind_rows(list(eval(parse(text = RESULT_DF)), summaryResult.normalizedGini(model_list[[1]]))))
} else {
  assign(RESULT_DF, summaryResult.normalizedGini(model_list[[1]]))
}
saveRDS(eval(parse(text = RESULT_DF)), paste0("result/", RESULT_DF, ".data"))


# predict() を利用した検算 
#pred_test.verification <- predict(model_list[[1]], TRAIN.SELECTED, type="prob")
pred_test.verification <- predict(model_list[[1]], TRAIN.SELECTED, type="raw")

# 精度確認 (  )
normalizedGini(data.frame(obs = tp$obs, pred = pred_test.verification))

# 学習用データ全てを利用してデルを作成
finalModel <- ranger(formula = as.formula(paste("response", "~" , paste(explanation_variable, collapse = "+")))
      ,data = TRAIN
      ,mtry = as.numeric(model_list[[1]]$bestTune["mtry"])
      ,probability = TRUE
      ,importance="impurity"
      )


#
# 評価用データにモデルの当てはめ
#
if (is.null(model_list[[1]]$preProcess)){
  # preProcess を指定していない場合
  pred_test <- predict(object = finalModel, data = TEST, type="response")
  pred_test <- pred_test$predictions[,"yes"]
  
  PREPROCESS <- "no_preProcess"
} else {
  # preProcess を指定している場合
  PREPROCESS <- preProcess(
      subset(TEST, select = explanation_variable)
      ,method = my_preProcess
    ) %>%
      predict(., subset(TEST, select = explanation_variable))
    
  pred_test <- predict(object = finalModel, data = PREPROCESS, type="response")
  pred_test <- pred_test$predictions[,"yes"]
  
  PREPROCESS <- paste(my_preProcess, collapse = "_")
}


#submitの形式で出力(CSV)
#データ加工
out <- data.frame(TEST$id, pred_test)
names(out) <- c("id", "target")

sapply(out, function(x) sum(is.na(x)))

# 予測データを保存
for(NUM in 1:10){
  DATE <- format(jrvFinance::edate(from = Sys.Date(), 0), "%Y%m%d")
  SUBMIT_FILENAME <- paste("./submit/submit_", DATE, "_", NUM, "_", PREPROCESS, "_", model_list[[1]]$method, ".csv", sep = "")
  
  if ( !file.exists(SUBMIT_FILENAME) ) {
    write.table(out, #出力データ
                SUBMIT_FILENAME, #出力先
                quote = FALSE, #文字列を「"」で囲む有無
                col.names = TRUE, #変数名(列名)の有無
                row.names = FALSE, #行番号の有無
                sep = "," #区切り文字の指定
    )
    break
  }
}
