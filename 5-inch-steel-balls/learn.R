
source("prepare.R")

library(caret)
library(randomForest)

library(doMC)
registerDoMC(25)

trainIndex <- createDataPartition(
  training$LABEL,
  p = .75,
  list = FALSE,
  times = 1)

train_data <- training[ trainIndex,]
test_data  <- training[-trainIndex,]

set.seed(1337)
ctrl <- trainControl(
  method          = "repeatedcv",
  number          = 10,
  repeats         = 1,
  search          = "random",
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter     = TRUE
)

modelTune <- train(LABEL ~ ., data = train_data, 
                   method = "rf",          
                   metric = "ROC",
                   na.action = na.pass,
                   preProcess = c("center", "scale", "medianImpute", "nzv"),
                   trControl = ctrl,
                   tuneLength = 10)        



resultsTest <- predict(modelTune,test_data, na.action = na.pass, type = "prob")
resultsTest$obs <- test_data$LABEL
resultsTest$pred <- predict(modelTune,test_data, na.action = na.pass)

print(confusionMatrix(resultsTest$pred,resultsTest$obs))

varImp(modelTune)

saveRDS(modelTune, "~/model.rds")

