
library(caret)

ctrl <- trainControl(method = "repeatedcv",
  repeats = 10,
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary)

fitglmnet <- train(x = tr.m, y = yFac.m,
    method = 'glmnet',
    trControl = ctrl,
    preProc = c("center", "scale"),
    metric = 'ROC')



glmfit <- predict(fitglmnet, newdata = te.m)

subglm <- cbind(test$Id, glmfit)
colnames(subglm) <- c("Id","WnvPresent")
options("scipen" = 100, "digits" = 8)

filenameglm <- 'subs/glm1sub150428.csv'
write.csv(subglm, filenameglm, row.names = FALSE, quote = FALSE)


# not finished
sub <- function(model, test, name){
  fit <- predict(model, newdata = test)

  subglm <- cbind(test$Id, glmfit)
  colnames(subglm) <- c("Id","WnvPresent")
  options("scipen" = 100, "digits" = 8)

  filenameglm <- 'subs/glm1sub150428.csv'
  write.csv(subglm, filenameglm, row.names = FALSE, quote = FALSE)
}

