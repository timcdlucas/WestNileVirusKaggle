
library(caret)

ctrl <- trainControl(method = "repeatedcv",
  repeats = 5,
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary)

fitglmnet <- train(x = cbind(tr.m, train$Latitude, train$Longitude), y = yFac.m,
    method = 'glmnet',
    trControl = ctrl,
    preProc = c("center", "scale", 'knnImpute'),
    metric = 'ROC')


x = cbind(te.m, test$Latitude, test$Longitude)


glmfit <- predict(fitglmnet, newdata = x)

subglm <- cbind(test$Id, glmfit)
colnames(subglm) <- c("Id","WnvPresent")
options("scipen" = 100, "digits" = 8)

filenameglm <- 'subs/glm1sub150428.csv'
write.csv(subglm, filenameglm, row.names = FALSE, quote = FALSE)


# not finished
sub <- function(model, test, name){
  fit <- predict(model, newdata = test)

  sub <- cbind(test$Id, fit)
  colnames(sub) <- c("Id","WnvPresent")
  options("scipen" = 100, "digits" = 8)

  filename <- paste0('subs/', 'name', sys.Date(), '.csv')
  write.csv(sub, filename, row.names = FALSE, quote = FALSE)
}



# See how long a deep net from caret takes to train


ctrl <- trainControl(method = "repeatedcv",
  repeats = 5,
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary)

# Don't think this is actually 'deep'

system.time({
fitglmnet <- train(x = cbind(tr.m, train$Latitude, train$Longitude), y = yFac.m,
    method = 'mlp',
    trControl = ctrl,
    preProc = c("center", "scale", 'knnImpute'),
    metric = 'ROC')
})


# This one is deep. Don't know how it compares to h20
# Ok its quick.

dnnGrid <-  expand.grid(layer1 = c(5, 10, 15), 
                        layer2 = c(5, 10, 15), 
                        layer3 = c(5, 10, 15), 
                        hidden_dropout = c(0.1, 0.3, 0.5), 
                        visible_dropout = c(0.1, 0.3, 0.5))



fitdeep <- train(x = cbind(tr.m, la = train$Latitude, lo = train$Longitude), y = yFac.m,
    method = 'dnn',
    trControl = ctrl,
    preProc = c("center", "scale", 'knnImpute'),
    metric = 'ROC',
    tuneGrid = dnnGrid)


sub(fitdeep, cbind(te.m, la = test$Latitude, lo = test$Longitude), 'deepnet')
	


sub <- function(model, test, name, Id = test$Id){
  fit <- predict(model, newdata = test, type = 'prob')

  sub <- cbind(Id, fit)
  colnames(sub) <- c("Id","WnvPresent")
  options("scipen" = 100, "digits" = 8)

  filename <- paste0('subs/', name, Sys.Date(), '.csv')
  write.csv(sub, filename, row.names = FALSE, quote = FALSE)
}
