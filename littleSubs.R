
library(caret)
class2ind <- function(cl)
{
        n <- length(cl)
        cl <- as.factor(cl)
        x <- matrix(0, n, length(levels(cl)) )
        x[(1:n) + n*(unclass(cl)-1)] <- 1
        dimnames(x) <- list(names(cl), levels(cl))
        x
}



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






# Using data from half way through data.sort

ctrl <- trainControl(method = "repeatedcv",
  repeats = 5,
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary)



glmnetgrid <- expand.grid(lambda = 0.01^(seq(1, 4, length.out = 20)),
                          alpha = seq(0, 1, length.out = 5))

fitglmnet <- train(x = tr.m, y = yFac.m,
    method = 'glmnet',
    trControl = ctrl,
    preProc = c("center", "scale", 'medianImpute'),
    metric = 'ROC', 
    tuneGrid = glmnetgrid)

fitglmnet

plot(fitglmnet$results$alpha ~ fitglmnet$results$lambda, pch = 16, 
  col =  rgb(colorRamp(c('blue', 'green'))(fitglmnet$results$ROC), maxColorValue = 255))

  fit <- predict(fitglmnet, newdata = te.m, type = 'prob')
  fit <- predict(fitglmnet$finalModel, newx = te.m, type = 'response')

  sub <- cbind(test$Id, fit[,2])
  colnames(sub) <- c("Id","WnvPresent")
  options("scipen" = 100, "digits" = 8)

  filename <- paste0('subs/', 'yearMean', Sys.Date(), '.csv')
  write.csv(sub, filename, row.names = FALSE, quote = FALSE)


x <- tr.m[,-c(7, 8)]
x2 <- te.m[, -c(7, 8)]

g=cv.glmnet(x=x, y=yFac.m, family='binomial', type.measure = 'auc', parallel = T, alpha = 1)
gBest <- glmnet(x=x, y=yFac.m, family='binomial', alpha = 1, lambda = g$lambda.min)
fit <- predict(gBest, newx = x2, type = 'response')


  sub <- cbind(test$Id, fit)
  colnames(sub) <- c("Id","WnvPresent")
  options("scipen" = 100, "digits" = 8)

  filename <- paste0('subs/', 'yearMean', Sys.Date(), '.csv')
  write.csv(sub, filename, row.names = FALSE, quote = FALSE)







#############################################################

# Do a submission with monthly data.

# glmnet


glmnetgrid <- expand.grid(lambda = 0.01^(seq(1, 4, length.out = 7)),
                          alpha = seq(0, 1, length.out = 7))


fitnetMonth <- train(x = tr.m, y = yFac.m,
    method = 'glmnet',
    trControl = ctrl,
    preProc = c("center", "scale", 'medianImpute'),
    metric = 'ROC', 
    tuneGrid = glmnetgrid)


fit <- predict(fitnetMonth, newdata = te.m, type = 'prob')


  sub <- cbind(test$Id, fit[,2])
  colnames(sub) <- c("Id","WnvPresent")
  options("scipen" = 100, "digits" = 8)

  filename <- paste0('subs/', 'monthMeanglmnet', Sys.Date(), '.csv')
  write.csv(sub, filename, row.names = FALSE, quote = FALSE)

# Now smallish but sensible ensemble.
# Make sure there's some nonlinear stuff in there.

library(caretEnsemble)



ctrlEns <- trainControl(index = yearIndex,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = TRUE)

#ensGrid <- expand.grid(lambda = 0.01^(seq(1, 4, length.out = 4)),
#                          alpha = seq(0, 1, length.out = 4),)

# Using default grid for now.


monthList <- caretList(x = tr.m, y = yFac.m,
    methodList = c('glmnet', 'gamboost', 'RRF'),
    trControl = ctrlEns,
    preProc = c("center", "scale", 'medianImpute'),
    metric = 'ROC')


xyplot(resamples(monthList))
modelCor(resamples(monthList))
greedy_ensemble <- caretEnsemble(monthList)
summary(greedy_ensemble)


fit <- predict(greedy_ensemble, newdata = te.m, type = 'prob')


  sub <- cbind(test$Id, fit[,2])
  colnames(sub) <- c("Id","WnvPresent")
  options("scipen" = 100, "digits" = 8)

  filename <- paste0('subs/', 'monthMeanEnsemble', Sys.Date(), '.csv')
  write.csv(sub, filename, row.names = FALSE, quote = FALSE)







########################

ctrl <- trainControl(index = yearIndex,
  classProbs = TRUE,
  summaryFunction = twoClassSummary, 
  verboseIter=TRUE,
  savePredictions = TRUE
  )

v <- c(1:7, grep('monthMean', colnames(tr.m)))
v <- v[v != 36]

fitMonthEns <- caretList(x = tr.m[,v], y = yFac.m,
    methodList = c('glmnet', 'nnet', 'gamboost'),
    trControl = ctrl,
    preProc = c("center", "scale", 'medianImpute'),
    metric = 'ROC', 
    tuneLength = 6)



xyplot(resamples(fitMonthEns[2:3]))
modelCor(resamples(fitMonthEns))
greedy_ensemble <- caretEnsemble(fitMonthEns)
summary(greedy_ensemble)



fit <- predict(greedy_ensemble, newdata = te.m[,v])


  sub <- cbind(test$Id, fit)
  colnames(sub) <- c("Id","WnvPresent")
  options("scipen" = 100, "digits" = 8)

  filename <- paste0('subs/', 'monthMeanEns2', Sys.Date(), '.csv')
  write.csv(sub, filename, row.names = FALSE, quote = FALSE)




###################################################################


ctrl <- trainControl(index = yearIndex,
  classProbs = TRUE,
  summaryFunction = twoClassSummary, 
  verboseIter=TRUE,
  savePredictions = TRUE
  )

v <- c(1:7, grep('monthMean', colnames(tr.m)))
v <- v[v != 36]



fitMonthEns <- caretList(x = cbind(tr.m[,v], lat = train$Latitude, lon = train$Longitude), y = yFac.m,
    methodList = c('nnet', 'dnn', 'pcaNNet', 'gamboost'),
    trControl = ctrl,
    preProc = c("center", "scale", 'knnImpute'),
    metric = 'ROC', 
    tuneLength = 10)




modelCor(resamples(fitMonthEns))
greedy_ensemble <- caretEnsemble(fitMonthEns)
summary(greedy_ensemble)



fit <- predict(greedy_ensemble, newdata =  cbind(te.m[,v], lat = test$Latitude, lon = test$Longitude))


  sub <- cbind(test$Id, fit)
  colnames(sub) <- c("Id","WnvPresent")
  options("scipen" = 100, "digits" = 8)

  filename <- paste0('subs/', 'monthMeanNnetEns', Sys.Date(), '.csv')
  write.csv(sub, filename, row.names = FALSE, quote = FALSE)



