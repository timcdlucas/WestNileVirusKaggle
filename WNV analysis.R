#' ---
#' title: "West Nile Virus Kaggle Competition"
#'output:
#'  html_document:
#'    keep_md: true
#'    toc: true
#'    number_sections: true
#' ---


#' # West Nile Virus Kaggle Competition

#' The competition is to predict the presence of West Nile Viris in mosquitos
#' The sampling unit is a trap.
#' We're given some species information and weather information.
#'
#' The competition metric is AUC.

#' ## Librarys and options

#+ libs

library(magrittr)
library(dplyr)
library(visreg)
library(e1071)
library(Metrics)
library(randomForest)
library(lubridate)
library(glmnet)
library(caret)
library(caretEnsemble)
library(gam)
library(pROC)
library(doMC)

auc <- Metrics::auc

set.seed(101)
#' ## Read in data

#+ dataRead

train <- read.csv('train.csv')
dim(train)

train %>% head(1) %>% t

test <- read.csv('test.csv')


spray <- read.csv('spray.csv')
dim(spray)

head(spray)


w <- read.csv('weather.csv', stringsAsFactors = FALSE)
dim(w)

w %>% head(4) %>% t

#' Reading through the weather data description I think it needs some sorting out.
#' But for now I'll ignore the mess.
#' I want to see how hard it's going to be to merge the weather and training data.
#'
#' Only two weather stations. The descrimination here will be through time not space.
#' 






#' ## Some data exploration






#' # Submission 1
#' ## Basic models
#'
#' Let's just try some basic stuff on the data in train.csv (i.e. ignore weather.)
#' I'll copy a lot of the starter script by mlandry to get it going quickly.
#'
#' http://www.kaggle.com/users/48625/mlandry/predict-west-nile-virus/h2o-starter/run/2231
#'
#' As in that script, use 2011 as CV year.

#+ sortData1

train$dMonth <- substr(train$Date, 6, 7) 
train$dYear <- substr(train$Date, 1, 4) 
test$dMonth <- substr(test$Date, 6, 7)
test$dYear <- substr(test$Date, 1, 4)

train$Species2 <- train$Species
test$Species2 <- test$Species
test$Species2[test$Species2 == 'UNSPECIFIED CULEX'] <- 'CULEX ERRATICUS'
test$Species2 <- factor(test$Species2)

train$SpeciesNArm <- as.character(train$Species2)
test$SpeciesNArm <- as.character(test$Species2)
test$SpeciesNArm[is.na(test$SpeciesNArm)] <- 'CULEX ERRATICUS'

#+ fitModel1

fitCv1 <- train %>%
  filter(dYear != 2011) %$%
  glm(WnvPresent ~ dMonth + Species2 + Block, family = "binomial")

par(mfrow = c(2,2))
plot(fitCv1)

par(mfrow = c(2,2))
visreg(fitCv1)

predCv1 <- train %>%
  filter(dYear == 2011) %>%
  predict(fitCv1, newdata = ., type = "response")

## check for a reasonable AUC of the model against unseen data (2011)
train %>%
  filter(dYear == 2011) %>%
  select(WnvPresent) %>%
  table

# Kinda stupid but seems I need a integer vector, not a data.frame for auc
train %>%
  filter(dYear == 2011) %>%
  .[['WnvPresent']] %>%
  auc(., predCv1)


#' Now add Latitude and longitude

#+ fitModel2


fitCv2 <- train %>%
  filter(dYear != 2011) %$%
  glm(WnvPresent ~ dMonth + Species2 + Block + Latitude + Longitude, family = "binomial")

# Make predictions
predCv2 <- train %>%
  filter(dYear == 2011) %>%
  predict(fitCv2, newdata = ., type = "response")



# Calculate AUC
train %>%
  filter(dYear == 2011) %>%
  .[['WnvPresent']] %>%
  auc(., predCv2)

#' Hooray. An increase. Let's still naively try some different models.



#' Last one for now. Random Forest.


#+ fitModelRF


fitCvRF <- train %>%
  filter(dYear != 2011) %$%
  randomForest::randomForest(WnvPresent ~ dMonth + Species2 + Block + Latitude + Longitude, 
    type = "classification",
    ntree = 2000, 
    mtry = 2)

# Make predictions
predCvRF <- train %>%
  filter(dYear == 2011) %>%
  predict(fitCvRF, newdata = ., type = "response")



# Calculate AUC
train %>%
  filter(dYear == 2011) %>%
  .[['WnvPresent']] %>%
  auc(., predCvRF)


#' We'll go with the Random Forest as there's 5 subs a day.
#+ subRF




fullRF1 <- train %$%
  randomForest::randomForest(factor(WnvPresent) ~ dMonth + Species2 + Block + Latitude + Longitude, 
    type = "classification",
    ntree = 2000, 
    mtry = 2)


fullRF1.pred <- predict(fullRF1, newdata = test, type = "prob")

subRF1 <- cbind(test$Id, fullRF1.pred[, 2])
colnames(subRF1) <- c("Id","WnvPresent")
options("scipen" = 100, "digits" = 8)

filenameRF1 <- 'subs/rf1sub150425.csv'
write.csv(subRF1, filenameRF1, row.names=FALSE, quote=FALSE)

#' Time to submit!
#' I did not do well.



#'# Get some weather data in.

#+ weatherMunge

w %>% names

# Change to date format
w$Date %<>% ymd

# Change to numeric.
w$Tavg %<>% as.numeric
w$Tmax %<>% as.numeric
w$Tmin %<>% as.numeric

w$Depart %<>% as.numeric
w$Heat %<>% as.numeric

w$WetBulb %<>% as.numeric

# Change to numeric. 1 NA which is actually an NA so that's fine.
w$Cool %<>% as.numeric

# 'T' means trace. Which I'm going to count as zero.

w$PrecipTotal %<>% gsub('T', '0', .)
w$PrecipTotal %<>% as.numeric

w$SnowFall %<>% gsub('T', '0', .)
w$SnowFall %<>% as.numeric


# '-' mean NA. Hopefully that's what as.numeric will do anyway.
w$Sunset %<>% as.numeric
w$Sunrise %<>% as.numeric

w$SeaLeve %<>% as.numeric

w$AvgSpeed %<>% as.numeric

w$StnPressure %<>% as.numeric

head(w)

#' Ok. That's not too bad.
#' For now lets see what corelates with mosquito number.
#' Later I will want to look at data before the date of collection.

#+ AttachWeather

# Turn to dates
train$Date <- train$Date %>% as.character %>% ymd
test$Date <- test$Date %>% as.character %>% ymd


# Make a reference vector of which rows in w is the same date as the rows in train/test
#   Do once for each station
wTOtr1 <- sapply(train$Date, function(x) which(w$Date == x & w$Station == 1))
wTOtr2 <- sapply(train$Date, function(x) which(w$Date == x & w$Station == 2))

wTOte1 <- sapply(test$Date, function(x) which(w$Date == x & w$Station == 1))
wTOte2 <- sapply(test$Date, function(x) which(w$Date == x & w$Station == 2))

train$PrecipTotal <- cbind(w$PrecipTotal[wTOtr1], w$PrecipTotal[wTOtr2]) %>%
                       apply(., 1, function(x) mean(x, na.rm = TRUE))

test$PrecipTotal <- cbind(w$PrecipTotal[wTOte1], w$PrecipTotal[wTOte2]) %>%
                       apply(., 1, function(x) mean(x, na.rm = TRUE))


smoothScatter(train$NumMosquitos ~ train$PrecipTotal)

#' OK. That kind of surprises me.
#' I thought there'd be a positive correlation.
#' The issue with overspilling datapoint when 50 mosquitos is reached is noticeable but not a disaster.

#+ tempData


train$Tavg <- cbind(w$Tavg[wTOtr1], w$Tavg[wTOtr2]) %>%
                       apply(., 1, function(x) mean(x, na.rm = TRUE))

test$Tavg <- cbind(w$Tavg[wTOte1], w$Tavg[wTOte2]) %>%
                       apply(., 1, function(x) mean(x, na.rm = TRUE))


smoothScatter(train$NumMosquitos ~ train$Tavg)



train$AvgSpeed <- cbind(w$AvgSpeed[wTOtr1], w$AvgSpeed[wTOtr2]) %>%
                       apply(., 1, function(x) mean(x, na.rm = TRUE))

test$AvgSpeed <- cbind(w$AvgSpeed[wTOte1], w$AvgSpeed[wTOte2]) %>%
                       apply(., 1, function(x) mean(x, na.rm = TRUE))


smoothScatter(train$NumMosquitos ~ train$AvgSpeed)



train$Tmax <- cbind(w$Tmax[wTOtr1], w$Tmax[wTOtr2]) %>%
                       apply(., 1, function(x) mean(x, na.rm = TRUE))

test$Tmax <- cbind(w$Tmax[wTOte1], w$Tmax[wTOte2]) %>%
                       apply(., 1, function(x) mean(x, na.rm = TRUE))


smoothScatter(train$NumMosquitos ~ train$Tmax)


train$StnPressure <- cbind(w$StnPressure[wTOtr1], w$StnPressure[wTOtr2]) %>%
                       apply(., 1, function(x) mean(x, na.rm = TRUE))

test$StnPressure <- cbind(w$StnPressure[wTOte1], w$StnPressure[wTOte2]) %>%
                       apply(., 1, function(x) mean(x, na.rm = TRUE))


smoothScatter(train$NumMosquitos ~ train$StnPressure)


#' Think I need to deal with this 50 mosquito cut off.
#' However for now let's just fit a RandomForest, and lasso.
#' I can't be bothered dealing with dummy variables at the moment, so for now I'll skip that.

#+ Lasso



trainCVMat <- train %>%
  filter(dYear != 2011) %>%
  select(PrecipTotal, Tavg, AvgSpeed, Tmax, StnPressure, Block, Latitude, Longitude, dMonth) %>%
  as.matrix

trainCVMat %<>% apply(., 2, as.numeric)
  


testCVMat <- train %>%
  filter(dYear == 2011) %>%
  select(PrecipTotal, Tavg, AvgSpeed, Tmax, StnPressure, Block, Latitude, Longitude, dMonth) %>%
  as.matrix

testCVMat %<>% apply(., 2, as.numeric)
  

trainFullMat <- train %>%
  select(PrecipTotal, Tavg, AvgSpeed, Tmax, StnPressure, Block, Latitude, Longitude, dMonth) %>%
  as.matrix

trainFullMat %<>% apply(., 2, as.numeric)
  


testFullMat <- test %>%
  select(PrecipTotal, Tavg, AvgSpeed, Tmax, StnPressure, Block, Latitude, Longitude, dMonth) %>%
  as.matrix

testFullMat %<>% apply(., 2, as.numeric)
  

# PrecipTotal has NAs which LASSO doens't like, so remove


fitCvLas <-
  cv.glmnet(x = trainCVMat[, -1], y = train$WnvPresent[train$dYear != 2011], 
    family = 'binomial', alpha = 1)

# Make predictions
predCvLas <- 
  predict(fitCvLas, newx = testCVMat[, -1], type = 'response', s = fitCvLas$lambda.min)


# Calculate AUC
train %>%
  filter(dYear == 2011) %>%
  .[['WnvPresent']] %>%
  Metrics::auc(., predCvLas)



#+ subLasso

fitFullLas <-
  cv.glmnet(x = trainFullMat[, -1], y = train$WnvPresent, 
    family = 'binomial', alpha = 1)

# Make predictions
predFullLas1 <- 
  predict(fitFullLas, newx = testFullMat[, -1], type = 'response', s = fitFullLas$lambda.min)



subLas1 <- cbind(test$Id, predFullLas1)
colnames(subLas1) <- c("Id","WnvPresent")
options("scipen" = 100, "digits" = 8)

filenameLas1 <- 'subs/las1sub150425.csv'
write.csv(subLas1, filenameLas1, row.names=FALSE, quote=FALSE)

#' Time to submit!
#' Fair bit better. 0.6
#'
#' Let's do a ridge regression type thing quickly.

fitCvLas.5 <-
  cv.glmnet(x = trainCVMat[, -1], y = train$WnvPresent[train$dYear != 2011], 
    family = 'binomial', alpha = 0.5)

# Make predictions
predCvLas.5 <- 
  predict(fitCvLas.5, newx = testCVMat[, -1], type = 'response', s = fitCvLas.5$lambda.min)


# Calculate AUC
train %>%
  filter(dYear == 2011) %>%
  .[['WnvPresent']] %>%
  Metrics::auc(., predCvLas.5)


fitCvLas.1 <-
  cv.glmnet(x = trainCVMat[, -1], y = train$WnvPresent[train$dYear != 2011], 
    family = 'binomial', alpha = 0.1)

# Make predictions
predCvLas.1 <- 
  predict(fitCvLas.1, newx = testCVMat[, -1], type = 'response', s = fitCvLas.1$lambda.min)


# Calculate AUC
train %>%
  filter(dYear == 2011) %>%
  .[['WnvPresent']] %>%
  Metrics::auc(., predCvLas.1)



#+ subRidge

fitFullRidge <-
  cv.glmnet(x = trainFullMat[, -1], y = train$WnvPresent, 
    family = 'binomial', alpha = 0.1)

# Make predictions
predFullRidge1 <- 
  predict(fitFullRidge, newx = testFullMat[, -1], type = 'response', s = fitFullRidge$lambda.min)



subRidge1 <- cbind(test$Id, predFullRidge1)
colnames(subRidge1) <- c("Id","WnvPresent")
options("scipen" = 100, "digits" = 8)

filenameRidge1 <- 'subs/ridge1sub150425.csv'
write.csv(subRidge1, filenameRidge1, row.names=FALSE, quote=FALSE)


#' Same as Lasso
#' 
#' Finally. Quickly add interactions?

#'/* ##################################################### *?



trainCVMat <- train %>%
  filter(dYear != 2011) %>%
  select(Tavg, AvgSpeed, Tmax, StnPressure, Block, Latitude, Longitude, dMonth) %>%
  as.matrix

trainCVMat %<>% apply(., 2, as.numeric)

trainCVMat <- cbind(trainCVMat, trainCVMat[,1] * trainCVMat[,2], trainCVMat[,1] * trainCVMat[,4],
  trainCVMat[,1] * trainCVMat[,6], trainCVMat[,1] * trainCVMat[,8], trainCVMat[,2] * trainCVMat[,4], trainCVMat[,4] * trainCVMat[,8])

  


testCVMat <- train %>%
  filter(dYear == 2011) %>%
  select(Tavg, AvgSpeed, Tmax, StnPressure, Block, Latitude, Longitude, dMonth) %>%
  as.matrix

testCVMat %<>% apply(., 2, as.numeric)

testCVMat <- cbind(testCVMat, testCVMat[,1] * testCVMat[,2], testCVMat[,1] * testCVMat[,4],
  testCVMat[,1] * testCVMat[,6], testCVMat[,1] * testCVMat[,8], testCVMat[,2] * testCVMat[,4], testCVMat[,4] * testCVMat[,8])

  

trainFullMat <- train %>%
  select(Tavg, AvgSpeed, Tmax, StnPressure, Block, Latitude, Longitude, dMonth) %>%
  as.matrix

trainFullMat %<>% apply(., 2, as.numeric)
  

trainFullMat <- cbind(trainFullMat, trainFullMat[,1] * trainFullMat[,2], trainFullMat[,1] * trainFullMat[,4],
  trainFullMat[,1] * trainFullMat[,6], trainFullMat[,1] * trainFullMat[,8], trainFullMat[,2] * trainFullMat[,4], trainFullMat[,4] * trainFullMat[,8])



testFullMat <- test %>%
  select(Tavg, AvgSpeed, Tmax, StnPressure, Block, Latitude, Longitude, dMonth) %>%
  as.matrix

testFullMat %<>% apply(., 2, as.numeric)

testFullMat <- cbind(testFullMat, testFullMat[,1] * testFullMat[,2], testFullMat[,1] * testFullMat[,4],
  testFullMat[,1] * testFullMat[,6], testFullMat[,1] * testFullMat[,8], testFullMat[,2] * testFullMat[,4], testFullMat[,4] * testFullMat[,8])

  

# PrecipTotal has NAs which LASSO doens't like, so remove


fitCvLas2 <-
  cv.glmnet(x = trainCVMat, y = train$WnvPresent[train$dYear != 2011], 
    family = 'binomial', alpha = 1)

# Make predictions
predCvLas2 <- 
  predict(fitCvLas2, newx = testCVMat, type = 'response', s = fitCvLas2$lambda.min)


# Calculate AUC
train %>%
  filter(dYear == 2011) %>%
  .[['WnvPresent']] %>%
  Metrics::auc(., predCvLas2)



#+ subLasso2

fitFullLas2 <-
  cv.glmnet(x = trainFullMat, y = train$WnvPresent, 
    family = 'binomial', alpha = 1)

# Make predictions
predFullLas2 <- 
  predict(fitFullLas2, newx = testFullMat, type = 'response', s = fitFullLas$lambda.min)



subLas2 <- cbind(test$Id, predFullLas2)
colnames(subLas2) <- c("Id","WnvPresent")
options("scipen" = 100, "digits" = 8)

filenameLas2 <- 'subs/las2sub150425.csv'
write.csv(subLas2, filenameLas2, row.names=FALSE, quote=FALSE)

#' This gave me my best result so far. But only by a tiny amount.



#'## Let's try the caret package for fun.

#+ Make dummy vars

train %<>% cbind(model.matrix( ~ Species2 + 0, train))
test %<>% cbind(model.matrix( ~ Species2 + 0, test))


varsTr <- c('Tavg', 'AvgSpeed', 'Tmax', 'StnPressure', 'Block', 
          'Latitude', 'Longitude', 'dMonth', "Species2CULEX ERRATICUS",
          "Species2CULEX PIPIENS", "Species2CULEX PIPIENS/RESTUANS", 
          "Species2CULEX RESTUANS", "Species2CULEX SALINARIUS", 
          "Species2CULEX TARSALIS", "Species2CULEX TERRITANS"  ) %>% 
          match(names(train))



varsTe <- c('Tavg', 'AvgSpeed', 'Tmax', 'StnPressure', 'Block', 
          'Latitude', 'Longitude', 'dMonth', "Species2CULEX ERRATICUS",
          "Species2CULEX PIPIENS", "Species2CULEX PIPIENS/RESTUANS", 
          "Species2CULEX RESTUANS", "Species2CULEX SALINARIUS", 
          "Species2CULEX TARSALIS", "Species2CULEX TERRITANS"  ) %>% 
          match(names(test))

trainCVMat2 <- train %>%
  filter(dYear != 2011) %>%
  select(varsTr) %>%
  as.matrix

trainCVMat2 %<>% apply(., 2, as.numeric)
  


testCVMat2 <- train %>%
  filter(dYear == 2011) %>%
  select(varsTr) %>%
  as.matrix

testCVMat2 %<>% apply(., 2, as.numeric)
  

trainFullMat2 <- train %>%
  select(varsTr) %>%
  as.matrix

trainFullMat2 %<>% apply(., 2, as.numeric)
  


testFullMat2 <- test %>%
  select(varsTe) %>%
  as.matrix

testFullMat2 %<>% apply(., 2, as.numeric)
  


#+ LassoPresence


fitLassoSp <-
  cv.glmnet(x = trainCVMat2, y = train$WnvPresent[train$dYear != 2011], 
    family = 'binomial', alpha = 0)

# Make predictions
predCvLassoSp <- 
  predict(fitLassoSp, newx = testCVMat2, type = 'response', s = fitLassoSp$lambda.min)


# Calculate AUC
train %>%
  filter(dYear == 2011) %>%
  .[['WnvPresent']] %>%
  Metrics::auc(., predCvLassoSp)


fitFullLassoSp <-
  cv.glmnet(x = trainFullMat2, y = train$WnvPresent, 
    family = 'binomial', alpha = 1)

# Make predictions
predFullLassoSp <- 
  predict(fitFullLassoSp, newx = testFullMat2, type = 'response', s = fitFullLassoSp$lambda.min)



subLassoSp <- cbind(test$Id, predFullLassoSp)
colnames(subLassoSp) <- c("Id","WnvPresent")
options("scipen" = 100, "digits" = 8)

filenameLassoSp <- 'subs/las4sub150426.csv'
write.csv(subLassoSp, filenameLassoSp, row.names=FALSE, quote=FALSE)


#'## Try a GAM as forum said it's quite good
#+ gam1




train$dWeek <- week(train$Date)
test$dWeek <- week(test$Date)


set.seed(2256)
fitCvGAM <- train %>%
  filter(dYear != 2011) %$%
  gam::gam(WnvPresent ~ s(dWeek) + Species2  + lo(Latitude + Longitude) + s(PrecipTotal), 
    family = 'binomial')

# Make predictions
predCvGAM <- train %>%
  filter(dYear == 2011) %>%
  predict(fitCvGAM, newdata = ., type = "response")



# Calculate AUC
train %>%
  filter(dYear == 2011) %>%
  .[['WnvPresent']] %>%
  auc(., predCvGAM)


#' Now submit
#+ subGAM




fullGAM <- train %$%
  gam::gam(WnvPresent ~ s(dWeek) + Species2  + lo(Latitude + Longitude) + s(PrecipTotal), 
    family = 'binomial')


fullGAM.pred <- predict(fullGAM, newdata = test, type = "response")

subGAM <- cbind(test$Id, fullGAM.pred)
colnames(subGAM) <- c("Id","WnvPresent")
options("scipen" = 100, "digits" = 8)

filenameGAM <- 'subs/gam1sub150426.csv'
write.csv(subGAM, filenameGAM, row.names=FALSE, quote=FALSE)



#+ gam2

set.seed(2256)
fitCvGAM <- train %>%
  filter(dYear != 2011) %$%
  gam::gam(WnvPresent ~ lo(dWeek) + Species2  + lo(Latitude + Longitude) + s(PrecipTotal), 
    family = 'binomial')

# Make predictions
predCvGAM <- train %>%
  filter(dYear == 2011) %>%
  predict(fitCvGAM, newdata = ., type = "response")



# Calculate AUC
train %>%
  filter(dYear == 2011) %>%
  .[['WnvPresent']] %>%
  auc(., predCvGAM)


#' Now submit
#+ subGAM2

fullGAM <- train %$%
  gam::gam(WnvPresent ~ s(dWeek) + Species2  + lo(Latitude + Longitude) + s(PrecipTotal), 
    family = 'binomial')


fullGAM.pred <- predict(fullGAM, newdata = test, type = "response")

subGAM <- cbind(test$Id, fullGAM.pred)
colnames(subGAM) <- c("Id","WnvPresent")
options("scipen" = 100, "digits" = 8)

filenameGAM <- 'subs/gam2sub150426.csv'
write.csv(subGAM, filenameGAM, row.names=FALSE, quote=FALSE)





#' ## Try caret


#+ caret

set.seed(2330)

train$wnvFac <- factor(train$WnvPresent)


registerDoMC(cores = 7)


ctrl <- trainControl(method = "repeatedcv",
  repeats = 1,
  classProbs = TRUE,
  summaryFunction = twoClassSummary)

fitCvCar <- train(wnvFac ~ Longitude + Latitude + Species2,
    data = train,
    method = 'glm',
    trControl = ctrl,
    preProc = c("center", "scale"))







#' ## References and notes
#' ### Vector competence
#' http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2631924/
#'
#' http://jme.oxfordjournals.org/content/38/2/130.abstract
#' - Tested 2 weeks later for virus.
#' - Current weather less likely to be important than previous 2-4 weeks?
#'
#' http://parasitesandvectors.com/content/pdf/1756-3305-3-19.pdf
#'
#' http://journals.plos.org/plosntds/article?id=10.1371/journal.pntd.0002768
#' - "90% of the female Culex mosquitoes stayed within 3 km"
#'
#' Spatial extent of data set ~25km

