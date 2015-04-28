---
title: "West Nile Virus Kaggle Competition"
output:
 html_document:
   keep_md: true
---
# West Nile Virus Kaggle Competition
The competition is to predict the presence of West Nile Viris in mosquitos
The sampling unit is a trap.
We're given some species information and weather information.

The competition metric is AUC.
## Librarys and options



```r
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
```

```r
set.seed(101)
```

## Read in data


```r
train <- read.csv('train.csv')
dim(train)
```

```
## [1] 10506    12
```

```r
train %>% head(1) %>% t
```

```
##                        1                                                   
## Date                   "2007-05-29"                                        
## Address                "4100 North Oak Park Avenue, Chicago, IL 60634, USA"
## Species                "CULEX PIPIENS/RESTUANS"                            
## Block                  "41"                                                
## Street                 " N OAK PARK AVE"                                   
## Trap                   "T002"                                              
## AddressNumberAndStreet "4100  N OAK PARK AVE, Chicago, IL"                 
## Latitude               "41.95"                                             
## Longitude              "-87.8"                                             
## AddressAccuracy        "9"                                                 
## NumMosquitos           "1"                                                 
## WnvPresent             "0"
```

```r
test <- read.csv('test.csv')


spray <- read.csv('spray.csv')
dim(spray)
```

```
## [1] 14835     4
```

```r
head(spray)
```

```
##         Date       Time Latitude Longitude
## 1 2011-08-29 6:56:58 PM    42.39    -88.09
## 2 2011-08-29 6:57:08 PM    42.39    -88.09
## 3 2011-08-29 6:57:18 PM    42.39    -88.09
## 4 2011-08-29 6:57:28 PM    42.39    -88.09
## 5 2011-08-29 6:57:38 PM    42.39    -88.09
## 6 2011-08-29 6:57:48 PM    42.39    -88.09
```

```r
w <- read.csv('weather.csv', stringsAsFactors = FALSE)
dim(w)
```

```
## [1] 2944   22
```

```r
w %>% head(4) %>% t
```

```
##             1            2            3            4           
## Station     "1"          "2"          "1"          "2"         
## Date        "2007-05-01" "2007-05-01" "2007-05-02" "2007-05-02"
## Tmax        "83"         "84"         "59"         "60"        
## Tmin        "50"         "52"         "42"         "43"        
## Tavg        "67"         "68"         "51"         "52"        
## Depart      "14"         "M"          "-3"         "M"         
## DewPoint    "51"         "51"         "42"         "42"        
## WetBulb     "56"         "57"         "47"         "47"        
## Heat        "0"          "0"          "14"         "13"        
## Cool        " 2"         " 3"         " 0"         " 0"        
## Sunrise     "0448"       "-"          "0447"       "-"         
## Sunset      "1849"       "-"          "1850"       "-"         
## CodeSum     " "          " "          "BR"         "BR HZ"     
## Depth       "0"          "M"          "0"          "M"         
## Water1      "M"          "M"          "M"          "M"         
## SnowFall    "0.0"        "M"          "0.0"        "M"         
## PrecipTotal "0.00"       "0.00"       "0.00"       "0.00"      
## StnPressure "29.10"      "29.18"      "29.38"      "29.44"     
## SeaLevel    "29.82"      "29.82"      "30.09"      "30.08"     
## ResultSpeed " 1.7"       " 2.7"       "13.0"       "13.3"      
## ResultDir   "27"         "25"         " 4"         " 2"        
## AvgSpeed    "9.2"        "9.6"        "13.4"       "13.4"
```

Reading through the weather data description I think it needs some sorting out.
But for now I'll ignore the mess.
I want to see how hard it's going to be to merge the weather and training data.

Only two weather stations. The descrimination here will be through time not space.

## Some data exploration
# Submission 1
## Basic models

Let's just try some basic stuff on the data in train.csv (i.e. ignore weather.)
I'll copy a lot of the starter script by mlandry to get it going quickly.

http://www.kaggle.com/users/48625/mlandry/predict-west-nile-virus/h2o-starter/run/2231

As in that script, use 2011 as CV year.


```r
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
```

```r
fitCv1 <- train %>%
  filter(dYear != 2011) %$%
  glm(WnvPresent ~ dMonth + Species2 + Block, family = "binomial")

par(mfrow = c(2,2))
plot(fitCv1)
```

```
## Warning: NaNs produced
## Warning: NaNs produced
```

![plot of chunk fitModel1](figure/fitModel11.png) 

```r
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
```

```
## train %>% filter(dYear == 2011) %>% select(WnvPresent)
##    0    1 
## 1997   57
```

```r
# Kinda stupid but seems I need a integer vector, not a data.frame for auc
train %>%
  filter(dYear == 2011) %>%
  .[['WnvPresent']] %>%
  auc(., predCv1)
```

```
## [1] 0.7117
```

![plot of chunk fitModel1](figure/fitModel12.png) 

Now add Latitude and longitude


```r
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
```

```
## [1] 0.7934
```

Hooray. An increase. Let's still naively try some different models.
Last one for now. Random Forest.


```r
fitCvRF <- train %>%
  filter(dYear != 2011) %$%
  randomForest::randomForest(WnvPresent ~ dMonth + Species2 + Block + Latitude + Longitude, 
    type = "classification",
    ntree = 1000, 
    mtry = 2)
```

```
## Warning: The response has five or fewer unique values.  Are you sure you
## want to do regression?
```

```r
# Make predictions
predCvRF <- train %>%
  filter(dYear == 2011) %>%
  predict(fitCvRF, newdata = ., type = "response")



# Calculate AUC
train %>%
  filter(dYear == 2011) %>%
  .[['WnvPresent']] %>%
  auc(., predCvRF)
```

```
## [1] 0.7521
```

We'll go with the Random Forest as there's 5 subs a day.


```r
fullRF1 <- train %$%
  randomForest::randomForest(factor(WnvPresent) ~ dMonth + Species2 + Block + Latitude + Longitude, 
    type = "classification",
    ntree = 1000, 
    mtry = 2)


fullRF1.pred <- predict(fullRF1, newdata = test, type = "prob")

subRF1 <- cbind(test$Id, fullRF1.pred[, 2])
colnames(subRF1) <- c("Id","WnvPresent")
options("scipen" = 100, "digits" = 8)

filenameRF1 <- 'subs/rf1sub150425.csv'
write.csv(subRF1, filenameRF1, row.names=FALSE, quote=FALSE)


rm(fullRF1, fitCvRF, fullRF1.pred, subRF1)
```

Time to submit!
I did not do well.
# Get some weather data in.


```r
w %>% names
```

```
##  [1] "Station"     "Date"        "Tmax"        "Tmin"        "Tavg"       
##  [6] "Depart"      "DewPoint"    "WetBulb"     "Heat"        "Cool"       
## [11] "Sunrise"     "Sunset"      "CodeSum"     "Depth"       "Water1"     
## [16] "SnowFall"    "PrecipTotal" "StnPressure" "SeaLevel"    "ResultSpeed"
## [21] "ResultDir"   "AvgSpeed"
```

```r
# Change to date format
w$Date %<>% ymd

# Change to numeric.
w$Tavg %<>% as.numeric
```

```
## Warning: NAs introduced by coercion
```

```r
w$Tmax %<>% as.numeric
w$Tmin %<>% as.numeric

w$Depart %<>% as.numeric
```

```
## Warning: NAs introduced by coercion
```

```r
w$Heat %<>% as.numeric
```

```
## Warning: NAs introduced by coercion
```

```r
w$WetBulb %<>% as.numeric
```

```
## Warning: NAs introduced by coercion
```

```r
# Change to numeric. 1 NA which is actually an NA so that's fine.
w$Cool %<>% as.numeric
```

```
## Warning: NAs introduced by coercion
```

```r
# 'T' means trace. Which I'm going to count as zero.

w$PrecipTotal %<>% gsub('T', '0', .)
w$PrecipTotal %<>% as.numeric
```

```
## Warning: NAs introduced by coercion
```

```r
w$SnowFall %<>% gsub('T', '0', .)
w$SnowFall %<>% as.numeric
```

```
## Warning: NAs introduced by coercion
```

```r
# '-' mean NA. Hopefully that's what as.numeric will do anyway.
w$Sunset %<>% as.numeric
```

```
## Warning: NAs introduced by coercion
```

```r
w$Sunrise %<>% as.numeric
```

```
## Warning: NAs introduced by coercion
```

```r
w$SeaLeve %<>% as.numeric
```

```
## Warning: NAs introduced by coercion
```

```r
w$AvgSpeed %<>% as.numeric
```

```
## Warning: NAs introduced by coercion
```

```r
w$StnPressure %<>% as.numeric
```

```
## Warning: NAs introduced by coercion
```

```r
head(w)
```

```
##   Station       Date Tmax Tmin Tavg Depart DewPoint WetBulb Heat Cool
## 1       1 2007-05-01   83   50   67     14       51      56    0    2
## 2       2 2007-05-01   84   52   68     NA       51      57    0    3
## 3       1 2007-05-02   59   42   51     -3       42      47   14    0
## 4       2 2007-05-02   60   43   52     NA       42      47   13    0
## 5       1 2007-05-03   66   46   56      2       40      48    9    0
## 6       2 2007-05-03   67   48   58     NA       40      50    7    0
##   Sunrise Sunset CodeSum Depth Water1 SnowFall PrecipTotal StnPressure
## 1     448   1849             0      M        0           0       29.10
## 2      NA     NA             M      M       NA           0       29.18
## 3     447   1850      BR     0      M        0           0       29.38
## 4      NA     NA   BR HZ     M      M       NA           0       29.44
## 5     446   1851             0      M        0           0       29.39
## 6      NA     NA      HZ     M      M       NA           0       29.46
##   SeaLevel ResultSpeed ResultDir AvgSpeed SeaLeve
## 1    29.82         1.7        27      9.2   29.82
## 2    29.82         2.7        25      9.6   29.82
## 3    30.09        13.0         4     13.4   30.09
## 4    30.08        13.3         2     13.4   30.08
## 5    30.12        11.7         7     11.9   30.12
## 6    30.12        12.9         6     13.2   30.12
```

Ok. That's not too bad.
For now lets see what corelates with mosquito number.
Later I will want to look at data before the date of collection.


```r
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
```

![plot of chunk AttachWeather](figure/AttachWeather.png) 

OK. That kind of surprises me.
I thought there'd be a positive correlation.
The issue with overspilling datapoint when 50 mosquitos is reached is noticeable but not a disaster.


```r
train$Tavg <- cbind(w$Tavg[wTOtr1], w$Tavg[wTOtr2]) %>%
                       apply(., 1, function(x) mean(x, na.rm = TRUE))

test$Tavg <- cbind(w$Tavg[wTOte1], w$Tavg[wTOte2]) %>%
                       apply(., 1, function(x) mean(x, na.rm = TRUE))


smoothScatter(train$NumMosquitos ~ train$Tavg)
```

![plot of chunk tempData](figure/tempData1.png) 

```r
train$AvgSpeed <- cbind(w$AvgSpeed[wTOtr1], w$AvgSpeed[wTOtr2]) %>%
                       apply(., 1, function(x) mean(x, na.rm = TRUE))

test$AvgSpeed <- cbind(w$AvgSpeed[wTOte1], w$AvgSpeed[wTOte2]) %>%
                       apply(., 1, function(x) mean(x, na.rm = TRUE))


smoothScatter(train$NumMosquitos ~ train$AvgSpeed)
```

![plot of chunk tempData](figure/tempData2.png) 

```r
train$Tmax <- cbind(w$Tmax[wTOtr1], w$Tmax[wTOtr2]) %>%
                       apply(., 1, function(x) mean(x, na.rm = TRUE))

test$Tmax <- cbind(w$Tmax[wTOte1], w$Tmax[wTOte2]) %>%
                       apply(., 1, function(x) mean(x, na.rm = TRUE))


smoothScatter(train$NumMosquitos ~ train$Tmax)
```

![plot of chunk tempData](figure/tempData3.png) 

```r
train$StnPressure <- cbind(w$StnPressure[wTOtr1], w$StnPressure[wTOtr2]) %>%
                       apply(., 1, function(x) mean(x, na.rm = TRUE))

test$StnPressure <- cbind(w$StnPressure[wTOte1], w$StnPressure[wTOte2]) %>%
                       apply(., 1, function(x) mean(x, na.rm = TRUE))


smoothScatter(train$NumMosquitos ~ train$StnPressure)
```

![plot of chunk tempData](figure/tempData4.png) 

Think I need to deal with this 50 mosquito cut off.
However for now let's just fit a RandomForest, and lasso.
I can't be bothered dealing with dummy variables at the moment, so for now I'll skip that.


```r
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
```

```
## [1] 0.74160803
```

```r
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
```

Time to submit!
Fair bit better. 0.6

Let's do a ridge regression type thing quickly.


```r
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
```

```
## [1] 0.74036933
```

```r
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
```

```
## [1] 0.74098428
```

```r
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
```

Same as Lasso

Finally. Quickly add interactions?
/* ##################################################### *?


```r
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
```

```
## [1] 0.73714519
```

```r
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
```

This gave me my best result so far. But only by a tiny amount.
## Let's try the caret package for fun.


```r
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
```

```r
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
```

```
## [1] 0.76791942
```

```r
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
```

## Try a GAM as forum said it's quite good


```r
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
```

```
## [1] 0.75298474
```

Now submit


```r
fullGAM <- train %$%
  gam::gam(WnvPresent ~ s(dWeek) + Species2  + lo(Latitude + Longitude) + s(PrecipTotal), 
    family = 'binomial')


fullGAM.pred <- predict(fullGAM, newdata = test, type = "response")

subGAM <- cbind(test$Id, fullGAM.pred)
colnames(subGAM) <- c("Id","WnvPresent")
options("scipen" = 100, "digits" = 8)

filenameGAM <- 'subs/gam1sub150426.csv'
write.csv(subGAM, filenameGAM, row.names=FALSE, quote=FALSE)
```

```r
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
```

```
## [1] 0.76347416
```

Now submit


```r
fullGAM <- train %$%
  gam::gam(WnvPresent ~ s(dWeek) + Species2  + lo(Latitude + Longitude) + s(PrecipTotal), 
    family = 'binomial')


fullGAM.pred <- predict(fullGAM, newdata = test, type = "response")

subGAM <- cbind(test$Id, fullGAM.pred)
colnames(subGAM) <- c("Id","WnvPresent")
options("scipen" = 100, "digits" = 8)

filenameGAM <- 'subs/gam2sub150426.csv'
write.csv(subGAM, filenameGAM, row.names=FALSE, quote=FALSE)
```

## Try caret


```r
registerDoMC(cores = 7)
```

```r
set.seed(2330)

train$wnvFac <- factor(train$WnvPresent, labels = c('Absent', 'Present'))





ctrl <- trainControl(method = "repeatedcv",
  repeats = 1,
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary)

fitCvCarGlm <- train(wnvFac ~ Longitude + Latitude + Species2,
    data = train,
    method = 'LogitBoost',
    trControl = ctrl,
    preProc = c("center", "scale"),
    tuneLength = 3,
    metric = 'ROC')


fitCvCarGlm
```

```
## Boosted Logistic Regression 
## 
## 10506 samples
##    29 predictors
##     2 classes: 'Absent', 'Present' 
## 
## Pre-processing: centered, scaled 
## Resampling: Cross-Validated (5 fold, repeated 1 times) 
## 
## Summary of sample sizes: 8405, 8404, 8405, 8405, 8405 
## 
## Resampling results across tuning parameters:
## 
##   nIter  ROC         Sens        Spec  ROC SD       Sens SD        Spec SD
##   11     0.39694670  1.00000000  0     0.015812107  0.00000000000  0      
##   21     0.38458247  1.00000000  0     0.028764519  0.00000000000  0      
##   31     0.40069872  0.99959819  0     0.054202970  0.00089847031  0      
## 
## ROC was used to select the optimal model using  the largest value.
## The final value used for the model was nIter = 31.
```

## Now let's try caretEnsemble.
Code mostly copied from the vignette http://cran.r-project.org/web/packages/caretEnsemble/vignettes/caretEnsemble-intro.html


```r
ctrlEns <- trainControl(method = "repeatedcv",
  repeats = 1,
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = TRUE)



model_list <- caretList(wnvFac ~ Longitude + Latitude  + Species2 + dMonth + PrecipTotal + Tavg,
    data = train,
    methodList = c('fda',  'nnet', 'glm'),
    trControl = ctrlEns,
    preProc = c("center", "scale"),
    metric = 'ROC')
```

```
## Warning: indexes not defined in trControl.  Attempting to set them
## ourselves, so each model in the ensemble will have the same resampling
## indexes.
```

```
## # weights:  86
## initial  value 5954.013973 
## iter  10 value 1860.438570
## iter  20 value 1823.487757
## iter  30 value 1801.562927
## iter  40 value 1788.969726
## iter  50 value 1782.246425
## iter  60 value 1778.814100
## iter  70 value 1778.225864
## iter  80 value 1778.062310
## iter  90 value 1777.815982
## iter 100 value 1777.721143
## final  value 1777.721143 
## stopped after 100 iterations
```

```r
xyplot(resamples(model_list))
```

![plot of chunk ensemble](figure/ensemble.png) 

```r
greedy_ensemble <- caretEnsemble(model_list)
summary(greedy_ensemble)
```

```
## The following models were ensembled: fda, nnet, glm 
## They were weighted: 
## 0.44 0.55 0.01
## The resulting AUC is: 0.8106
## The fit for each individual model on the AUC is: 
##  method     metric    metricSD
##     fda 0.79375356 0.042140096
##    nnet 0.80131463 0.030485298
##     glm 0.78012855 0.027611279
```

```r
fullEns.pred <- predict(greedy_ensemble, newdata = test)

subEns <- cbind(test$Id, fullEns.pred)
colnames(subEns) <- c("Id","WnvPresent")
options("scipen" = 100, "digits" = 8)

filenameEns <- 'subs/ens1sub150427.csv'
write.csv(subEns, filenameEns, row.names=FALSE, quote=FALSE)
```


## A big ensemble.


```r
set.seed(2042)

ctrlEns <- trainControl(method = "repeatedcv",
  repeats = 1,
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary)


model_list <- caretList(wnvFac ~ Longitude^2 + Longitude + Latitude + Latitude^2 + Species2 + Block + dMonth + PrecipTotal + Tavg,
    data = train,
    methodList = c('fda', 'LogitBoost', 'bagEarth', 'nnet'),
    trControl = ctrlEns,
    preProc = c("center", "scale"),
    metric = 'ROC')
```

```
## Warning: trControl$savePredictions=FALSE.  Setting to TRUE so we can ensemble the models.
## Warning: indexes not defined in trControl.  Attempting to set them ourselves, so each model in the ensemble will have the same resampling indexes.
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## # weights:  91
## initial  value 4262.396414 
## iter  10 value 1882.927819
## iter  20 value 1824.204131
## iter  30 value 1807.709728
## iter  40 value 1795.270746
## iter  50 value 1789.922130
## iter  60 value 1783.054994
## iter  70 value 1776.986838
## iter  80 value 1774.123579
## iter  90 value 1773.017485
## iter 100 value 1772.639791
## final  value 1772.639791 
## stopped after 100 iterations
```

```r
xyplot(resamples(model_list))
```

![plot of chunk ensemble2](figure/ensemble2.png) 

```r
greedy_ensemble <- caretEnsemble(model_list)
summary(greedy_ensemble)
```

```
## The following models were ensembled: bagEarth, nnet 
## They were weighted: 
## 0.78 0.22
## The resulting AUC is: 0.806
## The fit for each individual model on the AUC is: 
##    method     metric    metricSD
##  bagEarth 0.80410878 0.063796990
##      nnet 0.78488744 0.011528006
```

```r
fullEns.pred <- predict(greedy_ensemble, newdata = test)

subEns <- cbind(test$Id, fullEns.pred)
colnames(subEns) <- c("Id","WnvPresent")
options("scipen" = 100, "digits" = 8)

filenameEns <- 'subs/ens2sub150427.csv'
write.csv(subEns, filenameEns, row.names = FALSE, quote = FALSE)
```

## Ensemble of some slow methods.


```r
ctrlEns <- trainControl(method = "repeatedcv",
  repeats = 1,
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary)


model_list <- caretStack(wnvFac ~ Longitude + Latitude + Longitude^2 + Latitude^2 + Species2 + Block + dMonth + PrecipTotal + Tavg,
    data = train,
    methodList = c('bagEarth', 'gaussprRadial','xyf','avNNet' ),  
    trControl = ctrlEns,
    preProc = c("center", "scale"),
    metric = 'ROC')
```

```
## Error: is(list_of_models, "caretList") is not TRUE
```

```r
xyplot(resamples(model_list))
```

![plot of chunk ensemble3](figure/ensemble3.png) 

```r
greedy_ensemble <- caretEnsemble(model_list)
summary(greedy_ensemble)
```

```
## The following models were ensembled: bagEarth, nnet 
## They were weighted: 
## 0.78 0.22
## The resulting AUC is: 0.806
## The fit for each individual model on the AUC is: 
##    method     metric    metricSD
##  bagEarth 0.80410878 0.063796990
##      nnet 0.78488744 0.011528006
```

```r
fullEns.pred <- predict(greedy_ensemble, newdata = test)

subEns <- cbind(test$Id, fullEns.pred)
colnames(subEns) <- c("Id","WnvPresent")
options("scipen" = 100, "digits" = 8)

filenameEns <- 'subs/ens3sub150427.csv'
write.csv(subEns, filenameEns, row.names = FALSE, quote = FALSE)
```

## Nonlinear combs of .


```r
ctrlEns <- trainControl(method = "repeatedcv",
  repeats = 1,
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary)




model_list <- caretList(wnvFac ~ Longitude + Latitude + Species2 + Block + dMonth + PrecipTotal + Tavg,
    data = train,
    methodList = c('fda',  'nnet', 'glm', 'rpart', 'LogitBoost', 'binda', 'glmnet'),
    trControl = ctrlEns,
    preProc = c("center", "scale"),
    metric = 'ROC')
```

```
## Warning: trControl$savePredictions=FALSE.  Setting to TRUE so we can ensemble the models.
## Warning: indexes not defined in trControl.  Attempting to set them ourselves, so each model in the ensemble will have the same resampling indexes.
```

```
## # weights:  91
## initial  value 3491.336775 
## iter  10 value 1910.416090
## iter  20 value 1863.531787
## iter  30 value 1835.850217
## iter  40 value 1822.334982
## iter  50 value 1811.830110
## iter  60 value 1805.425605
## iter  70 value 1800.024502
## iter  80 value 1795.378446
## iter  90 value 1793.468750
## iter 100 value 1792.760272
## final  value 1792.760272 
## stopped after 100 iterations
```

```
## Warning: There were missing values in resampled performance measures.
## Warning: missing values found in aggregated results
```

```
## Error: final tuning parameters could not be determined
```

```r
modEns <-  caretStack(
              model_list, 
              method='nnet',
              metric='ROC',
              trControl=trainControl(
                method='boot',
                number=10,
                savePredictions=TRUE,
                classProbs=TRUE,
                summaryFunction=twoClassSummary
              )
            )
```

```
## # weights:  19
## initial  value 5406.729440 
## iter  10 value 1901.283736
## iter  20 value 1851.766941
## iter  30 value 1848.687876
## iter  40 value 1847.428238
## iter  50 value 1845.056049
## iter  60 value 1844.792615
## iter  70 value 1844.759005
## iter  80 value 1844.355540
## iter  90 value 1843.836010
## iter 100 value 1843.629611
## final  value 1843.629611 
## stopped after 100 iterations
```

```r
modEns
```

```
## A nnet ensemble of 2 base models: fda, LogitBoost, bagEarth, nnet
## 
## Ensemble results:
## Neural Network 
## 
## 10506 samples
##     4 predictors
##     2 classes: 'Absent', 'Present' 
## 
## No pre-processing
## Resampling: Bootstrapped (10 reps) 
## 
## Summary of sample sizes: 10506, 10506, 10506, 10506, 10506, 10506, ... 
## 
## Resampling results across tuning parameters:
## 
##   size  decay   ROC         Sens        Spec           ROC SD      
##   1     0.0000  0.59050057  1.00000000  0.00000000000  0.1458670151
##   1     0.0001  0.70948062  0.99991823  0.00051020408  0.1450197921
##   1     0.1000  0.80620445  0.99983647  0.00102040816  0.0088505823
##   3     0.0000  0.74557878  0.99991823  0.00051020408  0.1298898626
##   3     0.0001  0.77575131  0.99934696  0.00650469439  0.0972328897
##   3     0.1000  0.80655008  0.99970040  0.00200562984  0.0086940218
##   5     0.0000  0.80571603  0.99891017  0.00520400379  0.0078857436
##   5     0.0001  0.80552626  0.99877204  0.00655948728  0.0099582669
##   5     0.1000  0.80650566  0.99972765  0.00200562984  0.0087417470
##   Sens SD        Spec SD     
##   0.00000000000  0.0000000000
##   0.00025856727  0.0016134070
##   0.00051713453  0.0032268139
##   0.00025856727  0.0016134070
##   0.00134199648  0.0125228235
##   0.00077562926  0.0042290522
##   0.00161973183  0.0085357212
##   0.00206815326  0.0135627305
##   0.00069181232  0.0042290522
## 
## ROC was used to select the optimal model using  the largest value.
## The final values used for the model were size = 3 and decay = 0.1.
```

```r
fullEns.pred <- predict(modEns, newdata = test)

subEns <- cbind(test$Id, fullEns.pred)
colnames(subEns) <- c("Id","WnvPresent")
options("scipen" = 100, "digits" = 8)

filenameEns <- 'subs/ens4sub150427.csv'
write.csv(subEns, filenameEns, row.names = FALSE, quote = FALSE)
```

## References and notes
### Vector competence
http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2631924/

http://jme.oxfordjournals.org/content/38/2/130.abstract
- Tested 2 weeks later for virus.
- Current weather less likely to be important than previous 2-4 weeks?

http://parasitesandvectors.com/content/pdf/1756-3305-3-19.pdf

http://journals.plos.org/plosntds/article?id=10.1371/journal.pntd.0002768
- "90% of the female Culex mosquitoes stayed within 3 km"

Spatial extent of data set ~25km
