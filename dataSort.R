#' ---
#' title: "West Nile Virus Kaggle data sorting"
#'output:
#'  html_document:
#'    keep_md: true
#' ---


#' # West Nile Virus Kaggle Data sorting

#' The competition is to predict the presence of West Nile Viris in mosquitos
#' The sampling unit is a trap.
#' We're given some species information and weather information.
#'
#' The competition metric is AUC.
#'
#' Here I will sort out the data a bit more properly than my previous analyses.
#'
#' ## Preamble
#'
#' First some libraries

#+ libs

library(dplyr) # For handling data
library(ggplot2) # For plotting
library(lubridate) # For time data
library(glmnet) # For quick analyses. Lasso/ridge good for seeing which vars are important.
library(doMC) # Multicore training
library(caret) # General machine learning
library(caretEnsemble) # ensemble methods
library(parallel) # add mcsapply

# Register cores for parallel processesing
registerDoMC(cores = 7)

set.seed(12049)



#' ## Read in data
#' This is all the data given. 
#'
#' - `train.csv` and `test.csv` contain the basic trap locations etc.
#'
#' - `spray.csv` contains time and locations of anti mosquito sprays. 
#' But apparently there is not much data for the test years.
#'
#' - `weather.csv` contains weather data for each day from 2 weather stations.

#+ dataRead

train <- read.csv('train.csv', stringsAsFactors = FALSE)
dim(train)

train %>% head(1) %>% t

test <- read.csv('test.csv', stringsAsFactors = FALSE)


spray <- read.csv('spray.csv', stringsAsFactors = FALSE)
dim(spray)

head(spray)


w <- read.csv('weather.csv', stringsAsFactors = FALSE)
dim(w)

w %>% head(4) %>% t



#' ## Road map
#' - First sort out data into correct formats etc.
#' - I want to look at the weather data in the days/weeks/months *before* a trap is examined
#' - Look at species composition and whether the trap has been +ve for WNV in the past
#' - Look at species composition and WNV in nearby traps.
#' - The test data for spray is not good. But maybe extract whether there has ever been spray at a trap?
#'
#' As I'm going to use a matrix of data, I will make a seperate matrix with only numeric data.
#' And I'll keep the original data.frames in the shape their in at least.



#' ## Data types
#' There's a lot of annoying leading zeros and things. So let's fix all that.
#' First training and testing data.

#'### Y values
#' 

#+ Y vectors

yFac.m <- factor(train$WnvPresent, labels = c('Abs', 'Pres'))
yNum.m <- train$WnvPresent


#' ### Date to POSIX
#+ ToDate

# Dates to POSIX
train$Date %<>% ymd
test$Date %<>% ymd
spray$Date %<>% ymd
w$Date %<>% ymd

# Also want numeric data of week, month day of the year.
# This will be our final data matrix.

tr.m <- train %$% 
          cbind(day = week(Date) * 7 + day(Date), week = week(Date), month = month(Date))
te.m <- test %$% 
          cbind(day = week(Date) * 7 + day(Date), week = week(Date), month = month(Date))

data.frame(tr.m, WNV = factor(train$WnvPresent, labels = c('Absent', 'Present'))) %>%         
  ggplot(aes(x = WNV, y = day)) + 
    geom_violin(adjust = 1.2)

#g <- cv.glmnet(y = yNum.m, x = tr.m, type.measure="auc", family = 'binomial', parallel = TRUE)
#plot(g)

#'### Mosquito species to 0/1 dummy variables for each species.
#' There is unspecified in the test but not in the training.
#' This doesn't matter in the 0/1 column format.
#' Also want names that won't cause any column header problems.

#+ speciesData
# First remove spaces and slashes

train$Species %<>% gsub('\ |/', '-', .)
test$Species %<>% gsub('\ |/', '-', .)

train %>%
  select(Species, WnvPresent) %>%
  group_by(Species) %>%
  summarise(wnv = mean(WnvPresent))

# As Culex erraticus, C. salinarius, C. tarsalis and C. territans all have 0 WNV we can combine them. And this column will get removed.
#   Can also add unspecified as this will then also get removed. Which is good as there's no training info for that class.
train$Species <- train$Species %<>% 
  gsub('CULEX-SALINARIUS|CULEX-TARSALIS|CULEX-TERRITANS', 'CULEX-ERRATICUS', .) %>%
  factor

test$Species <- test$Species %<>% 
  gsub('CULEX-SALINARIUS|CULEX-TARSALIS|CULEX-TERRITANS|UNSPECIFIED-CULEX', 'CULEX-ERRATICUS', .) %>%
  factor

table(train$Species)
table(test$Species)

# Now make dummy variables and remove intercept

tr.m <- (model.matrix( ~ Species, train))[, -1] %>% 
          cbind(tr.m, .)
te.m <- (model.matrix( ~ Species, test))[, -1] %>% 
          cbind(te.m, .)


colnames(tr.m) <- gsub('-', '', colnames(tr.m))
colnames(te.m) <- gsub('-', '', colnames(te.m))

#g <- cv.glmnet(y = yNum.m, x = (model.matrix( ~ Species, train))[, -1], 
#  type.measure="auc", family = 'binomial', , parallel = TRUE)
#plot(g)


#' ### Traps
#' Not sure if I can do anything with the pure trap data.
#' Possibly trap as a factor
#' Otherwise do proportion of WNS Present per trap.

#+ trapData
# Are all the test data represented in the training data.
unique(test$Trap) %in% unique(train$Trap)

# No. They'll have to be NAs


train %>%
  select(Trap, WnvPresent) %>%
  group_by(Trap) %>%
  summarise(wnv = mean(WnvPresent)) %>%
  ggplot(aes(x = factor(Trap), y = wnv)) + 
    geom_bar(stat = 'identity', position = 'dodge')

# How often does each trap have WNV?
trapProp <- train %>%
  select(Trap, WnvPresent) %>%
  group_by(Trap) %>%
  summarise(wnv = mean(WnvPresent))

# Add this data to the matrix
tr.m <- trapProp$wnv[sapply(train$Trap, function(x) which(trapProp$Trap == x))] %>%
          cbind(tr.m, trapPrev = .)

# To deal with logical(0) have to write a function
trapNA <- function(x){
  if(length(which(trapProp$Trap == x)) == 0){
    return(NA)
  } else {
    return(which(trapProp$Trap == x))
  }
}

# find indices for test traps and add p(WNV) to matrix.

te.m <- trapProp$wnv[sapply(test$Trap, trapNA  )] %>% 
          cbind(te.m, trapPrev = .)

# See if it is retained with LASSO
# g <- glmnet(y = yNum.m, x = tr.m, family = 'binomial', alpha = 1)
# plot(g$beta['trapPrev', ] ~ g$lambda, type = 'l')
# Is pretty strong to start.

# Have a look at how well it does on it's own with glm and plot
data.frame(x = trapProp$wnv[sapply(train$Trap, function(x) which(trapProp$Trap == x))], 
  y = yNum.m) %>%
  glm(y ~ x, data = ., family = 'binomial')

data.frame(y = trapProp$wnv[sapply(train$Trap, function(x) which(trapProp$Trap == x))], 
  x = yFac.m) %>%         
  ggplot(aes(x = x, y = y)) + 
    geom_violin(adjust = 1.2)


#'### Number of mosquitos found in traps previously
#' Could have done this and trap prevalence by nearest year as well. But didn't

#+ numMoz

trapMoz <- train %>%
  select(Trap, NumMosquitos) %>%
  group_by(Trap) %>%
  summarise(numMoz = mean(NumMosquitos))


tr.m <- trapMoz$numMoz[sapply(train$Trap, function(x) which(trapMoz$Trap == x))] %>%
          cbind(tr.m, numMoz = .)


te.m <- trapMoz$numMoz[sapply(test$Trap, trapNA)] %>% 
          cbind(te.m, numMoz = .)


# See if it is retained with LASSO
# g <- glmnet(y = yNum.m, x = tr.m, family = 'binomial', alpha = 1)
# plot(g$beta['numMoz', ] ~ g$lambda, type = 'l')
# Gets very much dropped out of the model


data.frame(y = trapMoz$numMoz[sapply(train$Trap, function(x) which(trapMoz$Trap == x))], 
  x = yFac.m) %>%         
  ggplot(aes(x = x, y = y)) + 
    geom_violin(adjust = 1.2)




#' ### Weather data
#' I'm not sure how to go about this.
#' We certainly need to look at the weather before the trap date.
#' But we could conceivably go 60 months or a year back through time.
#' There are 15-20 weather variables. I thin 20 x 365 predictors is overkill.
#'
#' The plan
#' - Mean weather for the year before data collection
#' - Mean weather for 12-6 months before and 6-0 months before
#' - Mean of 12 individual months before. Months might be a pain... so maybe 12 x 4 week periods.
#' - Mean of 8 individual weeks before hand.
#' - 28 days before data collection.
#' - Mean of 12 calendar months. i.e. "The previous March". 
#'     I can see "the previous breeding season" being important for example.
#'
#' That's about 60 x 20 predictors. Still totally overkill. But we'll drop some once we've looked at them.

#+ weatherData, warning = FALSE

names(w)
w$Date %>% range
test$Date %>% range

# Ok so we have enough data for the whole year before the first test data point.

# And make a vector of the actual weather variables.
# Including dropping station as there's only 2
# Also drop codesum for now. Will deal with it later.

wVars <- names(w)[-c(1, 2, 13, 14, 15)]

# Now sort out data classes and stuff.

# Change to numeric.
w$Tavg %<>% as.numeric
w$Tmax %<>% as.numeric
w$Tmin %<>% as.numeric

w$Depart %<>% as.numeric
w$Heat %<>% as.numeric

w$WetBulb %<>% as.numeric
w$DewPoint %<>% as.numeric
w$ResultDir %<>% as.numeric

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

w$SeaLevel %<>% as.numeric

w$AvgSpeed %<>% as.numeric

w$StnPressure %<>% as.numeric

w[, wVars] %>% sapply(class)

# Ridiculous plot but oh well
pairs(w[, wVars], col = rgb(0,0,0, 0.02), pch = 16)
pairs(w[, wVars[1:9]], col = rgb(0,0,0, 0.02), pch = 16)
pairs(w[, wVars[10:17]], col = rgb(0,0,0, 0.02), pch = 16)

w.m <- as.matrix(w[, wVars])

#' ### Weather stations
#' How correlated are the two weather stations?
#+ weatherStations
par(mfrow = c(4, 4))
for(i in c(3:5, 7:10, 17:22)){
  plot(w[w$Station == 1, i] ~ w[w$Station == 2, i], col = rgb(0,0,0, 0.02), pch = 16)
}

#' They're so correlated. I think I might ignore them.

#' ### Year average
#' For each weather row
#'
#'   Find which rows are between the row date and a year before
#'
#'   Take mean of each weather var for those rows.

#+ weatherDataYear

# Year wide moving window on weather

nrow(w)
unique(w$Date) %>% length %>% `*`(2)
# Exactly 2 rows per day

w$Date[nrow(w)] - w$Date[1]  
# But there are some missing days.

# Convert to dates as it's easier
date <- as.Date(w$Date)
# And find the date one year before the date for this row in weather data.
yearBefore <- date - 365

# For each row, find all rows that are between row date and one year before.
#   Then take colmeans. Giving mean of that var, over the year.
#   Note, that dates less than a year before the beginning have less data.
yearMean <- sapply(1:length(date), function(x) 
                          w.m[which(date <= date[x] & date >= yearBefore[x]), ] %>% 
                            colMeans(na.rm = TRUE)) %>%
                            t

colnames(yearMean) <- paste0('yearMean', colnames(yearMean))

w <- cbind(w, yearMean)

# Dates less than a year before have less date.
#   Find out how much data for each row. Might use for weighting.
nDaysUsed <- sapply(1:length(date), function(x) 
                          sum(date <= date[x] & date >= yearBefore[x]))

w <- cbind(w, nDaysUsed = nDaysUsed)


# For each row in training data, which row in weather data has same data.
#   Just going to ignore the second weather station for now.
wTOtr1 <- sapply(train$Date[1:10], function(x) which(w$Date == x & w$Station == 1))
wTOtr12 <- mcmapply(train$Date, function(x) which(w$Date == x & w$Station == 1))
# Same for test data.
wTOte1 <- sapply(test$Date, function(x) which(w$Date == x & w$Station == 1))

# Now find yearMean columns, and take correct rows.
tr.m <- w[wTOtr1, grepl('yearMean', names(w))] %>%
          cbind(tr.m, .) %>%
          as.matrix

te.m <- w[wTOte1, grepl('yearMean', names(w))] %>%
          cbind(te.m, .) %>%
          as.matrix

# g <- glmnet(y = yNum.m, x = tr.m, family = 'binomial', alpha = 1)

# Plot the beta coefficiants against lambda w/ red for yearMean enviro.
#   A few enviro data points become v. important.
# plot(g, col = ifelse(grepl('yearMean', colnames(tr.m)), 'red', 'grey'))


#' ### Make better cross validation

#+ yearCv

train$year <- year(train$Date)

yearIndex <- lapply(unique(train$year), 
  function(x) which(train$year != x)
)

ctrl <- trainControl(index = yearIndex,
  classProbs = TRUE,
  summaryFunction = twoClassSummary, 
  verboseIter=TRUE

  )



glmnetgrid <- expand.grid(lambda = 0.01^(seq(1, 4, length.out = 4)),
                          alpha = seq(0, 1, length.out = 4))


fitglmnet <- train(x = tr.m, y = yFac.m,
    method = 'glmnet',
    trControl = ctrl,
    preProc = c("center", "scale", 'medianImpute'),
    metric = 'ROC', 
    tuneGrid = glmnetgrid)

fitglmnet


#' Lets get the Month before data for now.

#+ monthData

monthBefore <- date - 31

# For each row, find all rows that are between row date and one year before.
#   Then take colmeans. Giving mean of that var, over the year.
#   Note, that dates less than a year before the beginning have less data.
monthMean <- sapply(1:length(date), function(x) 
                          w.m[which(date <= date[x] & date >= monthBefore[x]), ] %>% 
                            colMeans(na.rm = TRUE)) %>%
                            t

colnames(monthMean) <- paste0('monthMean', colnames(monthMean))

w <- cbind(w, monthMean)


# Now find yearMean columns, and take correct rows.
tr.m <- w[wTOtr1, grepl('monthMean', names(w))] %>%
          cbind(tr.m, .) %>%
          as.matrix

te.m <- w[wTOte1, grepl('monthMean', names(w))] %>%
          cbind(te.m, .) %>%
          as.matrix



glmnetgrid <- expand.grid(lambda = 0.01^(seq(1, 4, length.out = 7)),
                          alpha = seq(0, 1, length.out = 7))


fitnetMonth <- train(x = tr.m, y = yFac.m,
    method = 'glmnet',
    trControl = ctrl,
    preProc = c("center", "scale", 'medianImpute'),
    metric = 'ROC', 
    tuneGrid = glmnetgrid)

varImp(fitnetMonth)
fitnetMonth

plot(fitnetMonth, metric = "ROC", plotType = "level",
     scales = list(x = list(rot = 90)))





ctrl <- trainControl(index = yearIndex,
  classProbs = TRUE,
  summaryFunction = twoClassSummary, 
  verboseIter=TRUE

  )

fitBagMonth <- train(x = tr.m, y = yFac.m,
    method = 'gam',
    trControl = ctrl,
    preProc = c("center", "scale", 'medianImpute'),
    metric = 'ROC', 
    tuneLength = 4)

varImp(fitBagMonth)
fitBagMonth

plot(fitBagMonth, metric = 'ROC')
plot(fitBagMonth, metric = "ROC", plotType = "level",
     scales = list(x = list(rot = 90)))


fit <- predict(fitBagMonth, newdata = te.m, type = 'prob')


  sub <- cbind(test$Id, fit[,2])
  colnames(sub) <- c("Id","WnvPresent")
  options("scipen" = 100, "digits" = 8)

  filename <- paste0('subs/', 'monthMeanBag', Sys.Date(), '.csv')
  write.csv(sub, filename, row.names = FALSE, quote = FALSE)



