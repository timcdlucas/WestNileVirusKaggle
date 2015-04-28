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
tr.m <- trapPop$wnv[sapply(train$Trap, function(x) which(trapProp$Trap == x))] %>%
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

te.m <- trapPop$wnv[sapply(test$Trap, trapNA  )] %>% 
          cbind(te.m, trapPrev = .)




