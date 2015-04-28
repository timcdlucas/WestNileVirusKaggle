#' ---
#' title: "West Nile Virus Kaggle data sorting"
#'output:
#'  html_document:
#'    keep_md: true
#' ---

#'# Factors with no cross validation
#' Some factors like year can't really be cross validated here because the test data is all in years we don't have data for. 
#'
#' So do some quick submissions to crossvalidate using the scoreboard.
#'
#' Using data from dataSort.R
#' 
#' ## Years
#'
#' First train a GAM so we can then add year to compare

#+ Gam

library(magrittr)
library(gam)

yFac.m <- factor(train$WnvPresent)
yNum.m <- train$WnvPresent

tr.m <- train %$% 
          cbind(day = week(Date) * 7 + day(Date), week = week(Date), month = month(Date))
te.m <- test %$% 
          cbind(day = week(Date) * 7 + day(Date), week = week(Date), month = month(Date))

tr.m <- cbind(tr.m, year = year(train$Date))

te.m <- cbind(te.m, year = year(test$Date))

d <- data.frame(yNum.m, tr.m)
dTest <- data.frame(te.m)


# Now train a gam without year

gam <- gam(yNum.m ~ lo(day), data = d, family = 'binomial')


gam.pred <- predict(gam, newdata = dTest, type = "response")

subgam <- cbind(test$Id, gam.pred)
colnames(subgam) <- c("Id","WnvPresent")
options("scipen" = 100, "digits" = 8)

filenamegam <- 'subs/dayGAM1sub150428.csv'
write.csv(subgam, filenamegam, row.names=FALSE, quote=FALSE)




# Now add year.


gamYear <- gam(yNum.m ~ lo(day) + year, data = d, family = 'binomial')


gamYear.pred <- predict(gamYear, newdata = dTest, type = "response")

subgamYear <- cbind(test$Id, gamYear.pred)
colnames(subgamYear) <- c("Id","WnvPresent")
options("scipen" = 100, "digits" = 8)

filenamegamYear <- 'subs/yearGAM1sub150428.csv'
write.csv(subgamYear, filenamegamYear, row.names=FALSE, quote=FALSE)


#' GAM with just day gives 0.621
#' GAM with day and year gives 0.639
#' So it's worth putting year in.







