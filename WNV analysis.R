#' # West Nile Virus Kaggle Competition

#' The competition is to predict the presence of West Nile Viris in mosquitos
#' The sampling unit is a trap.
#' We're given some species information and weather information.
#'
#' The competition metric is AUC.

#' ## Librarys and options

#+ libs

library(magrittr)


#' ## Read in data

#+ dataRead

train <- read.csv('train.csv')
dim(train)

train %>% head(1) %>% t

test <- read.csv('test.csv')


spray <- read.csv('spray.csv')
dim(spray)

head(spray)


w <- read.csv('weather.csv')
dim(w)

w %>% head(4) %>% t

#' Reading through the weather data description I think it needs some sorting out.
#' But for now I'll ignore the mess.
#' I want to see how hard it's going to be to merge the weather and training data.
#'
#' Only two weather stations. The descrimination here will be through time not space.
#' 






#' ## Some data exploration

















#' ## References
#' ### Vector competence
#' http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2631924/
#'
#' http://jme.oxfordjournals.org/content/38/2/130.abstract
#' - Tested 2 weeks later for virus.
#' - Current weather less likely to be important than previous 2-4 weeks?
