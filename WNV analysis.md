# West Nile Virus Kaggle Competition
The competition is to predict the presence of West Nile Viris in mosquitos
The sampling unit is a trap.
We're given some species information and weather information.

The competition metric is AUC.
## Librarys and options


```r
library(magrittr)
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
## Latitude               "41.95469"                                          
## Longitude              "-87.800991"                                        
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
##         Date       Time  Latitude  Longitude
## 1 2011-08-29 6:56:58 PM 42.391623 -88.089163
## 2 2011-08-29 6:57:08 PM 42.391348 -88.089163
## 3 2011-08-29 6:57:18 PM 42.391022 -88.089157
## 4 2011-08-29 6:57:28 PM 42.390637 -88.089158
## 5 2011-08-29 6:57:38 PM 42.390410 -88.088858
## 6 2011-08-29 6:57:48 PM 42.390395 -88.088315
```

```r
w <- read.csv('weather.csv')
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
## References
### Vector competence
http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2631924/

http://jme.oxfordjournals.org/content/38/2/130.abstract
- Tested 2 weeks later for virus.
- Current weather less likely to be important than previous 2-4 weeks?
