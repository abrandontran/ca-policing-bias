#### Initial Data Exploration

library(readr)

data <- url('https://stacks.stanford.edu/file/druid:hp256wp2687/hp256wp2687_ca_san_francisco_2019_08_13.rds')
test <- readRDS(file = data)

#San Francisco Data NOTE THIS NO LONGER WORKS BECAUSE WE WILL BE ACCESSING THE RDS FILES LIVE.  CSV FILES ARE TOO BIG FOR GIT.
sf <- read_csv("Data/san_francisco.csv", 
          col_types = cols(arrest_made = col_logical(), 
              date = col_date(format = "%Y-%m-%d"), 
              district = col_factor(levels = c("A", 
                                               "B", "C", "D", "E", "F", "G", 
                                               "H", "I", "J", "K", "S", "T")), 
              lat = col_number(), lng = col_number(), 
              outcome = col_factor(levels = c("arrest", 
                                              "citation", "warning")), search_basis = col_factor(levels = c("consent", 
                                                                                                            "other")), subject_age = col_number(), 
              subject_race = col_factor(levels = c("asian/pacific islander", 
                                                   "black", "hispanic", "other", 
                                                   "white")), subject_sex = col_factor(levels = c("female", 
                                                                                                  "male")), time = col_time(format = "%H:%M:%S"), 
              type = col_factor(levels = c("vehicular"))))

library(MASS)
library(randomForest)
set.seed(47)

sf.train <- sample(1:nrow(sf), 5000)

forest.sf <- randomForest(outcome~subject_race + subject_sex, data = sf, subset = sf.train, importance = TRUE, na.action = na.omit)

forest.sf2 <- randomForest(outcome~subject_race + subject_sex + search_conducted + search_vehicle, 
                           data = sf, subset = sf.train, importance = TRUE, na.action = na.omit)


#### Processing Data for usage in API
library(dplyr)
library(hms)

sf <- readRDS(file = url('https://stacks.stanford.edu/file/druid:hp256wp2687/hp256wp2687_ca_san_francisco_2019_08_13.rds'))
sf.api <- sf %>%
  select(c(date, time, lat, lng, subject_race))

#### Using API to classify as day/night
library(httr)
library(jsonlite)
options(stringsAsFactors = FALSE)

request <- GET(url = 'https://api.sunrise-sunset.org/json', query = list(
                 lat = 53.421813,
                 lng = -2.330251,
                 date = "2018-05-01"))

response <- content(request, as = "text", encoding = "UTF-8")


#### Using StreamMetabolism to classify as day/night
library(StreamMetabolism)
sunrise.set(34.042220, -117.622500, "2019-12-07", timezone="UTC+8")


#### Function to take data input, return sunrise/sunset time, and then classify as a new mutated variable.

classify <- function(lat, long, date, time) {
  #Takes lat, long, time, and date as input and classifies TRUE if the time is at day and FALSE if at night.
  
  sunrise <- sunrise.set(lat, long, date)$sunrise
  attributes(sunrise)$tzone <- 'America/Los_Angeles'
  sunrise <- as_hms(sunrise)
  
  sunset <- sunrise.set(lat, long, date)$sunset
  attributes(sunset)$tzone <- 'America/Los_Angeles'
  sunset <- as_hms(sunset)

  if (time < sunset & time >= sunrise)
    return(TRUE)
  
  else
    return(FALSE)
}

mutate_class <- function(df) {
  #Takes a dataframe and adds the boolean day/night classification found via `classify` as a new column.
  
  classifications <- c()
  
  for (i in 1:nrow(df)) {
    classifications <- classifications + classify(df$lat[i], df$long[i], df$date[i])
  }
  
  return(classifications)
}

#Python version
# library(reticulate)
# 
# import pandas
# 
# def classify_df(df):
#   """Takes a dataframe and adds the boolean day/night classification found via `classify` as a new column."""
# 
# classifications = []
# 
# for i in range(len(df.index)):
#   classifications.append('test')
# 
# return classifications
