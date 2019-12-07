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

sf <- readRDS(file = url('https://stacks.stanford.edu/file/druid:hp256wp2687/hp256wp2687_ca_san_francisco_2019_08_13.rds'))
sf.api <- sf %>%
  select(c(date, time, lat, lng, subject_race))







