#seulement lancer les packages ci-dessous, sinon rien besoin de lancer dans la partie
#wrangling

library(tidyverse)
library(readr)
library(lubridate)
library(anytime)

##############################################################################

station <- read_csv("data/station.csv")
status <- read_csv("data/avg_bike_available.csv")
trip <- read_csv("data/trip.csv")
weather <- read_csv("data/weather.csv")

#####DO NOT RERUN THE CODE. ALL THE DATA SET ARE UPDATED########
#station####################################
station <- station %>%
  rename(ID_station = id)

write.csv(station, "station.csv")

#######################status##########################################
#######################################################################
#####avg_bike_available
#run the final data for status because otherwise is too long to run every time
#wrangling of the data set status
#create a column year
status <- status %>%
  mutate(ID_year = year(anydate(time)))
#add 2 new variables
status <- status %>%
  mutate(ID_day = day(anydate(time))) %>%
  mutate(ID_month = month(anydate(time)))
#add hours
status <- status %>%
  mutate(ID_hour = hour(anytime(time)))

#regroup mean per hour
status <- status %>%
  group_by(ID_station, ID_year, ID_month, ID_hour) %>%
  summarise(n_available = mean(bikes_available))
#Round because we cannot have half bike
status <- status %>%
  mutate_if(is.numeric, round, 0)
#drop X1 column
status <- status %>%
  select(station_id:n_available)
#rename ID
status <- status %>%
  rename(ID_station = station_id)
#create a key ID
date <- with(status, ymd(paste(year, month, day,  sep = ' ')))

status <- status %>%
  mutate(ID_date = date)

status <- status %>% 
  rename(ID_hour = hour,
         ID_day = day,
         ID_month = month,
         ID_year = year)
#save new csv
write.csv(status, "avg_bike_available.csv")

####################### trip###############################################
###########################################################################

#keep only 10 variables
trip <- trip %>%
  select(id:subscription_type)
#rename variable
trip <- trip %>%
  rename(ID_trip = id)

#i create a table trip_fact. The value will be aggregate to fit the fact_table
trip <- trip %>% 
  mutate(ID_day = day(anydate(start_date))) %>%
  mutate(ID_month = month(anydate(start_date))) %>%
  mutate(ID_year = year(anydate(start_date))) %>% 
  mutate(ID_station = start_station_id) %>% 
  arrange(start_date)
#create a key ID_date
date <- with(trip, ymd(paste(ID_year, ID_month, ID_day,  sep = ' ')))

trip <- trip %>%
  mutate(ID_date = date)
#add key ID_hour
trip <- trip %>%
  mutate(ID_hour = hour(anytime(start_date)))
write.csv(trip, "trip.csv")

##preparation of the trip in order to match the future Fact_table

trip_mean_duration <- trip %>% 
  group_by(ID_station, ID_hour ,ID_date) %>% 
  summarise(mean_duration = mean(duration))

trip_count_trip <- trip %>% 
  group_by(ID_station, ID_hour ,ID_date) %>% 
  summarise(n_trip = n())

trip_fact <- inner_join(trip_mean_duration, trip_count_trip)

write.csv(trip_fact, "trip_fact.csv")

#################### weather#############################################
#########################################################################
#keep only interesting variables
weather <- weather %>%
  select(
    date:min_temperature_f,
    max_humidity:min_humidity,
    max_visibility_miles:min_visibility_miles,
    max_wind_Speed_mph:mean_wind_speed_mph,
    precipitation_inches,
    events
  )
#drop some noise "T" value for the variable precipitation_inches
precipitation <-
  as.numeric(ifelse(
    str_detect(weather$precipitation_inches, "T"),
    0,
    weather$precipitation_inches
  ))
weather <- weather %>% mutate(precipitation_inche = precipitation)
#transform from miles and farenheit to km and celcius
weather <- weather %>%
  transmute(
    date,
    max_temperature = ((max_temperature_f - 32) * (5 / 9)),
    min_temperature = ((min_temperature_f - 32) * (5 / 9)),
    mean_temperature = ((mean_temperature_f - 32) * (5 / 9)),
    max_visibility = (max_visibility_miles * 1.60934),
    min_visibility = (min_visibility_miles * 1.60934),
    mean_visibility = (mean_visibility_miles * 1.60934),
    max_wind_Speed = (max_wind_Speed_mph * 1.60934),
    mean_wind_speed = (mean_wind_speed_mph * 1.60934),
    mean_humidity,
    min_humidity,
    events,
    precipitation_mm = (precipitation_inche * 25.4)
  )
weather <- weather %>%
  mutate(ID_day = day(anydate(date))) %>%
  mutate(ID_month = month(anydate(date))) %>%
  mutate(ID_year = year(anydate(date))) %>%
  select( ID_day, ID_month, ID_year, max_temperature:precipitation_mm)
##########create an ID_date
date <- with(weather, ymd(paste(ID_year, ID_month, ID_day,  sep = ' ')))
weather <- weather %>%
  mutate(ID_date = date)
#save new csv
write.csv(weather, "weather_final.csv")

#########################creation of the table fact####################
########################################################################
Fact_table <- full_join(status, trip_fact, by = c("ID_station", "ID_hour", 
                                                  "ID_date")) 

#Add weather information 
weather_fact <- weather %>%
  group_by(ID_date) %>%
  summarise(
    mean_temperature = mean(mean_temperature),
    mean_visibility = mean(mean_visibility),
    mean_wind_speed = mean(mean_wind_speed),
    mean_humidity = mean(mean_humidity),
    mean_precipitation_mm = mean(precipitation_mm)
  ) %>%
  transmute(ID_date,
    mean_temperature,
    mean_visibility,
    mean_wind_speed,
    mean_humidity,
    mean_precipitation_mm,
  ) 
#add weather to the table fact

Fact_table <- inner_join(Fact_table, weather, by=("ID_date")) 

#finally change NA value by zero
Fact_table <- Fact_table %>% 
  mutate(mean_duration = ifelse(is.na(mean_duration), 0, mean_duration)) %>% 
  mutate(n_trip = ifelse(is.na(n_trip), 0, n_trip))

write.csv(Fact_table, "Fact_table.csv")

