install.packages("tidyverse")
library(tidyverse)
# url where the data is located
url <- "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/lax_to_jfk.tar.gz"
# download the file
download.file(url, destfile = "lax_to_jfk.tar.gz")
# untar the file so we can get the csv only
untar("lax_to_jfk.tar.gz", tar = "internal")
# read_csv only 
sub_airline <- read_csv("lax_to_jfk/lax_to_jfk.csv",
                        col_types = cols(
                          'DivDistance' = col_number(),
                          'DivArrDelay' = col_number()
                        ))

head(sub_airline, 3)
head(sub_airline)
tail(sub_airline)
# Question 1) Check the last 10 rows of data
tail(sub_airline,10)

## download the data that is original
url <- "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/airline_2m.tar.gz"
download.file(url, destfile = "airline_2m.tar.gz")
# untar the file to get csv only
untar("airline_2m.tar.gz")
# read_csv only
airlines <- read_csv("airline_2m.csv",
                     col_types = cols(
                       'DivDistance' = col_number(),
                       'Div1Airport' = col_character(),
                       'Div1AirportID' = col_character(),
                       'Div1AirportSeqID' = col_character(),
                       'Div1WheelsOn' = col_character(),
                       'Div1TotalGTime' = col_number(),
                       'Div1LongestGTime' = col_number(),
                       'DivReachedDest' = col_number(),
                       'DivActualElapsedTime' = col_number(),
                       'DivArrDelay' = col_number(),
                       'Div1WheelsOff'= col_character(),
                       'Div1TailNum' = col_character(),
                       'Div2Airport' = col_character(),
                       'Div2AirportID' = col_character(),
                       'Div2AirportSeqID' = col_character(),
                       'Div2WheelsOn' = col_character(),
                       'Div2TotalGTime' = col_number(),
                       'Div2LongestGTime' = col_number(),
                       'Div2WheelsOff' = col_character(),
                       'Div2TailNum' = col_character()
                     ))
## filtering the data to create a subset and filter out flights b/w LAX to JFK
sub_airline <- airlines %>% 
  filter(Origin == "LAX", Dest == "JFK", 
         Cancelled != 1, Diverted != 1) %>% 
  select(Month, DayOfWeek, FlightDate, 
         Reporting_Airline, Origin, Dest, 
         CRSDepTime, CRSArrTime, DepTime, 
         ArrTime, ArrDelay, ArrDelayMinutes, 
         CarrierDelay, WeatherDelay, NASDelay,
         SecurityDelay, LateAircraftDelay, DepDelay, 
         DepDelayMinutes, DivDistance, DivArrDelay)
# printing and rechecking data 
dim(sub_airline)
unique(sub_airline)
# Question 2) Find all the coumn names of the subset
## print all the column names of the subset
colnames(sub_airline)
## Saving the dataset
write_csv(sub_airline, "lax_to_jfk.csv")

## ******** Basic Insights of the Data ***************

# 1) Data Tpes______
sapply(sub_airline, typeof)

x = 10.5
class(x)
k=1
class(k)
y=as.integer(3)
class(y)
z=0i
class(z)
# logical type
log_val = c(TRUE, T , FALSE, F)
class(log_val)

class('This is Harini')
## Summarize 
# group_by / summarize workflow example
install.packages("dplyr")
library(dplyr)
sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarize(avg_carrier_delay = mean(CarrierDelay, na.rm = TRUE)) # use mean value
#trying another summarize functions
sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarise(sd_carrier_delay = sd(CarrierDelay, na.rm = TRUE))
## Question 3) Using sub_airline get the mean of ArrDelay, for each Reporting_Airline.
## In other words, group by Reporting_Airline and summarize the mean of ArrDelay of each reporting airline.
# remember to use na.rm = TRUE.

sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarize(mean_ArrDelay = mean(ArrDelay, na.rm = TRUE))

glimpse(sub_airline)
