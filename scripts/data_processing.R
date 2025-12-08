#processing data prior to main analysis

library(tidyverse)
library(dplyr)

ridership_data <- read.csv("/Users/zoeybug/Documents/GitHub/1650_final_project/data/ridership_simulated.csv")
otp_data <- read.csv("/Users/zoeybug/Documents/GitHub/1650_final_project/data/otp_simulated.csv")

#removing error routes and trips
ridership_data <- ridership_data %>%
  filter(!Route %in% c(99, 999), !Trip %in% c(99, 999))

#formatting date
ridership_data <- ridership_data %>%
  mutate(datetime_obj = parse_date_time(Time, orders = c("mdy HM", "mdy")),
         date_only = as_date(datetime_obj),
         #making Day only column
         Day = day(date_only)) %>%
  select(-c(datetime_obj, date_only))

otp_data$Date <- as.Date(otp_data$Date)
otp_data <- otp_data %>%
  mutate(Day = day(Date))

#identifying incident
otp_data <- otp_data %>%
  mutate(Delay.Min = Delay.Sec/60,
         Incident = case_when(Delay.Min < -1 ~ "Early",
                              Delay.Min > 5 ~ "Late",
                              TRUE ~ "On-Time"))

#matching stops
.stops <- read.csv("/Users/zoeybug/Documents/GitHub/1650_final_project/data/stop.csv")
.stops <- .stops %>%
  rename(Stop.Number = TimePointId, Stop = TimePointShortName)

otp_data <- otp_data %>%
  left_join(.stops, by = "Stop") %>%
  select(-c("Stop"))
