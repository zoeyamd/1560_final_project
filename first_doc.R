#data processing
library(tidyverse)
library(dplyr)

ridership_data <- read.csv("/Users/zoeybug/Documents/GitHub/1650_final_project/data/ridership_simulated.csv")
otp_data <- read.csv("/Users/zoeybug/Documents/GitHub/1650_final_project/data/otp_simulated.csv")

#removing error routes and trips
ridership_data <- ridership_data %>%
  filter(!Route %in% c(99, 999), !Trip %in% c(99, 999))

#formatting date
ridership_data <- ridership_data %>%
  mutate(
    datetime_obj = parse_date_time(Time, orders = c("mdy HM", "mdy")),
    date_only = as_date(datetime_obj),
    #making Day only column
    Day = day(date_only)) %>%
  select(-c(datetime_obj, date_only))

sum(is.na(ridership_data$Time))

oop <- otp_data %>%
  mutate(Delay.Min = Delay.Sec/60,
         Incident = case_when(Delay.Min < -1 ~ "Early",
                              Delay.Min > 5 ~ "Late",
                              TRUE ~ "On-Time"))

your_data_fixed <- your_data %>%
  mutate(
    # Check if the string contains a space (which implies time is present)
    has_time = grepl(" ", datetime_char),
    
    # Use if_else to apply the correct parsing function
    datetime_obj = if_else(
      has_time,
      # If TRUE (has time): use Month/Day/Year Hour:Minute parser
      mdy_hm(datetime_char),
      # If FALSE (no time): use Month/Day/Year parser
      mdy(datetime_char)
    ),
    
    # Final step: Convert to a pure Date object (dropping the time for consistency)
    date_only = as_date(datetime_obj)
  )




toob <- ridership_data %>% 
  mutate(Date = day(Time))
  
df$datetime_dttm <- ymd_hms(df$datetime_chr)
ridership_data$Time <- mdy_hm(ridership_data$Time)
  
  mutate(count = length(unique(day(Time))), .groups = 'drop') %>%
  ungroup() %>%
  group_by(Trip, Route, Stop.Number) %>%
  mutate(riders = sum(Ride.Count))

toot <- ridership_data %>%
  group_by(Trip, Route, Stop.Number) %>%
  summarize(riders = sum(Ride.Count)/)


#Riders = sum(Ride.Count), .groups ='drop'
#filter out all route trip 99