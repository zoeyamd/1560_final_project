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



#identifying unique # of days each trip occurs for averaging
.distinct_days <- ridership_data %>% 
  group_by(Route, Stop.Number) %>%
  summarize(distinct_days = length(unique(Day)), .groups = 'drop')

#total riders
.total_riders <- ridership_data %>%
  group_by(Route, Stop.Number) %>%
  summarize(total_riders = sum(Ride.Count), .groups = 'drop')

#average ridership
average_ridership <- .total_riders %>%
  left_join(.distinct_days, by = c("Route", "Stop.Number")) %>%
  mutate(avg_riders_per_day = total_riders / distinct_days) %>%
  filter(avg_riders_per_day <= 50) %>%
  select(-c("total_riders","distinct_days"))

#trip frequency
daily_trip_frequency <- ridership_data %>%
  group_by(Route, Stop.Number, Day) %>%
  summarise(Daily_Trips_Served = length(unique(Trip)), .groups = 'drop')

total_trip_frequency <- daily_trip_frequency %>%
  group_by(Route, Stop.Number) %>%
  summarise(Total_Trips_Served = sum(Daily_Trips_Served))

#trip frequency cateogrized
.frequency_thresholds <- total_trip_frequency %>%
  pull(Total_Trips_Served) %>%
  # Calculate the 33rd and 66th percentiles, ignoring any NA values
  quantile(c(0.33, 0.66), na.rm = TRUE)

.p33_threshold <- round(.frequency_thresholds["33%"]) # 33rd percentile cutoff
.p66_threshold <- round(.frequency_thresholds["66%"]) # 66th percentile cutoff

frequency_categorized <- total_trip_frequency %>%
  mutate(
    Frequency_Category = case_when(
      # Category 1: High Frequency (Trips_Served is greater than or equal to the 66th percentile)
      Total_Trips_Served >= .p66_threshold ~ "High Frequency (Peak Core)",
      
      # Category 2: Medium Frequency (Trips_Served is greater than or equal to the 33rd percentile, but less than p66)
      Total_Trips_Served >= .p33_threshold ~ "Medium Frequency (Base Core)",
      
      # Category 3: Low Frequency (Everything else, which is below the 33rd percentile)
      TRUE ~ "Low Frequency (Tail/Coverage)"
    )
  )

#combined
frequency_ridership_combined <- frequency_categorized %>%
  select(Route, Stop.Number, Frequency_Category) %>%
  inner_join(average_ridership, by = c("Route","Stop.Number")) %>%
  distinct()



#common incident 
common_incident <- otp_data %>%
  group_by(Route, Stop.Number, Incident) %>%
  summarize(occurences = n()) %>%
  ungroup() %>%
  group_by(Route, Stop.Number) %>%
  slice_max(order_by = occurences, n = 1, with_ties = FALSE) %>%
  rename(Common_Incident = Incident) %>%
  select(-c("occurences"))

#join
test <- frequency_ridership_combined %>%
  inner_join(common_incident, by = c("Route", "Stop.Number"))

