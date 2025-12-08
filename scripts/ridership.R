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



