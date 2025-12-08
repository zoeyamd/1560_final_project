#identifying unique # of days each trip occurs for averaging
distinct_days <- ridership_data %>% 
  group_by(Route, Trip, Stop.Number) %>%
  summarize(distinct_days = length(unique(Day)), .groups = 'drop')

#total riders
total_riders <- ridership_data %>%
  group_by(Trip, Route, Stop.Number) %>%
  summarize(total_riders = sum(Ride.Count), .groups = 'drop')

#average ridership
average_ridership <- total_riders %>%
  left_join(distinct_days, by = c("Route", "Trip", "Stop.Number")) %>%
  mutate(avg_riders_per_day = total_riders / distinct_days) %>% 
  select(-c("total_riders","distinct_days"))

#trip frequency (number of trips at each stop/each day)
trip_frequency <- ridership_data %>%
  group_by(Route, Stop.Number, Day) %>%
  summarise(Trips_Served = length(unique(Trip)), .groups = 'drop')

#trip frequency cateogrized
frequency_thresholds <- trip_frequency %>%
  pull(Trips_Served) %>%
  # Calculate the 33rd and 66th percentiles, ignoring any NA values
  quantile(c(0.33, 0.66), na.rm = TRUE)

p33_threshold <- round(frequency_thresholds["33%"]) # 33rd percentile cutoff
p66_threshold <- round(frequency_thresholds["66%"]) # 66th percentile cutoff

#create frequency columns
stop_frequency_categorized <- trip_frequency %>%
  mutate(
    Frequency_Category = case_when(
      # Category 1: High Frequency (Trips_Served is greater than or equal to the 66th percentile)
      Trips_Served >= p66_threshold ~ "High Frequency (Peak Core)",
      
      # Category 2: Medium Frequency (Trips_Served is greater than or equal to the 33rd percentile, but less than p66)
      Trips_Served >= p33_threshold ~ "Medium Frequency (Base Core)",
      
      # Category 3: Low Frequency (Everything else, which is below the 33rd percentile)
      TRUE ~ "Low Frequency (Tail/Coverage)"
    )
  )

#combined
frequency_ridership_combined <- stop_frequency_categorized %>%
  select(Route, Stop.Number, Frequency_Category) %>%
  left_join(stop_level_avg_ridership, by = c("Route","Stop.Number")) %>%
  distinct() 

#analysis 
final_analysis_by_category <- frequency_ridership_combined %>%
  group_by(Frequency_Category) %>%
  summarize(Mean_Ridership_in_Category = mean(overall_avg_riders, na.rm = TRUE),
            Total_Stops_in_Category = n(),.groups = 'drop') %>%
  arrange(desc(Mean_Ridership_in_Category))

stop_level_median_ridership <- average_ridership %>%
  group_by(Route, Stop.Number) %>%
  summarize(
    overall_median_riders_at_stop = median(avg_riders_per_day, na.rm = TRUE),
    .groups = 'drop'
  )

#consolidating avg ridership to stops
stop_level_avg_ridership <- average_ridership %>%
  group_by(Route, Stop.Number) %>%
  summarize(overall_avg_riders = mean(avg_riders_per_day, na.rm = TRUE),
            .groups = 'drop')






#ridership per stop + #of days + trips
stop_ridership <- ridership_data %>% 
  group_by(Route, Stop.Number) %>%
  summarize(total_boardings = sum(Ride.Count),
            observed_stop_instances = n_distinct(paste(Route, Stop.Number, Trip, 
                                                       Day)), .groups = 'drop')
stop_ridership_summary <- stop_ridership %>%
  mutate(
    Avg_Riders_Per_Stop_Instance = total_boardings / observed_stop_instances)


diagnostic_check <- ridership_data %>%
  filter(Route == 54, Stop.Number == 17045)

#total trips
total_trips <- ridership_data %>%
  group_by(Route, Stop.Number) %>%
  summarise(total_trips = n_distinct(Trip), .groups = 'drop')

#average ridership per trip
trip_average_ridership <- total_riders %>%
  left_join(distinct_days, by = c("Route", "Trip", "Stop.Number")) %>%
  mutate(avg_riders_per_day = total_riders / distinct_days) #%>% 
#select(-c("total_riders","distinct_days"))

#average ridership per stop
stop_average_ridership <- total_riders %>%
  left_join(total_trips, by = c("Route", "Stop.Number")) %>%
  mutate(avg_riders_per_trip = total_riders / total_trips)



