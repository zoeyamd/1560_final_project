#identifying unique # of days each trip occurs for averaging
distinct_days <- ridership_data %>% 
  group_by(Route, Trip, Stop.Number) %>%
  summarize(distinct_days = length(unique(Day)), .groups = 'drop')

#total riders
total_riders <- ridership_data %>%
  group_by(Trip, Route, Stop.Number) %>%
  summarize(total_riders = sum(Ride.Count), .groups = 'drop')

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

#average ridership
average_rider 


ridership_data$Time <- as.POSIXlt(df$Time, format = "%m/%d/%y %H:%M")
ridership_data$Day <- day(df$Time)
rd_upd <- ridership_data %>% group_by(Route, Trip, Stop.Number, Day) %>% summarize(Tot_Riders = n()) %>% filter(Trip != 99, Trip != 999, Route != 99) %>% summarize(Avg_Riders = Tot_Riders/n())

summary(df_upd$Avg_Riders)
sum(df_upd$Avg_Riders > 100)/nrow(df_upd) # about 0.2%
sum(df_upd$Avg_Riders > 50)/nrow(df_upd) # about 1%
sum(df_upd$Avg_Riders > 10)/nrow(df_upd) # about 10%

So if you wanted to update some of those that are really extreme by saying you cap any entry at 50 I don't think that would impact much. 


