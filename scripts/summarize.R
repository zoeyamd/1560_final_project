#******************************** DATA SUMMARY *******************************#
#                                                                             #
#                                                                             #
# This code organizes and summarizes your data.                               #
#*****************************************************************************#

#' Calculate average ridership
#' 
#' @description This function calculates the average ridership for each
#' route-stop combination. 
#' 
#' @param ridership_data data frame. your cleaned ridership data
#' @param threshold numeric. threshold for max bus capacity 
#' 
#' @return a data frame of average ridership and trip frequency for each
#' route-stop combo

summarize_ridership <- function(ridership_data, threshold) {
  #calculate total ridership for each route-stop combo
  total_riders <- ridership_data %>%
    group_by(Route, Stop.Number) %>%
    summarize(total_riders = sum(Ride.Count), .groups = 'drop')
  
  #calculate number of days each route-stop combo occured
  distinct_days <- ridership_data %>% 
    group_by(Route, Stop.Number) %>%
    summarize(distinct_days = length(unique(Day)), .groups = 'drop')
  
  #calculate average ridership per day for each route-stop combo
  average_ridership <- total_riders %>%
    left_join(distinct_days, by = c("Route", "Stop.Number")) %>%
    mutate(avg_riders_per_day = total_riders / distinct_days) %>%
    #filter by max bus capacity threshold
    filter(avg_riders_per_day <= threshold) %>%
    select(-c("total_riders","distinct_days"))
  
  #calculate daily trip frequency
  daily_trip_frequency <- ridership_data %>%
    group_by(Route, Stop.Number, Day) %>%
    summarise(Daily_Trips_Served = length(unique(Trip)), .groups = 'drop')
  #sum total trip frequency
  total_trip_frequency <- daily_trip_frequency %>%
    group_by(Route, Stop.Number) %>%
    summarise(Total_Trips_Served = sum(Daily_Trips_Served), .groups = 'drop')
  
  #cateogrize trip frequency
  .frequency_thresholds <- total_trip_frequency %>%
    pull(Total_Trips_Served) %>%
    #calculate the 25rd and 75th percentiles, ignoring any NA values
    quantile(c(0.50, 0.75), na.rm = TRUE)
  
  .p50_threshold <- round(.frequency_thresholds["50%"]) #50th percentile cutoff
  .p75_threshold <- round(.frequency_thresholds["75%"]) #75th percentile cutoff
  
  frequency_categorized <- total_trip_frequency %>%
    mutate(Frequency_Category = case_when(
        #High Frequency (Trips_Served is greater than or equal to the 75p)
        Total_Trips_Served >= .p75_threshold ~ "High Frequency (Peak Core)",
        #Medium Frequency (Trips_Served is greater than or equal to the 50p, but less than p75)
        Total_Trips_Served >= .p50_threshold ~ "Medium Frequency (Base Core)",
        #Low Frequency (everything else, which is below the 50p)
        TRUE ~ "Low Frequency (Tail/Coverage)"))
  
  #combine average ridership and trip frequency
  frequency_ridership_combined <- frequency_categorized %>%
    select(Route, Stop.Number, Total_Trips_Served, Frequency_Category) %>%
    inner_join(average_ridership, by = c("Route","Stop.Number")) %>%
    distinct()
  
  return(frequency_ridership_combined)
}

#' Incident Occurences
#' 
#' @description This function summarizes how many times each incident occurs for
#' each route-stop combination. 
#' 
#' @param otp_data data frame. your cleaned otp data
#' 
#' @return a data frame of the incident occurence counts for each route-stop combo

summarize_otp <- function(otp_data){
#calculate summary of incident occurences
incidents_count <- otp_data %>%
    group_by(Route, Stop.Number, Incident) %>%
    #count the number of occurences for each incident
    summarize(Occurences = n(), .groups = 'drop') %>%
  group_by(Route, Stop.Number) %>%
  pivot_wider(names_from = Incident, values_from = Occurences, values_fill = 0) %>%
  ungroup()

  return(incidents_count)
}

