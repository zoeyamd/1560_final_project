#******************************** PROCESSING *********************************#
#                                                                             #
#                                                                             #
# This code cleans and processes your initial input data.                     #
#*****************************************************************************#


#' Load, clean, and process your input data.
#' 
#' @description This function prepares your data for processing. It loads in 
#' your initial data, removes errors, puts the date into proper format, 
#' identifies performance incidents, and standardizes stop names.
#' 
#' @param ridership_path character string. path to your ridership data CSV
#' @param otp_path character string. path to your otp data CSV
#' @param stop_path character string. path to your stop CSV
#' 
#' @return two cleaned processed data frames of ridership and otp data

clean_and_process <- function(ridership_path, otp_path, stop_path){
  #load in data
  ridership_data <- read.csv(ridership_path)
  otp_data <- read.csv(otp_path)
  stop <- read.csv(stop_path)
  
  #remove error routes and trips
  ridership_data <- ridership_data %>%
    filter(!Route %in% c(99, 999), !Trip %in% c(99,999)) %>%
  #reformat date
    #ridership data
    mutate(datetime_obj = parse_date_time(Time, orders = c("mdy HM", "mdy")),
           date_only = as_date(datetime_obj),
           #making day only column
           Day = day(date_only)) %>%
    select(-c(datetime_obj, date_only))
  
    #otp data
    otp_data$Date <- as.Date(otp_data$Date)
    otp_data <- otp_data %>%
      mutate(Day = day(Date))
    
  #identify incidents
  otp_data <- otp_data %>%
    mutate(Delay.Min = Delay.Sec/60,
           Incident = case_when(Delay.Min < -1 ~ "Early", 
                                Delay.Min > 5 ~ "Late",
                                TRUE ~ "On-Time"))
  
  #standardize stop names
  #ensure stop column names match ridership + otp columns
  stop <- stop %>%
    rename(Stop.Number = stop_id, Stop = Stop)
  
  #change otp stop names to match ridership stop names
  otp_data <- otp_data %>%
    left_join(stop, by = c("Route","Stop")) %>%
    select(-c("Stop"))
  
  return(list(ridership = ridership_data, otp = otp_data))
}

