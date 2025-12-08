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
  inner_join(common_incident, by = c("Stop.Number")) 
