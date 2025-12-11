#************************************* RUN ***********************************#
#                                                                             #
#                                                                             #
# This code runs all functions and analyses of inputted data.                 #
#*****************************************************************************#

#load dependences and functions
library(tidyverse)
library(broom)
library(knitr)

#replace path to where your functions are defined
source("processing.R")
source("summarize.R")

#PROCESSING
#run processing function and pull data frames
processed_data <- clean_and_process(ridership_path, otp_path, stop_path)
ridership_data <- processed_data$ridership
otp_data <- processed_data$otp

#SUMMARIZE
#run summarizing functions
frequency_ridership_combined <- summarize_ridership(ridership_data, threshold)
incidents_count <- summarize_otp(otp_data)

#merge summaries together
otp_ridership_joined <- frequency_ridership_combined %>%
  inner_join(incidents_count, by = c("Route", "Stop.Number"))

#ANALYSIS
#ensure group variables are vectors
frequency_ridership_combined$Frequency_Category <- factor(
  frequency_ridership_combined$Frequency_Category,
                          levels = c("Low Frequency (Tail/Coverage)", 
                                     "Medium Frequency (Base Core)", 
                                     "High Frequency (Peak Core)"),
                          ordered = TRUE)

otp_ridership_joined$Frequency_Category <- factor(
  otp_ridership_joined$Frequency_Category,
                          levels = c("Low Frequency (Tail/Coverage)", 
                                     "Medium Frequency (Base Core)", 
                                     "High Frequency (Peak Core)"),
                          ordered = TRUE)

#run ANOVA test for trip frequency x ridership
anova_frequency_ridership <- anova_test(frequency_ridership_combined, 
                                   "avg_riders_per_day", "Frequency_Category")

#combine ANOVA + post-hoc results into table
anova_table <- tidy(anova_frequency_ridership$anova)
tukey_table <- tidy(anova_frequency_ridership$post_hoc)

anova_tukey_summary <- bind_rows(anova_table %>% 
                                   filter(term != "Residuals"), tukey_table)
anova_tukey_summary <- anova_tukey_summary[-c(1),-c(2:6)]

#save anova results
write.csv(x = anova_tukey_summary,
          file = "results/ANOVA_Tukey_Summary.csv",
          row.names = FALSE,
          quote = TRUE)

#run lm model test for incident type x trip freequency
lm_incident_frequency <- lm_model(otp_ridership_joined, 
                                  incident_cols = c("Early","Late", "`On-Time`"))

#save lm model results
sink("results/lm_model_summary.txt")
print("Early Incident LM Model Results")
print(lm_incident_frequency$Early)
print("On-Time Incident LM Model Results")
print(lm_incident_frequency$`\`On-Time\``)
print("Late Incident LM Model Results")
print(lm_incident_frequency$Late)
sink()

#go to plotting.R to plot data
