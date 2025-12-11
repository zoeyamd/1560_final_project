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

#extracting stop specific data
#incident related dated
#set thresholds for volatility related to incident
.late_threshold <- 200
.early_threshold <- 200

#filter by stops above thresholds for incident count
volatile_stops_data <- otp_ridership_joined %>%
  filter(Late > .late_threshold, Early > .early_threshold)

#pull volatile stop numbers
volatile_stops <- unique(volatile_stops_data$Stop.Number)
volatile_stops <- as.data.frame(volatile_stops)

#save volatile incident stop numbers
sink("results/volatile_stops_list")
print("These are the stops with scheduling volatility that should be reviewed.")
print(volatile_stops)
sink()

#trip frequency/service related data
#set threshold for underserved related to ridership
ridership_threshold <- 16 

#filter by stops within low frequency and above ridership threshold
under_served_stops <- otp_ridership_joined %>%
  filter(Frequency_Category == "Low Frequency (Tail/Coverage)", 
         avg_riders_per_day > ridership_threshold)

#pull underserved stop numbers
under_served_stops <- under_served_stops_data %>%
  distinct(Stop.Number)
under_served_stops <- as.data.frame(under_served_stops)

#save underserved stop numbers
sink("results/underserved_stop_list")
print("These are the stops with high ridership and low trip frequnecy; they may potentially be underserved and should thus be reviewed.")
print(under_served_stops)
sink()

#PLOT
#average ridership by stop frequency
frequency_plot <- ggplot(otp_ridership_joined, 
                         aes(x = Frequency_Category, y = avg_riders_per_day)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", alpha = 0.7) +
  labs(title = "Average Ridership by Stop Service Frequency",
       x = "Service Frequency Category",
       y = "Average Riders per Day") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(face = "bold"))

#relationship between late and early incidents 
#off-setting zero values 
incident_plot <- ggplot(otp_ridership_joined, aes(x = Late +1, y = Early +1)) +
  geom_point(alpha = 0.6, color = "#FF7F00") +
  geom_smooth(method = "lm", se = TRUE, color = "#0066CC") +
  #spreading out compressed data in the beginning + pulling in outliers
  scale_x_continuous(trans = 'log10', labels = scales::comma) +
  scale_y_continuous(trans = 'log10', labels = scales::comma) +
  labs(title = "Relationship Between Late and Early Incidents per Stop",
       x = "Total Late Incident Count",
       y = "Total Early Incident Count") +
  theme_minimal()

#save plots to results folder
#frequency plot
ggsave(filename = "ridership_frequency_plot.png",
       plot = frequency_plot, 
       path = "results", 
       width = 10, 
       height = 6, 
       units = "in")
#incident plot
ggsave(filename = "incident_plot.png", 
       plot = incident_plot, 
       path = "results", 
       width = 8, 
       height = 5, 
       units = "in")

