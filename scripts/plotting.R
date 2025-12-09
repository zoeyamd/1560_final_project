#*********************************** PLOTTING ********************************#
#                                                                             #
#                                                                             #
# This code produces plots of your summarized data.                           #
#*****************************************************************************#

#load library
library(ggplot2)

#source work from run file
source("run.R")

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

