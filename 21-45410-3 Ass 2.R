install.packages("dplyr")
library(dplyr)
mydata <- read.csv("C:/Users/ASUS/Desktop/DataScince/fitness_tracker_dataset.csv",header = TRUE,sep = ",")
mydata

mydata$steps <- as.numeric(mydata$steps)
mydata$calories_burned <- as.numeric(mydata$calories_burned)
mydata$sleep_hours <- as.numeric(mydata$sleep_hours)

install.packages("ggplot2")
library(ggplot2)

ggplot(mydata, aes(x = steps)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  ggtitle("Histogram of Steps") +
  xlab("Steps") +
  ylab("Frequency")


ggplot(mydata, aes(x = steps)) +
  geom_freqpoly(binwidth = 1000, color = "red") +
  ggtitle("Line Histogram of Steps") +
  xlab("Steps") +
  ylab("Frequency")


mydata$shape <- ifelse(mydata$steps > median(mydata$steps, na.rm = TRUE), "Positive", "Negative")

ggplot(mydata, aes(x = steps, fill = shape)) +
  geom_histogram(binwidth = 1000, position = "dodge") +
  ggtitle("Histogram for Positive and Negative Shapes") +
  xlab("Steps") +
  ylab("Frequency") +
  facet_wrap(~shape)




set.seed(123)  # For reproducibility
sample_data <- mydata[sample(nrow(mydata), 500), ]  # Sample 500 points

ggplot(sample_data, aes(x = active_minutes, y = heart_rate_avg)) +
  geom_point(color = "blue", alpha = 0.7) +
  labs(title = "Scatter Plot of Active Minutes vs Average Heart Rate",
       x = "Active Minutes",
       y = "Average Heart Rate") +
  theme_minimal()




sample_data$active_minutes_binned <- cut(sample_data$active_minutes, breaks = 5)


ggplot(sample_data, aes(x = active_minutes_binned, y = heart_rate_avg, fill = active_minutes_binned)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  labs(title = "Violin Plot of Heart Rate vs Binned Active Minutes",
       x = "Active Minutes (Binned)",
       y = "Average Heart Rate") +
  theme_minimal() +
  theme(legend.position = "none")

