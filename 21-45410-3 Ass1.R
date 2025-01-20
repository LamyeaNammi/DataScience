install.packages("dplyr")
library(dplyr)
mydata <- read.csv("C:/Users/ASUS/Desktop/DataScince/fitness_tracker_dataset.csv",header = TRUE,sep = ",")
mydata


mydata$steps <- as.numeric(mydata$steps)
mydata$calories_burned <- as.numeric(mydata$calories_burned)
mydata$sleep_hours <- as.numeric(mydata$sleep_hours)
mydata


correlation <- cor(mydata$steps, mydata$calories_burned, method = "pearson", use = "complete.obs")
print(paste("Correlation coefficient between steps and calories burned:", correlation))
cor_test <- cor.test(mydata$steps, mydata$calories_burned, method = "pearson")
print("Pearson Correlation Test Results:")
print(cor_test)


mydata$workout_type <- as.factor(mydata$workout_type)
mydata$weather_conditions <- as.factor(mydata$weather_conditions)
mydata$mood <- as.factor(mydata$mood)
mydata


contingency_table <- table(mydata$workout_type, mydata$weather_conditions)
print("Contingency Table (workout_type vs. weather_conditions):")
print(contingency_table)

chi_square_result <- chisq.test(contingency_table)
print("Chi-Square Test Results:")
print(chi_square_result)



anova_result <- aov(calories_burned ~ workout_type, data = mydata)
print("ANOVA Test Results (calories_burned ~ workout_type):")
print(summary(anova_result))


anova_sleep <- aov(sleep_hours ~ weather_conditions, data = mydata)
print("ANOVA Test Results (sleep_hours ~ weather_conditions):")
print(summary(anova_sleep))
