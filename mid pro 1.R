install.packages("dplyr")
library(dplyr)
mydata <- read.csv("C:/Users/ASUS/Downloads/Mid_Dataset.csv",header = TRUE,sep = ",")

mydata

names(mydata)
summary(mydata)
str(mydata)

mydata[mydata == ""] <- NA
mydata[mydata == "NA"] <- NA

colSums(is.na(mydata))
rowSums(is.na(mydata))
which(is.na(mydata$Age))
which(is.na.data.frame(mydata))
is.na(mydata)


install.packages("visdat")
library(visdat)
vis_miss(mydata)

na.omit(mydata)
mydata

mydata$Age <- ifelse(is.na(mydata$Age),mean(mydata$Age, na.rm = TRUE),mydata$Age)
colSums(is.na(mydata))

mydata$Age <- ifelse(is.na(mydata$Age),median(mydata$Age, na.rm = TRUE),mydata$Age)
colSums(is.na(mydata))


age_mode <- names(sort(table(mydata$Age), decreasing = TRUE))[1]
mydata$Age <- ifelse(is.na(mydata$Age), age_mode, mydata$Age)
colSums(is.na(mydata))


 
calculate_mode <- function(x) {
  uniq_x <- unique(x[!is.na(x)])
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

mydata$Depression <- ifelse(is.na(mydata$Depression), calculate_mode(mydata$Depression), mydata$Depression)
colSums(is.na(mydata))


vis_miss(mydata)


library(ggplot2)
vis_dat(mydata)



summary_stats <- sapply(mydata, function(x) c(mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE), mode = names(sort(table(x), decreasing = TRUE))[1]))
summary_stats <- as.data.frame(t(summary_stats))
rownames(summary_stats) <- names(mydata)
summary_stats <- tidyr::gather(summary_stats, key = "Statistic", value = "Value")
ggplot(summary_stats, aes(x = rownames(summary_stats), y = Value, fill = Statistic)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(x = "Attributes", y = "Values", fill = "Statistic", title = "Mean, Median, and Mode of Dataset Attributes")












