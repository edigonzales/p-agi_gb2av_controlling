#install.packages("arrow")
#install.packages("zoo")
library(arrow)
library(ggplot2)
library(tidyverse)
library(dplyr)

data <- read_parquet("controlling_gb2av_vollzugsmeldung_delta.parquet")

####### Total

# Load required libraries
library(zoo)
library(ggplot2)

# Convert the data to a zoo object
data_zoo <- zoo::zoo(data$delta, order.by = data$av_gbeintrag)

# Calculate the rolling median with a window width of 7 days
rolling_median <- rollapply(data_zoo, width = 7, FUN = median, align = "center", fill = NA)

# Create a data frame from the rolling median
rolling_data <- data.frame(date = index(rolling_median), median_delta = coredata(rolling_median))

# Create the ggplot object
ggplot(rolling_data, aes(x = date, y = median_delta)) +

  # Specify the geometry for the plot (line plot for time series)
  geom_line() +
  
  ylim(0, 20) +

  # Add labels and title
  labs(x = "Date", y = "Rolling Median Delta", title = "Rolling Median Delta (7-day window)") +

  # Add theme (optional)
  theme_minimal()


####### pro Firma

# Load required libraries
library(dplyr)
library(ggplot2)
library(zoo)

# Define a function to calculate median within a sliding window
calculate_sliding_median <- function(data) {
  rollapply(data, width = 7, FUN = median, align = "center", fill = NA)
}

# Aggregate data by firm and calculate rolling median delta
data_rolling <- data %>%
  group_by(av_firma) %>%
  mutate(rolling_median = calculate_sliding_median(delta))

# Create the ggplot object
ggplot(data_rolling, aes(x = av_gbeintrag, y = rolling_median)) +
  
  # Specify the geometry for the plot (line plot for time series)
  geom_line() +
  
  # Add facets to show separate plots for each firm
  facet_wrap(~ av_firma, scales = "free_y", ncol = 1) +
  
  # Add labels and title
  labs(x = "Date", y = "Rolling Median Delta (7-day window)", title = "Rolling Median Delta for Each Firm") +
  
  # Add theme (optional)
  theme_minimal()


####### Boxplot

# Load the ggplot2 library
library(ggplot2)

# Create a box plot for each unique value of av_firma
ggplot(data, aes(x = av_firma, y = delta)) + 
  geom_boxplot(position = "dodge") + 
  labs(x = "av_firma", y = "delta") + 
  scale_y_log10() 
