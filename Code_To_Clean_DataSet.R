####################Fix Gold DataSet###############
# Load necessary libraries
library(readr)
library(dplyr)
library(lubridate)

# Read the data from the CSV file
data <- read_csv("Gold_Price_USD_per_day.csv")

# Convert the 'Date' column to Date type
data$Date <- mdy(data$Date)

# Create a sequence of dates from the minimum to the maximum
full_dates <- seq(min(data$Date), max(data$Date), by = "day")

# Create a data frame with all dates
full_data <- data.frame(Date = full_dates)

# Left join with the original data to find missing dates
full_data <- left_join(full_data, data, by = "Date")

# Replace NAs in columns other than 'Date' with null (or a desired placeholder)
full_data[is.na(full_data)] <- NA

# Write the modified data to a new CSV file
write_csv(full_data, "Modified_Gold_Price_Data.csv")

#####################Inverse the order of the Cryptos DataSets##############
#for bitcoin and ethereum, and all needed variables
# Load necessary library
library(readr)
library(dplyr)

# Read the data
data <- read_csv("Crypto_Volatility_Index_Historical_Data.csv")

# Reverse the order of the dataframe
data_sorted <- data[nrow(data):1, ]

write_csv(data_sorted, "Sorted_Crypto_Volatility_Index_Historical_Data.csv")



