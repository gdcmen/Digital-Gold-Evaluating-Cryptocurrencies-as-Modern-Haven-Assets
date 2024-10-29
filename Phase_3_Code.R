#1-Present your project dataset as timeseries in a table
library(dplyr)
library(tsibble)
library(readr) # For read_csv, which handles data types more gracefully than read.csv
library(tidyverse)
library(lubridate) # For handling date conversions
mydata = read.csv("Crypto_Clean_DataSet.csv")
data_set=mydata %>%
  mutate(Month = yearmonth(Date)) %>%
  as_tsibble(index = Month)
print(data_set, n = 100)



#2-Plot each variable in your dataset 
data_set |> autoplot(ETHPrice)
data_set |> autoplot(ETHVolume)
data_set |> autoplot(BTCPrice)
data_set |> autoplot(BTCVol.)
data_set |> autoplot(GoldPrice)
data_set |> autoplot(GoldVolume)
data_set |> autoplot(CVIPrice)
data_set |> autoplot(InflationRate...)
data_set |> autoplot(M1.Billion.)
data_set |> autoplot(M2.Billion.)

#3-Decompose the components of your dataset into trend, seasonal, and remainder (for each variable)
#ETHPRICE
ETHPricedcmp <- data_set |>
  model(stl = STL(ETHPrice))
components(ETHPricedcmp)
data_set |>
  model(
    classical_decomposition(ETHPrice, type = "additive")
  ) |>
  components() |>
  autoplot() +
  labs(title = "Classical additive decomposition of total
                  ETHPrice")
#ETHVOLUME
# Assuming data_set is a tsibble with a proper time index and ETHVolume is numeric
ETHVolumedcmp <- data_set %>%
  model(stl = STL(ETHVolume ~ season(window = "periodic")))

# Extracting components
comp <- components(ETHVolumedcmp)

# Classical decomposition might not directly apply as shown if using 'fable' package.
# For classical decomposition and its plot, assuming 'data_set' is prepared correctly:
data_set %>%
  model(
    classical = classical_decomposition(ETHVolume, type = "additive")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Classical additive decomposition of total ETHVolume")

#BTCPRICE
BTCPricedcmp <- data_set |>
  model(stl = STL(BTCPrice))
components(BTCPricedcmp)
data_set |>
  model(
    classical_decomposition(BTCPrice, type = "additive")
  ) |>
  components() |>
  autoplot() +
  labs(title = "Classical additive decomposition of total
                  Bitcoin Price")

#BTCVOLUME
BTCVoldcmp <- data_set |>
  model(stl = STL(BTCVol.))
components(BTCVoldcmp)
data_set |>
  model(
    classical_decomposition(BTCVol., type = "additive")
  ) |>
  components() |>
  autoplot() +
  labs(title = "Classical additive decomposition of total
                  BTCVol")

#GoldPrice
GoldPricedcmp <- data_set |>
  model(stl = STL(GoldPrice))
components(GoldPricedcmp)
data_set |>
  model(
    classical_decomposition(GoldPrice, type = "additive")
  ) |>
  components() |>
  autoplot() +
  labs(title = "Classical additive decomposition of total
                  GoldPrice")

#GoldVolume
GoldVolumedcmp <- data_set |>
  model(stl = STL(GoldVolume))
components(GoldVolumedcmp)
data_set |>
  model(
    classical_decomposition(GoldVolume, type = "additive")
  ) |>
  components() |>
  autoplot() +
  labs(title = "Classical additive decomposition of total
                  GoldVolume")


#CVIPrice
library(tidyverse)
library(tsibble) # For handling time series data
library(fable) # For STL and other models
library(imputeTS) # For imputation

# Assuming data_set is already loaded and is a tsibble

# Impute missing values in CVIPrice using linear interpolation
data_set <- data_set %>%
  mutate(CVIPrice = na.interpolation(CVIPrice))

# Now attempt the STL decomposition
CVIPricedcmp <- data_set |>
  model(stl = STL(CVIPrice))
components(CVIPricedcmp)
data_set |>
  model(
    classical_decomposition(CVIPrice, type = "additive")
  ) |>
  components() |>
  autoplot() +
  labs(title = "Classical additive decomposition of total
                  CVIPrice")

#M1.Billion
# Impute missing values in CVIPrice using linear interpolation
data_set <- data_set %>%
  mutate(M1.Billion. = na.interpolation(M1.Billion.))
M1.Billiondcmp <- data_set |>
  model(stl = STL(M1.Billion.))
components(M1.Billiondcmp)
data_set |>
  model(
    classical_decomposition(M1.Billion., type = "additive")
  ) |>
  components() |>
  autoplot() +
  labs(title = "Classical additive decomposition of total
                  M1.Billion.")

#M2.Billion
# Impute missing values in CVIPrice using linear interpolation
data_set <- data_set %>%
  mutate(M2.Billion. = na.interpolation(M2.Billion.))
M2.Billiondcmp <- data_set |>
  model(stl = STL(M2.Billion.))
components(M2.Billiondcmp)
data_set |>
  model(
    classical_decomposition(M2.Billion., type = "additive")
  ) |>
  components() |>
  autoplot() +
  labs(title = "Classical additive decomposition of total
                  M2.Billion.")

#4-Present the trend of your dataset variables with the original data (for each variable)
#ETHPRICE
components(ETHPricedcmp) |>
  as_tsibble() |>
  autoplot(ETHPrice, colour="blue") +
  geom_line(aes(y=trend), colour = "red") +
  labs(
    y = "ETHPrice",
    title = "Trend"
  )

#ETHVOLUME
components(ETHVolumedcmp) |>
  as_tsibble() |>
  autoplot(ETHVolume, colour="blue") +
  geom_line(aes(y=trend), colour = "red") +
  labs(
    y = "ETHVolume",
    title = "Trend"
  )


#BTCPRICE
components(BTCPricedcmp) |>
  as_tsibble() |>
  autoplot(BTCPrice, colour="orange", size = 1.1) + 
  geom_line(aes(y=trend), colour = "red", size = 1.1) +
  labs(
    x = "Month (1 month interval)",
    y = "BTC Price",
    title = "Bitcoin Price Trend over time"
  ) 

#BTCVOLUME
components(BTCVoldcmp) |>
  as_tsibble() |>
  autoplot(BTCVol., colour="blue") +
  geom_line(aes(y=trend), colour = "red") +
  labs(
    y = "BTCVol.",
    title = "Trend"
  )

#GoldPrice
components(GoldPricedcmp) |>
  as_tsibble() |>
  autoplot(GoldPrice, colour="blue") +
  geom_line(aes(y=trend), colour = "red") +
  labs(
    y = "GoldPrice",
    title = "Trend"
  )

#GoldVolume
components(GoldVolumedcmp) |>
  as_tsibble() |>
  autoplot(GoldVolume, colour="blue") +
  geom_line(aes(y=trend), colour = "red") +
  labs(
    y = "GoldVolumedcmp",
    title = "Trend"
  )


#CVIPrice
components(CVIPricedcmp) |>
  as_tsibble() |>
  autoplot(CVIPrice, colour="blue") +
  geom_line(aes(y=trend), colour = "red") +
  labs(
    y = "CVIPrice",
    title = "Trend"
  )

#M1.Billion
components(M1.Billiondcmp) |>
  as_tsibble() |>
  autoplot(M1.Billion., colour="blue") +
  geom_line(aes(y=trend), colour = "red") +
  labs(
    y = "M1.Billion.",
    title = "Trend"
  )

#M2.Billion
components(M2.Billiondcmp) |>
  as_tsibble() |>
  autoplot(M2.Billion., colour="blue") +
  geom_line(aes(y=trend), colour = "red") +
  labs(
    y = "M2.Billion.",
    title = "Trend"
  )


#5-Present the seasonal of your dataset variables with the original data (for each variable)
#ETHPRICE
components(ETHPricedcmp) |>
  as_tsibble() |>
  autoplot(ETHPrice, colour="blue") +
  geom_line(aes(y=season_year), colour = "red") +
  labs(
    y = "ETHPrice",
    title = "Season"
  )

#ETHVOLUME
components(ETHVolumedcmp) |>
  as_tsibble() |>
  autoplot(ETHVolume, colour="blue") +
  geom_line(aes(y=season_year), colour = "red") +
  labs(
    y = "ETHVolume",
    title = "Season"
  )


#BTCPRICE
components(BTCPricedcmp) |>
  as_tsibble() |>
  autoplot(BTCPrice, colour="orange", size = 1.1) +
  geom_line(aes(y=season_year), colour = "red", size = 1.1) +
  labs(
    y = "BTCPrice",
    title = "Bitcoin Price Seasonality"
  )

#BTCVOLUME
components(BTCVoldcmp) |>
  as_tsibble() |>
  autoplot(BTCVol., colour="blue") +
  geom_line(aes(y=season_year), colour = "red") +
  labs(
    y = "BTCVol.",
    title = "Season"
  )

#GoldPrice
components(GoldPricedcmp) |>
  as_tsibble() |>
  autoplot(GoldPrice, colour="blue") +
  geom_line(aes(y=season_year), colour = "red") +
  labs(
    y = "GoldPrice",
    title = "Season"
  )

#GoldVolume
components(GoldVolumedcmp) |>
  as_tsibble() |>
  autoplot(GoldVolume, colour="blue") +
  geom_line(aes(y=season_year), colour = "red") +
  labs(
    y = "GoldVolumedcmp",
    title = "Season"
  )

#CVIPrice
components(CVIPricedcmp) |>
  as_tsibble() |>
  autoplot(CVIPrice, colour="blue") +
  geom_line(aes(y=season_year), colour = "red") +
  labs(
    y = "CVIPrice",
    title = "Season"
  )

#M1.Billion
components(M1.Billiondcmp) |>
  as_tsibble() |>
  autoplot(M1.Billion., colour="blue") +
  geom_line(aes(y=season_year), colour = "red") +
  labs(
    y = "M1.Billion.",
    title = "Season"
  )

#M2.Billion
components(M2.Billiondcmp) |>
  as_tsibble() |>
  autoplot(M2.Billion., colour="blue") +
  geom_line(aes(y=season_year), colour = "red") +
  labs(
    y = "M2.Billion.",
    title = "Season"
  )

#6-Present the remainder of your dataset variables with the original data (for each variable)
#ETHPRICE
components(ETHPricedcmp) |>
  as_tsibble() |>
  autoplot(ETHPrice, colour="blue") +
  geom_line(aes(y=remainder), colour = "red") +
  labs(
    y = "ETHPrice",
    title = "Remainder"
  )

#ETHVOLUME
components(ETHVolumedcmp) |>
  as_tsibble() |>
  autoplot(ETHVolume, colour="blue") +
  geom_line(aes(y=remainder), colour = "red") +
  labs(
    y = "ETHVolume",
    title = "Remainder"
  )


#BTCPRICE
components(BTCPricedcmp) |>
  as_tsibble() |>
  autoplot(BTCPrice, colour="orange", size = 1.1) +
  geom_line(aes(y=remainder), colour = "red", size = 1.1) +
  labs(
    x = "Month (1 month interval)",
    y = "BTCPrice",
    title = "Bitcoin Price Remainder"
  )

#BTCVOLUME
components(BTCVoldcmp) |>
  as_tsibble() |>
  autoplot(BTCVol., colour="blue") +
  geom_line(aes(y=remainder), colour = "red") +
  labs(
    y = "BTCVol.",
    title = "Remainder"
  )

#GoldPrice
components(GoldPricedcmp) |>
  as_tsibble() |>
  autoplot(GoldPrice, colour="blue") +
  geom_line(aes(y=remainder), colour = "red") +
  labs(
    y = "GoldPrice",
    title = "Remainder"
  )

#GoldVolume
components(GoldVolumedcmp) |>
  as_tsibble() |>
  autoplot(GoldVolume, colour="blue") +
  geom_line(aes(y=remainder), colour = "red") +
  labs(
    y = "GoldVolumedcmp",
    title = "Remainder"
  )

#CVIPrice
components(CVIPricedcmp) |>
  as_tsibble() |>
  autoplot(CVIPrice, colour="blue") +
  geom_line(aes(y=remainder), colour = "red") +
  labs(
    y = "CVIPrice",
    title = "Remainder"
  )

#M1.Billion
components(M1.Billiondcmp) |>
  as_tsibble() |>
  autoplot(M1.Billion., colour="blue") +
  geom_line(aes(y=remainder), colour = "red") +
  labs(
    y = "M1.Billion.",
    title = "Remainder"
  )

#M2.Billion
components(M2.Billiondcmp) |>
  as_tsibble() |>
  autoplot(M2.Billion., colour="blue") +
  geom_line(aes(y=remainder), colour = "red") +
  labs(
    y = "M2.Billion.",
    title = "Remainder"
  )

#7-Find lambda and use box_cox transformation to visualize the transformed data (for each variable)
#ETHPRICE
lambda <- data_set |>
  features(ETHPrice, features = guerrero) |>
  pull(lambda_guerrero)
lambda
data_set |>
  autoplot(box_cox(ETHPrice, lambda)) +
  labs(y = "ETHPrice",
       title = "Adjusted Bread"
  )

#ETHVOLUME
lambda <- data_set |>
  features(ETHVolume, features = guerrero) |>
  pull(lambda_guerrero)
lambda
data_set |>
  autoplot(box_cox(ETHVolume, lambda)) +
  labs(y = "ETHVolume",
       title = "Adjusted Bread"
  )


#BTCPRICE
lambda <- data_set |>
  features(BTCPrice, features = guerrero) |>
  pull(lambda_guerrero)
lambda
data_set |>
  autoplot(box_cox(BTCPrice, lambda)) +
  labs(y = "BTCPrice",
       title = "Adjusted Bread"
  )

#BTCVOLUME
lambda <- data_set |>
  features(BTCVol., features = guerrero) |>
  pull(lambda_guerrero)
lambda
data_set |>
  autoplot(box_cox(BTCVol., lambda)) +
  labs(y = "BTCVolume",
       title = "Adjusted Bread"
  )

#GoldPrice
lambda <- data_set |>
  features(GoldPrice, features = guerrero) |>
  pull(lambda_guerrero)
lambda
data_set |>
  autoplot(box_cox(GoldPrice, lambda)) +
  labs(y = "GoldPrice",
       title = "Adjusted Bread"
  )


#GoldVolume
lambda <- data_set |>
  features(GoldVolume, features = guerrero) |>
  pull(lambda_guerrero)
lambda
data_set |>
  autoplot(box_cox(GoldVolume, lambda)) +
  labs(y = "GoldVolume",
       title = "Adjusted Bread"
  )


#CVIPrice
lambda <- data_set |>
  features(CVIPrice, features = guerrero) |>
  pull(lambda_guerrero)
lambda
data_set |>
  autoplot(box_cox(CVIPrice, lambda)) +
  labs(y = "CVIPrice",
       title = "Adjusted Bread"
  )


#M1.Billion
lambda <- data_set |>
  features(M1.Billion., features = guerrero) |>
  pull(lambda_guerrero)
lambda
data_set |>
  autoplot(box_cox(M1.Billion., lambda)) +
  labs(y = "M1.Billion",
       title = "Adjusted Bread"
  )

#M2.Billion
lambda <- data_set |>
  features(M2.Billion., features = guerrero) |>
  pull(lambda_guerrero)
lambda
data_set |>
  autoplot(box_cox(M2.Billion., lambda)) +
  labs(y = "M2.Billion",
       title = "Adjusted Bread"
  )