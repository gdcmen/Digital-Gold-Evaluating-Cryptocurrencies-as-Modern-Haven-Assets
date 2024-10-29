library(forecast)
library(tsibble)
library(dplyr)
library(fable)
library(fpp3)
library(tidyverse)
library(ggplot2)

mydata <- read.csv("Crypto_Clean_DataSet.csv")

data_set = mydata %>%
  mutate(Month = yearmonth(Date)) %>%
  as_tsibble(index = Month)

#Q1 
data_set |> autoplot(ETHPrice) + labs(title = "Ethereum price over time")
data_set |> autoplot(ETHVolume) + labs(title = "Ethereum volume over time")
data_set |> autoplot(BTCPrice) + labs(title = "Bitcoin price over time")
data_set |> autoplot(BTCVol.) + labs(title = "Bitcoin volume over time")
data_set |> autoplot(GoldPrice) + labs(title = "Gold price over time")
data_set |> autoplot(GoldVolume) + labs(title = "Gold volume over time")
data_set |> autoplot(CVIPrice) + labs(title = "CVI price over time")
data_set |> autoplot(InflationRate...) + labs(title = "Inflation over time")
data_set |> autoplot(M1.Billion.) + labs(title = "M1 in billions over time")
data_set |> autoplot(M2.Billion.) + labs(title = "M2 in billions over time")

--------------------------------------------------------
--------------------------------------------------------  
  
#Q2 - MEAN

#Ethereum Price
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Mean" = MEAN(ETHPrice)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "Ethereum Price from 2018 Jan to 2023 Jan + Using Mean Method", y = "Ethereum Price"
  )


#Ethereum Volume
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Mean" = MEAN(ETHVolume)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "Ethereum Volume from 2018 Jan to 2023 Jan + Using Mean Method", y = "Ethereum Volume"
  )


#Bitcoin Price
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Mean" = MEAN(BTCPrice)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "Figure 13 - Bitcoin Price from 2018 Jan to 2023 Jan + Using Mean Method", y = "Bitcoin Price"
  )


#Bitcoin Volume
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Mean" = MEAN(BTCVol.)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "Bitcoin Volume from 2018 Jan to 2023 Jan + Using Mean Method", y = "Bitcoin Volume"
  )


#Gold Price
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Mean" = MEAN(GoldPrice)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "Gold Price from 2018 Jan to 2023 Jan + Using Mean Method", y = "Gold Price"
  )


#Gold Volume
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Mean" = MEAN(GoldVolume)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "Gold Volume from 2018 Jan to 2023 Jan + Using Mean Method", y = "Gold Volume"
  )


#CVI Price
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Mean" = MEAN(CVIPrice)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "CVI Price from 2018 Jan to 2023 Jan + Using Mean Method", y = "CVI Price"
  )


#Inflation Price
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Mean" = MEAN(InflationRate...)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "Inflation Rate from 2018 Jan to 2023 Jan + Using Mean Method", y = "Inflation Rate"
  )


#M1.Billion
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Mean" = MEAN(M1.Billion.)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "M1 in Billions from 2018 Jan to 2023 Jan + Using Mean Method", y = "M1 in Billions"
  )


#M2.Billion
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Mean" = MEAN(M2.Billion.)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "M2 in Billions from 2018 Jan to 2023 Jan + Using Mean Method", y = "M2 in Billions"
  )


------------------------------------------------------------

#Q2 - NAIVE
#Ethereum Price
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Naive" = NAIVE(ETHPrice)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "Ethereum Price from 2018 Jan to 2023 Jan + Using Naive Method", y = "Ethereum Price"
  )


#Ethereum Volume
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Naive" = NAIVE(ETHVolume)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "Ethereum Volume from 2018 Jan to 2023 Jan + Using Naive Method", y = "Ethereum Volume"
  )


#Bitcoin Price
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Naive" = NAIVE(BTCPrice)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "Bitcoin Price from 2018 Jan to 2023 Jan + Using Naive Method", y = "Bitcoin Price"
  )


#Bitcoin Volume
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Naive" = NAIVE(BTCVol.)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "Bitcoin Volume from 2018 Jan to 2023 Jan + Using Naive Method", y = "Bitcoin Volume"
  )


#Gold Price
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Naive" = NAIVE(GoldPrice)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "Gold Price from 2018 Jan to 2023 Jan + Using Naive Method", y = "Gold Price"
  )


#Gold Volume
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Naive" = NAIVE(GoldVolume)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "Gold Volume from 2018 Jan to 2023 Jan + Using Naive Method", y = "Gold Volume"
  )


#CVI Price
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Naive" = NAIVE(CVIPrice)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "Figure 15 - CVI Price from 2018 Jan to 2023 Jan + Using Naive Method", y = "CVI Price"
  )


#Inflation Price
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Naive" = NAIVE(InflationRate...)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "Inflation Rate from 2018 Jan to 2023 Jan + Using Naive Method", y = "Inflation Rate"
  )


#M1.Billion
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Naive" = NAIVE(M1.Billion.)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "M1 in Billions from 2018 Jan to 2023 Jan + Using Naive Method", y = "M1 in Billions"
  )


#M2.Billion
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Naive" = NAIVE(M2.Billion.)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "M2 in Billions from 2018 Jan to 2023 Jan + Using Naive Method", y = "M2 in Billions"
  )

-----------------------------------------------------------------
  
#Q2 - DRIFT
  
#Ethereum Price
train <- data_set |>
filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Drift" = NAIVE(ETHPrice ~ drift())
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "Ethereum Price from 2018 Jan to 2023 Jan + Using Drift Method", y = "Ethereum Price"
  )


#Ethereum Volume
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Drift" = NAIVE(ETHVolume ~ drift())
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "Ethereum Volume from 2018 Jan to 2023 Jan + Using Drift Method", y = "Ethereum Volume"
  )


#Bitcoin Price
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Drift" = NAIVE(BTCPrice ~ drift())
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "Bitcoin Price from 2018 Jan to 2023 Jan + Using Drift Method", y = "Bitcoin Price"
  )


#Bitcoin Volume
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Drift" = NAIVE(BTCVol. ~ drift())
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "Bitcoin Volume from 2018 Jan to 2023 Jan + Using Drift Method", y = "Bitcoin Volume"
  )


#Gold Price
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Drift" = NAIVE(GoldPrice ~ drift())
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "Gold Price from 2018 Jan to 2023 Jan + Using Drift Method", y = "Gold Price"
  )


#Gold Volume
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Drift" = NAIVE(GoldVolume ~ drift())
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "Gold Volume from 2018 Jan to 2023 Jan + Using Drift Method", y = "Gold Volume"
  )


#CVI Price
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Drift" = NAIVE(CVIPrice ~ drift())
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "CVI Price from 2018 Jan to 2023 Jan + Using Drift Method", y = "CVI Price"
  )


#Inflation Price
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Drift" = NAIVE(InflationRate... ~ drift())
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "Inflation Rate from 2018 Jan to 2023 Jan + Using Drift Method", y = "Inflation Rate"
  )


#M1.Billion
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Drift" = NAIVE(M1.Billion. ~ drift())
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "M1 in Billions from 2018 Jan to 2023 Jan + Using Drift Method", y = "M1 in Billions"
  )


#M2.Billion
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Drift" = NAIVE(M2.Billion. ~ drift())
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  labs(
    title = "M2 in Billions from 2018 Jan to 2023 Jan + Using Drift Method", y = "M2 in Billions"
  )

-------------------------------------------------------------------
-------------------------------------------------------------------
  
# Q3 - Mean, Naive, Seasonal Naive

#ETHPrice
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Juan")
# Fit the models
model_fit <- train |>
  model(
    "Mean" = MEAN(ETHPrice),
    "Naïve" = NAIVE(ETHPrice),
    "Seasonal Naive" = SNAIVE(ETHPrice)
    )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  autolayer(filter_index(data_set, "2023 Jan" ~ .), ETHPrice, colour = "black") +
  labs(
    title = "Ethereum price from 2019 to 2024 + Using Mean, Naïve, and Naive Seasonal", y = "Ethereum Price"
  )


#ETHVolume
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Mean" = MEAN(ETHVolume),
    "Naïve" = NAIVE(ETHVolume),
    "Seasonal Naive" = SNAIVE(ETHVolume)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  autolayer(filter_index(data_set, "2023 Jan" ~ .), ETHVolume, colour = "black") +
  labs(
    title = "Ethereum volume from 2019 to 2024 + Using Mean, Naïve, and Naive Seasonal", y = "Ethereum Volume"
  )


#BTCPrice
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
test <- data_set |>
  filter_index("2023 Feb" ~ "2024 Mar")  # Ensure this covers the forecast period
# Fit the models
model_fit <- train |>
  model(
    "Mean" = MEAN(BTCPrice),
    "Naïve" = NAIVE(BTCPrice),
    "Seasonal Naive" = SNAIVE(BTCPrice)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  autolayer(filter_index(data_set, "2023 Jan" ~ .), BTCPrice, colour = "black") +
  labs(
    title = "Figure 13 - Bitcoin price from 2019 to 2024 + Using Mean, Naïve, and Naive Seasonal", y = "Bitcoin Price"
  )
accuracy(model_fc, test)

#BTCvOL.
train <- data_set |>
  filter_index("2018 Jan" ~ "2021 July")
# Fit the models
model_fit <- train |>
  model(
    "Mean" = MEAN(BTCVol.),
    "Naïve" = NAIVE(BTCVol.),
    "Seasonal Naive" = SNAIVE(BTCVol.)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  autolayer(filter_index(data_set, "2021 July" ~ .), BTCVol., colour = "black") +
  labs(
    title = "Bitcoin volume from 2019 to 2024 + Using Mean, Naïve, and Naive Seasonal", y = "Bitcoin Volume"
  )


#GoldPrice
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Mean" = MEAN(GoldPrice),
    "Naïve" = NAIVE(GoldPrice),
    "Seasonal Naive" = SNAIVE(GoldPrice)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  autolayer(filter_index(data_set, "2023 Jan" ~ .), GoldPrice, colour = "black") +
  labs(
    title = "Gold price from 2019 to 2024 + Using Mean, Naïve, and Naive Seasonal", y = "Gold Price"
  )


#GoldVolume
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Mean" = MEAN(GoldVolume),
    "Naïve" = NAIVE(GoldVolume),
    "Seasonal Naive" = SNAIVE(GoldVolume)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  autolayer(filter_index(data_set, "2023 Jan" ~ .), GoldVolume, colour = "black") +
  labs(
    title = "Gold Volume from 2019 to 2024 + Using Mean, Naïve, and Naive Seasonal", y = "Gold Volume"
  )


#CVIPrice
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
test <- data_set |>
  filter_index("2023 Feb" ~ "2024 Mar")  # Ensure this covers the forecast period
# Fit the models
model_fit <- train |>
  model(
    "Mean" = MEAN(CVIPrice),
    "Naïve" = NAIVE(CVIPrice),
    "Seasonal Naive" = SNAIVE(CVIPrice)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  autolayer(filter_index(data_set, "2023 Jan" ~ .), CVIPrice, colour = "black") +
  labs(
    title = "Figure 14 - CVI Price from 2019 to 2024 + Using Mean, Naïve, and Naive Seasonal", y = "CVI Price"
  )
accuracy(model_fc, test)


#Inflation Rate
train <- data_set |>
  filter_index("2018 Jan" ~ "2021 July")
# Fit the models
model_fit <- train |>
  model(
    "Mean" = MEAN(InflationRate...),
    "Naïve" = NAIVE(InflationRate...),
    "Seasonal Naive" = SNAIVE(InflationRate...)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  autolayer(filter_index(data_set, "2021 July" ~ .), InflationRate..., colour = "black") +
  labs(
    title = "Inflation Rate from 2019 to 2024 + Using Mean, Naïve, and Naive Seasonal", y = "Inflation Rate"
  )



#M1 in Billion
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Mean" = MEAN(M1.Billion.),
    "Naïve" = NAIVE(M1.Billion.),
    "Seasonal Naive" = SNAIVE(M1.Billion.)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  autolayer(filter_index(data_set, "2023 Jan" ~ .), M1.Billion., colour = "black") +
  labs(
    title = "M1 in Billion from 2019 to 2024 + Using Mean, Naïve, and Naive Seasonal", y = "M1 in Billion"
  )


#M2 in Billion
train <- data_set |>
  filter_index("2018 Jan" ~ "2023 Jan")
# Fit the models
model_fit <- train |>
  model(
    "Mean" = MEAN(M2.Billion.),
    "Naïve" = NAIVE(M2.Billion.),
    "Seasonal Naive" = SNAIVE(M2.Billion.)
  )
# Generate forecasts for 14 monhts
model_fc <- model_fit |> 
  forecast(h = 14)
# Plot forecasts against actual values
model_fc |>
  autoplot(train,level = NULL) +
  autolayer(filter_index(data_set, "2023 Jan" ~ .), M2.Billion., colour = "black") +
  labs(
    title = "M2 in Billion from 2019 to 2024 + Using Mean, Naïve, and Naive Seasonal", y = "M2 in Billion"
  )

------------------------------------------------------------------------