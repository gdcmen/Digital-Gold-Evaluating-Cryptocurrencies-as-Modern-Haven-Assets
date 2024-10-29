library(fpp3)
library(ggplot2)
library(tsibble)
library(dplyr)
library(fable)
library(tidyverse)

mydata = read.csv("Crypto_Clean_DataSet.csv")

data_set=mydata %>%
  mutate(Month = yearmonth(Date)) %>%
  as_tsibble(index = Month)
print(data_set, n = 100)


#1
fit <- data_set|>
  model(linear = TSLM(ETHPrice ~ trend()))
fit |> gg_tsresiduals()
fit <- data_set|>
  model(linear = TSLM(ETHVolume ~ trend()))
fit |> gg_tsresiduals()
fit <- data_set|>
  model(linear = TSLM(BTCPrice ~ trend()))
fit |> gg_tsresiduals()
fit <- data_set|>
  model(linear = TSLM(BTCVol. ~ trend()))
fit |> gg_tsresiduals()
fit <- data_set|>
  model(linear = TSLM(GoldPrice ~ trend()))
fit |> gg_tsresiduals()
fit <- data_set|>
  model(linear = TSLM(GoldVolume ~ trend()))
fit |> gg_tsresiduals()
fit <- data_set|>
  model(linear = TSLM(CVIPrice ~ trend()))
fit |> gg_tsresiduals()
fit <- data_set|>
  model(linear = TSLM(InflationRate... ~ trend()))
fit |> gg_tsresiduals()
fit <- data_set|>
  model(linear = TSLM(M1.Billion. ~ trend()))
fit |> gg_tsresiduals()
fit <- data_set|>
  model(linear = TSLM(M2.Billion. ~ trend()))
fit |> gg_tsresiduals()
---------------------------------------------------------------
---------------------------------------------------------------
#2


# Fit the models
fit_trends <- data_set|>
model(
  linear = TSLM(ETHPrice ~ trend()),
  exponential = TSLM(log(ETHPrice) ~ trend()),
  piecewise = TSLM(ETHPrice ~ trend(knots = c(yearmonth("2021 Jan"), yearmonth("2021 Dec"), yearmonth("2022 May"), yearmonth("2023 Aug"))))
  )
# Forecast
fc_trends <- fit_trends |> forecast(h = 12)
# Plot
data_set |>
  autoplot(ETHPrice) +
  geom_line(data = augment(fit_trends), aes(y = .fitted, color = .model)) +
  autolayer(fc_trends, aes(y = .mean), alpha = 0.5, level = 95) +
  labs(y = "ETHPrice", title = "Trend Analysis for Ethereum Price")
accuracy(fit_trends)



# Fit the models
fit_trends <- data_set|>
  model(
    linear = TSLM(ETHVolume ~ trend()),
    exponential = TSLM(log(ETHVolume) ~ trend()),
    piecewise = TSLM(ETHVolume ~ trend(knots = c(yearmonth("2020 Jun"), yearmonth("2020 Nov"), yearmonth("2020 Dec"))))
  )
# Forecast
fc_trends <- fit_trends |> forecast(h = 12)
# Plot
data_set |>
  autoplot(ETHVolume) +
  geom_line(data = augment(fit_trends), aes(y = .fitted, color = .model)) +
  autolayer(fc_trends, aes(y = .mean), alpha = 0.5, level = 95) +
  labs(y = "ETHVolume", title = "Trend Analysis for Ethereum Volume")
accuracy(fit_trends)
  
  
  

# Fit the models
fit_trends <- data_set|>
  model(
    linear = TSLM(BTCPrice ~ trend()),
    exponential = TSLM(log(BTCPrice) ~ trend()),
    piecewise = TSLM(BTCPrice ~ trend(knots = c(yearmonth("2021 Nov"), yearmonth("2022 May"))))
  )
# Forecast
fc_trends <- fit_trends |> forecast(h = 12)
# Plot
data_set |>
  autoplot(BTCPrice) +
  geom_line(data = augment(fit_trends), aes(y = .fitted, color = .model)) +
  autolayer(fc_trends, aes(y = .mean), alpha = 0.5, level = 95) +
  labs(y = "BTCPrice", title = "Trend Analysis for Bitcoin Price")
accuracy(fit_trends)





# Fit the models
fit_trends <- data_set|>
  model(
        linear = TSLM(BTCVol. ~ trend()),
    exponential = TSLM(log(BTCVol.) ~ trend()),
    piecewise = TSLM(BTCVol. ~ trend(knots = c(yearmonth("2021 Mar"), yearmonth("2023 Jun"))))
  )
# Forecast
fc_trends <- fit_trends |> forecast(h = 12)
# Plot
data_set |>
  autoplot(BTCVol.) +
  geom_line(data = augment(fit_trends), aes(y = .fitted, color = .model)) +
  autolayer(fc_trends, aes(y = .mean), alpha = 0.5, level = 95) +
  labs(y = "BTCVol.", title = "Trend Analysis for Bitcoin Volume")
accuracy(fit_trends)


##2 gold variables, value and volume. We skip to 
---------------------------------------------------------------
---------------------------------------------------------------
#3
  
# Fit the models
fit_trends <- data_set|>
  model(
    linear = TSLM(ETHPrice ~ trend()),
    exponential = TSLM(log(ETHPrice) ~ trend()),
    piecewise = TSLM(ETHPrice ~ trend(knots = c(yearmonth("2021 Jan"), yearmonth("2021 Dec"), yearmonth("2022 May"))))
  )
# Forecast
fc_trends <- fit_trends |> forecast(h = 12)
# Plot
data_set |>
  autoplot(ETHPrice) +
  geom_line(data = augment(fit_trends), aes(y = .fitted, color = .model)) +
  autolayer(fc_trends, aes(y = .mean), alpha = 0.5, level = 95) +
  labs(y = "ETHPrice", title = "Trend Analysis for Ethereum Price")


# Fit the models
fit_trends <- data_set|>
  model(
    linear = TSLM(ETHVolume ~ trend()),
    exponential = TSLM(log(ETHVolume) ~ trend()),
    piecewise = TSLM(ETHVolume ~ trend(knots = c(yearmonth("2020 May"), yearmonth("2021 Mar"))))
  )
# Forecast
fc_trends <- fit_trends |> forecast(h = 12)
# Plot
data_set |>
  autoplot(ETHVolume) +
  geom_line(data = augment(fit_trends), aes(y = .fitted, color = .model)) +
  autolayer(fc_trends, aes(y = .mean), alpha = 0.5, level = 95) +
  labs(y = "ETHVolume", title = "Trend Analysis for Ethereum Volume")


# Fit the models
fit_trends <- data_set|>
  model(
    linear = TSLM(BTCPrice ~ trend()),
    exponential = TSLM(log(BTCPrice) ~ trend()),
    piecewise = TSLM(BTCPrice ~ trend(knots = c(yearmonth("2020 Oct"), yearmonth("2021 Mar"), yearmonth("2022 Feb"), yearmonth("2022 Jun"))))
  )
# Forecast
fc_trends <- fit_trends |> forecast(h = 12)
# Plot
data_set |>
  autoplot(BTCPrice) +
  geom_line(data = augment(fit_trends), aes(y = .fitted, color = .model)) +
  autolayer(fc_trends, aes(y = .mean), alpha = 0.5, level = 95) +
  labs(y = "BTCPrice", title = "Trend Analysis for Bitcoin Price")


# Fit the models
fit_trends <- data_set|>
  model(
    linear = TSLM(BTCVol. ~ trend()),
    exponential = TSLM(log(BTCVol.) ~ trend()),
    piecewise = TSLM(BTCVol. ~ trend(knots = c(yearmonth("2021 Mar"), yearmonth("2023 Jan"))))
  )
# Forecast
fc_trends <- fit_trends |> forecast(h = 12)
# Plot
data_set |>
  autoplot(BTCVol.) +
  geom_line(data = augment(fit_trends), aes(y = .fitted, color = .model)) +
  autolayer(fc_trends, aes(y = .mean), alpha = 0.5, level = 95) +
  labs(y = "BTCVol.", title = "Trend Analysis for Bitcoin Volume")


# Fit the models
fit_trends <- data_set|>
  model(
    linear = TSLM(GoldPrice ~ trend()),
    exponential = TSLM(log(GoldPrice) ~ trend()),
    piecewise = TSLM(GoldPrice ~ trend(knots = c(yearmonth("2020 Dec"), yearmonth("2022 Dec"))))
  )
# Forecast
fc_trends <- fit_trends |> forecast(h = 12)
# Plot
data_set |>
  autoplot(GoldPrice) +
  geom_line(data = augment(fit_trends), aes(y = .fitted, color = .model)) +
  autolayer(fc_trends, aes(y = .mean), alpha = 0.5, level = 95) +
  labs(y = "GoldPrice", title = "Trend Analysis for Gold Price")


# Fit the model
fit_trends <- data_set|>
  model(
    linear = TSLM(GoldVolume ~ trend()),
    exponential = TSLM(log(GoldVolume) ~ trend()),
    piecewise = TSLM(GoldVolume ~ trend(knots = c(yearmonth("2020 Dec"), yearmonth("2022 Dec"))))
  )
# Forecast
fc_trends <- fit_trends |> forecast(h = 12)
# Plot
data_set |>
  autoplot(GoldVolume) +
  geom_line(data = augment(fit_trends), aes(y = .fitted, color = .model)) +
  autolayer(fc_trends, aes(y = .mean), alpha = 0.5, level = 95) +
  labs(y = "GoldVolume", title = "Trend Analysis for Gold Volume")


fit_trends <- data_set|>
  model(
    linear = TSLM(CVIPrice ~ trend()),
    exponential = TSLM(log(CVIPrice) ~ trend()),
    piecewise = TSLM(CVIPrice ~ trend(knots = c(yearmonth("2020 Dec"), yearmonth("2022 Dec"))))
  )
# Forecast
fc_trends <- fit_trends |> forecast(h = 12)
# Plot
data_set |>
  autoplot(CVIPrice) +
  geom_line(data = augment(fit_trends), aes(y = .fitted, color = .model)) +
  autolayer(fc_trends, aes(y = .mean), alpha = 0.5, level = 95) +
  labs(y = "CVIPrice", title = "Trend Analysis for CVIPrice")



fit_trends <- data_set|>
  model(
    linear = TSLM(InflationRate... ~ trend()),
    exponential = TSLM(log(InflationRate...) ~ trend()),
    piecewise = TSLM(InflationRate... ~ trend(knots = c(yearmonth("2020 Dec"), yearmonth("2022 Sep"), yearmonth("2023 May"))))
  )
# Forecast
fc_trends <- fit_trends |> forecast(h = 12)
# Plot
data_set |>
  autoplot(InflationRate...) +
  geom_line(data = augment(fit_trends), aes(y = .fitted, color = .model)) +
  autolayer(fc_trends, aes(y = .mean), alpha = 0.5, level = 95) +
  labs(y = "InflationRate", title = "Trend Analysis for Inflation Rate")



fit_trends <- data_set|>
  model(
    linear = TSLM(M1.Billion. ~ trend()),
    exponential = TSLM(log(M1.Billion.) ~ trend()),
    piecewise = TSLM(M1.Billion. ~ trend(knots = c(yearmonth("2020 Mar"), yearmonth("2020 May"))))
  )
# Forecast
fc_trends <- fit_trends |> forecast(h = 12)
# Plot
data_set |>
  autoplot(M1.Billion.) +
  geom_line(data = augment(fit_trends), aes(y = .fitted, color = .model)) +
  autolayer(fc_trends, aes(y = .mean), alpha = 0.5, level = 95) +
  labs(y = "M1.Billion", title = "Trend Analysis for M1.Billion")


fit_trends <- data_set|>
  model(
    linear = TSLM(M2.Billion. ~ trend()),
    exponential = TSLM(log(M2.Billion.) ~ trend()),
    piecewise = TSLM(M2.Billion. ~ trend(knots = c(yearmonth("2020 Jan"), yearmonth("2020 Mar"), yearmonth("2022 Jan"))))
  )
# Forecast
fc_trends <- fit_trends |> forecast(h = 12)
# Plot
data_set |>
  autoplot(M2.Billion.) +
  geom_line(data = augment(fit_trends), aes(y = .fitted, color = .model)) +
  autolayer(fc_trends, aes(y = .mean), alpha = 0.5, level = 95) +
  labs(y = "M2.Billion", title = "Trend Analysis for M2.Billion")

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
  
#4

fit <- data_set|>
  model(
    Holt = ETS(ETHPrice ~ error("A") + trend("A") + season("N")),
    Damped = ETS(ETHPrice ~ error("A") + trend("Ad", phi = 0.9) + season("N"))
  ) 
forecasts <- fit |> forecast(h = 12)
# Plotting
plot <- forecasts |> 
  autoplot(data_set) + 
  labs(title = "Ethereum Price Forecast",
       y = "ETHPrice",
       colour = "Forecast") +
  guides(colour = guide_legend(title = "Forecast Strategy"))
# Display the plot
plot
    


fit <- data_set|>
  model(
    Holt = ETS(ETHVolume ~ error("A") + trend("A") + season("N")),
    Damped = ETS(ETHVolume ~ error("A") + trend("Ad", phi = 0.9) + season("N"))
  ) 
forecasts <- fit |> forecast(h = 12)
# Plotting
plot <- forecasts |> 
  autoplot(data_set) + 
  labs(title = "Ethereum Volume Forecast",
       y = "ETHVolume",
       colour = "Forecast") +
  guides(colour = guide_legend(title = "Forecast Strategy"))
# Display the plot
plot


fit <- data_set|>
  model(
    Holt = ETS(BTCPrice ~ error("A") + trend("A") + season("N")),
    Damped = ETS(BTCPrice ~ error("A") + trend("Ad", phi = 0.9) + season("N"))
  ) 
forecasts <- fit |> forecast(h = 12)
# Plotting
plot <- forecasts |> 
  autoplot(data_set) + 
  labs(title = "Bitcoin Price Forecast",
       y = "BTCPrice",
       colour = "Forecast") +
  guides(colour = guide_legend(title = "Forecast Strategy"))
# Display the plot
plot



fit <- data_set|>
  model(
    Holt = ETS(BTCVol. ~ error("A") + trend("A") + season("N")),
    Damped = ETS(BTCVol. ~ error("A") + trend("Ad", phi = 0.9) + season("N"))
  ) 
forecasts <- fit |> forecast(h = 12)
# Plotting
plot <- forecasts |> 
  autoplot(data_set) + 
  labs(title = "Bitcoin Volume Forecast",
       y = "BTCVolume",
       colour = "Forecast") +
  guides(colour = guide_legend(title = "Forecast Strategy"))
# Display the plot
plot


fit <- data_set|>
  model(
    Holt = ETS(GoldPrice ~ error("A") + trend("A") + season("N")),
    Damped = ETS(GoldPrice ~ error("A") + trend("Ad", phi = 0.9) + season("N"))
  ) 
forecasts <- fit |> forecast(h = 12)
# Plotting
plot <- forecasts |> 
  autoplot(data_set) + 
  labs(title = "Gold Price Forecast",
       y = "GoldPrice",
       colour = "Forecast") +
  guides(colour = guide_legend(title = "Forecast Strategy"))
# Display the plot
plot


###############################################
#Get Rid of outlier
##############################################
fit <- data_set|>
  model(
    Holt = ETS(GoldVolume ~ error("A") + trend("A") + season("N")),
    Damped = ETS(GoldVolume ~ error("A") + trend("Ad", phi = 0.9) + season("N"))
  ) 
forecasts <- fit |> forecast(h = 12)
# Plotting
plot <- forecasts |> 
  autoplot(data_set) + 
  labs(title = "Gold Volume Forecast",
       y = "GoldVolume",
       colour = "Forecast") +
  guides(colour = guide_legend(title = "Forecast Strategy"))
# Display the plot
plot



fit <- data_set|>
  model(
    Holt = ETS(CVIPrice ~ error("A") + trend("A") + season("N")),
    Damped = ETS(CVIPrice ~ error("A") + trend("Ad", phi = 0.9) + season("N"))
  ) 
forecasts <- fit |> forecast(h = 12)
# Plotting
plot <- forecasts |> 
  autoplot(data_set) + 
  labs(title = "CVI Price Forecast",
       y = "CVIPrice",
       colour = "Forecast") +
  guides(colour = guide_legend(title = "Forecast Strategy"))
# Display the plot
plot


fit <- data_set|>
  model(
    Holt = ETS(InflationRate... ~ error("A") + trend("A") + season("N")),
    Damped = ETS(InflationRate... ~ error("A") + trend("Ad", phi = 0.9) + season("N"))
  ) 
forecasts <- fit |> forecast(h = 12)
# Plotting
plot <- forecasts |> 
  autoplot(data_set) + 
  labs(title = "Inflation Rate Forecast",
       y = "InflationRate...",
       colour = "Forecast") +
  guides(colour = guide_legend(title = "Forecast Strategy"))
# Display the plot
plot



fit <- data_set|>
  model(
    Holt = ETS(M1.Billion. ~ error("A") + trend("A") + season("N")),
    Damped = ETS(M1.Billion. ~ error("A") + trend("Ad", phi = 0.9) + season("N"))
  ) 
forecasts <- fit |> forecast(h = 12)
# Plotting
plot <- forecasts |> 
  autoplot(data_set) + 
  labs(title = "M1.Billion Forecast",
       y = "M1.Billion.",
       colour = "Forecast") +
  guides(colour = guide_legend(title = "Forecast Strategy"))
# Display the plot
plot




fit <- data_set|>
  model(
    Holt = ETS(M2.Billion. ~ error("A") + trend("A") + season("N")),
    Damped = ETS(M2.Billion. ~ error("A") + trend("Ad", phi = 0.9) + season("N"))
  ) 
forecasts <- fit |> forecast(h = 12)
# Plotting
plot <- forecasts |> 
  autoplot(data_set) + 
  labs(title = "M2.Billion Forecast",
       y = "M2.Billion.",
       colour = "Forecast") +
  guides(colour = guide_legend(title = "Forecast Strategy"))
# Display the plot
plot