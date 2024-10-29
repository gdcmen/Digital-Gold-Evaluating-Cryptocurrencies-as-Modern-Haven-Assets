library(dplyr)
library(tsibble)
library(tidyverse)
mydata = read.csv("Crypto_DataSet_ADS_csv.csv")
df=mydata %>%
  mutate(Month = yearmonth(Date)) %>%
  as_tsibble(index = Month)
print(df, n=10)