setwd("C:/.../...")
getwd()

library(ggplot2)
library(gridExtra)
library(ggrepel)
library(ggcorrplot)
library(dplyr)
library(DBI)
library(nls2) #nonlinear regression
library(nlshelper)
library(zoo)
library(forecast)
library(lubridate)

theme_set(theme_minimal())

#connect to sql server 2022 developer edition
con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "SQL Server", 
                      Server = ".\\...", 
                      Database = "GMAST_DATA", 
                      Trusted_Connection = "True")

#Temp. anomaly
dfTAnom1850 <- dbGetQuery(conn = con,
  "SELECT
  	  [time]
  	, [TAnom]
  FROM [GMAST_DATA].[dbo].[vw_TempAnomNormForStats]")
summary(dfTAnom1850)


#temp. anom. plot with order 4 smooth
ggplot(data = dfTAnom1850, mapping = aes(x = time, y = TAnom)) +
  geom_line(linewidth = 1, alpha = .5) +
  geom_smooth(method = 'gam', formula = y ~ poly(x, 4), color = 'midnightblue', linetype = 'solid', se = T) +
  scale_x_continuous(breaks = seq(from = 1755, to = 2025, by = 25)) +
  labs(x = 'Year', y = 'TAnom') +
  ggtitle("TAnom since 1850")

#all data
dfDataAll = dfTAnom1850
#training model
dfDataTrain <- filter(dfDataAll, time < 1937)
#test model
dfDataTest <- filter(dfDataAll, time > 1936)
nTestPeriod <- nrow(dfDataTest)

#create a TS
temp_anom_train_ts <- ts(data = dfDataTrain$TAnom)

#1st approach linear model - tslm (time series linear model)
model_lm <- tslm(temp_anom_train_ts ~ trend)
summary(model_lm)
#p-value = .194 > .05 => linear model not fitting well data

#poly model - order 4 poly
model_pol4 <- tslm(temp_anom_train_ts ~ poly(trend, 4))
summary(model_pol4)
#p-value is low, but R-squared also low
#let us try a point forecast
tanom_pred <- forecast(model_pol4, h = nTestPeriod, level = 0)
#compute accuracy
accuracy(tanom_pred, dfDataTest$TAnom)
#fitted values in our data
dfDataTrain$pred_train <- model_pol4$fitted.values
#forecasts in the test set
dfDataTest$pred_test <- tanom_pred$mean

#add data
dfDataAll$pred_train <- dfDataTrain$pred_train
dfDataAll$pred_test <- dfDataTest$pred_test

#plot - very bad performance
ggplot(data = dfDataAll, mapping = aes(x = time)) +
  labs(x = 'Year', y = 'Temp. Anom.') +
  ggtitle("TAnom since 1755") +
  geom_line(mapping = aes(y = TAnom), color = 'grey', linewidth = 1, alpha = .5) +
  geom_line(mapping = aes(y = pred_train), color = 'blue', linewidth = 1, alpha = .5) +
  geom_line(mapping = aes(y = pred_test), color = 'red', linewidth = 1, alpha = .5) +
  geom_vline(aes(xintercept = 1936), color = 'black', linewidth = 1) +
  scale_x_continuous(breaks = seq(from = 1755, to = 2025, by = 25))

#log. transf. cannot be applied bcos of neg. values
