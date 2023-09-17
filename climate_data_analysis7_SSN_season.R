setwd("C:/.../ClimateResearch")
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
#WORKING ON TS on temp. anom. normalized (x-xmin)/(xmax-xmin)+0.5 to [0.5,1.5]
dfSSNsinceCl1 <- dbGetQuery(conn = con,
  "SELECT
  	  [Time]
  	, [SSN_mean]
  FROM [GMAST_DATA].[dbo].[solar_cycles_SSN_number]")
summary(dfSSNsinceCl1)

#plot does not identify a trend
ggplot(data = dfSSNsinceCl1, mapping = aes(x = Time)) +
  geom_line(mapping = aes(y = SSN_mean), linewidth = 1, alpha = .5) +
  scale_x_continuous(breaks = seq(from = 1755, to = 2025, by = 25)) +
  labs(x = 'Year', y = 'SSN') +
  ggtitle("SSN since 1755")

#zoom in to identify seasons - choose cycles 5 and 6
#plot identifies a seasonality of 11yr as expected
#11 is the number of seasons
dfSSNseason <- filter(dfSSNsinceCl1, Time > 1797, Time < 1823)
ggplot(data = dfSSNseason, mapping = aes(x = Time)) +
  geom_line(mapping = aes(y = SSN_mean), linewidth = 1, alpha = .5) +
  scale_x_continuous(breaks = seq(from = 1790, to = 1825, by = 2)) +
  labs(x = 'Year', y = 'SSN') +
  ggtitle("SSN Cycles 5-6")

#Holt-Winter's method
#all data
dfDataAll = dfSSNsinceCl1
#training model
dfDataTrain <- filter(dfDataAll, Time < 1889)
#test model
dfDataTest <- filter(dfDataAll, Time > 1888)
nTestPeriod <- nrow(dfDataTest)

#create seasonal time series for the training set
SSN_train_ts <- ts(data = dfDataTrain$SSN_mean, freq = 11)
#decompose the series into seasonal pattern
SSN_decomp <- decompose(SSN_train_ts)
autoplot(SSN_decomp)

#Holt-Winter model
#AAA, addit. error, addit. trend, addit. seasonality
model_ssn_hw <- ets(SSN_train_ts, model = 'AAA', alpha = .9)
print(model_ssn_hw$fitted)

#forecasts are no longer identical as it happened with simpl. exp. smooth.
pred_tmp_ssn <- forecast(model_ssn_hw, h = nTestPeriod, level = 0)
#prediction vector
pred_ssn_hw <- c(model_ssn_hw$fitted,pred_tmp_ssn$mean)
dfDataAll$predSSNhw <- pred_ssn_hw

#plot
ggplot(data = dfDataAll, mapping = aes(x = Time)) +
  labs(x = 'Year', y = 'Temp. Anom. Norm.') +
  ggtitle("SSN since 1755 - HW-model ANA") +
  geom_line(mapping = aes(y = SSN_mean), color = 'grey', linewidth = 1, alpha = .5) +
  geom_line(mapping = aes(y = predSSNhw), color = 'blue', linewidth = 1, alpha = .5) +
  geom_vline(aes(xintercept = 1888), color = 'black', linewidth = 1) +
  scale_x_continuous(breaks = seq(from = 1755, to = 2025, by = 25))

#compute the accuracy metrics
accuracy(pred_ssn_hw, dfDataTest$SSN_mean)
#HW not good to be applied to SSN anyway
#forecast for the next 11 years
pred_f_cycle <- forecast(model_ssn_hw, h = nTestPeriod+11, level = 0)
tail(pred_f_cycle$mean, 11)
autoplot(tail(pred_f_cycle$mean, 11))

#we can build an automated model using ZZZ for the model parameter
#the function will select the best-fit model for our data
#best-fit model => lowest AIC (Akaike Information Criterion) value
#drawback => model with lowest AIC is not always the best model
model_ssn__hw_auto <- ets(SSN_train_ts, model = 'ZZZ', alpha = .9)
print(model_ssn__hw_auto)
#prediction
pred_tmp_ssn_auto <- forecast(model_ssn__hw_auto, h = nTestPeriod, level = 0)
#compute the accuracy metrics
accuracy(pred_tmp_ssn_auto, dfDataTest$SSN_mean)
#HW not good as expected
#prediction vector
pred_ssn_hw_auto <- c(model_ssn_hw$fitted,pred_tmp_ssn_auto$mean)
dfDataAll$predSSNhwAuto <- pred_ssn_hw_auto
#plot - very bad performance - worse than the one above
ggplot(data = dfDataAll, mapping = aes(x = Time)) +
  labs(x = 'Year', y = 'Temp. Anom. Norm.') +
  ggtitle("SSN since 1755 - HW-model ANA") +
  geom_line(mapping = aes(y = SSN_mean), color = 'grey', linewidth = 1, alpha = .5) +
  geom_line(mapping = aes(y = predSSNhwAuto), color = 'blue', linewidth = 1, alpha = .5) +
  geom_vline(aes(xintercept = 1888), color = 'black', linewidth = 1) +
  scale_x_continuous(breaks = seq(from = 1755, to = 2025, by = 25))
