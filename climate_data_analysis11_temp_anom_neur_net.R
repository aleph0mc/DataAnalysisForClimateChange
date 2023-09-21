setwd("C:/...")
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
library(tseries) #Time series analysis

theme_set(theme_minimal())

#connect to sql server 2022 developer edition
con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "SQL Server", 
                      Server = ".\\...", 
                      Database = "GMAST_DATA", 
                      Trusted_Connection = "True")

#Temp. anomaly
dfTAnomSince1850 <- dbGetQuery(conn = con,
  "SELECT
  	  [time]
  	, [TAnom]
  FROM [GMAST_DATA].[dbo].[vw_TempAnomNormForStats]")
summary(dfTAnomSince1850)

#plot
ggplot(data = dfTAnomSince1850, mapping = aes(x = time, y = TAnom)) +
  geom_line(linewidth = 1, alpha = .5) +
  scale_x_continuous(breaks = seq(from = 1850, to = 2025, by = 25)) +
  labs(x = 'Year', y = 'TAnom °C') +
  ggtitle("Temp. Anom. since 1850")

dfDataAll <- dfTAnomSince1850

#training model
dfDataTrain <- filter(dfDataAll, time < 1981)
#test model
dfDataTest <- filter(dfDataAll, time > 1980)
nTestPeriod <- nrow(dfDataTest)

#TS in the training set
tanom_train_ts <- ts(dfDataTrain$TAnom)

#build NN
tanom_net <- nnetar(tanom_train_ts, repeats = 20)
#check some parameters
print(tanom_net$p) # lags
print(tanom_net$P) # seasonal lags - 0 means non-seasonal
print(tanom_net$size) # nodes in the hidden layers

#model ouputs
tanom_net$fitted
plot(tanom_net$fitted)
print(tanom_net$residuals)

#try forecast in test set - point forecast (also set as default)
pred_tanom_test <- forecast(tanom_net, h = nTestPeriod, level = 0)
print(pred_tanom_test$mean)

#predfiction accuracy
accuracy(pred_tanom_test,dfDataTest$TAnom)

#plot
dfDataAll$TAnom_pred <- c(tanom_net$fitted, pred_tanom_test$mean)
ggplot(data = dfDataAll, mapping = aes(x = time)) +
  labs(x = 'Year', y = 'TAnom °C') +
  ggtitle("TAnom since 1850") +
  geom_line(mapping = aes(y = TAnom), color = 'grey', linewidth = 1, alpha = .5) +
  geom_line(mapping = aes(y = TAnom_pred), color = 'blue', linewidth = 1, alpha = .5) +
  geom_vline(aes(xintercept = 1980), color = 'black', linewidth = 1) +
  scale_x_continuous(breaks = seq(from = 1850, to = 2025, by = 25))
