setwd("C:/Users/milko/OneDrive/Documenti/R_statistics_language/rScriptsForClimateResearch")
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
                      Server = ".\\SQLSERVER01", 
                      Database = "GMAST_DATA", 
                      Trusted_Connection = "True")

#Temp. anomaly
dfSSN_since_1749 <- dbGetQuery(conn = con,
  "SELECT
  	  [time]
  	, [full_date]
  	, [SSN_monthly]
  FROM [GMAST_DATA].[dbo].[SSN_monthly_since_1749]")
summary(dfSSN_since_1749)

#SSN plot
ggplot(data = dfSSN_since_1749, mapping = aes(x = time, y = SSN_monthly)) +
  geom_line(linewidth = 1, alpha = .5) +
  scale_x_continuous(breaks = seq(from = 1745, to = 2025, by = 25)) +
  labs(x = 'Year', y = 'SSN') +
  ggtitle("SSN since 1749")

dfDataAll <- data.frame(time = dfSSN_since_1749$full_date, SSN_monthly = dfSSN_since_1749$SSN_monthly)
ssn_ts <- ts(dfDataAll$SSN_monthly, frequency = 132) #11yr cycle times 12 months
decomp_ssn_ts <- decompose(ssn_ts)
autoplot(decomp_ssn_ts)

#training model
dfDataTrain <- filter(dfDataAll, time < ymd('1980-01-01'))
#test model
dfDataTest <- filter(dfDataAll, time > ymd('1979-12-31'))
nTestPeriod <- nrow(dfDataTest)

#TS in the training set
SSN_train_ts <- ts(dfDataTrain$SSN_monthly)

#IMPORTANT: test for stationarity
adf.test(SSN_train_ts, alternative = 'stationary')
#p is low null must go => H0 rejected => TS is stationary (no differencing necessary => d = 0)
#as a consequence we can use an ARMA(p,q) model, that is ARIMA(p,0,q)

#identify the model
#autocorrelation functions and charts
par(mfrow = c(1,2))
Pacf(SSN_train_ts) #for AR process - analyzing chart parameter p = 1 seems to be good
Acf(SSN_train_ts) #for MA process - analyzing chart parameter q = 1 seems to be good
#reset partition plot
par(mfrow = c(1,1))

#build forecasting model ARIMA - order = c(p,d,q)
#method = ML maximum-likelihood
ssn_model <- Arima(SSN_train_ts, order = c(1,0,1), method = 'ML')
summary(ssn_model)
par(mfrow = c(1,2))
plot(ssn_model$fitted)
plot(ssn_model$residuals)
par(mfrow = c(1,1))

#forecast in the test set - point forecast
ssn_pred_test <- forecast(ssn_model, h = nTestPeriod, level = 0)
#forecast accuracy
accuracy(ssn_pred_test, dfDataTest$SSN_monthly)

#plot forecast values
dfDataAll$ssn_pred <- c(ssn_model$fitted, ssn_pred_test$mean)
#need a date class
dfDataAll$fullDate <- as.Date(dfDataAll$time)
#plot - very bad performance
ggplot(data = dfDataAll, mapping = aes(x = fullDate)) +
  labs(x = 'Year', y = 'SSN') +
  ggtitle("SSN since 1749") +
  geom_line(mapping = aes(y = SSN_monthly), color = 'grey', linewidth = 1, alpha = .5) +
  geom_line(mapping = aes(y = ssn_pred), color = 'blue', linewidth = 1, alpha = .5) +
  geom_vline(aes(xintercept = as.Date('1980-01-01')), color = 'black', linewidth = 1) +
  scale_x_date(breaks = seq(as.Date("1745-01-01"), as.Date("2024-12-31"), by="25 years"), date_labels="%Y")
