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

theme_set(theme_minimal())

#connect to sql server 2022 developer edition
con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "SQL Server", 
                      Server = ".\\SQLSERVER01", 
                      Database = "GMAST_DATA", 
                      Trusted_Connection = "True")

#getting full recordset
dfTSIdata <- dbGetQuery(conn = con,
  "SELECT
  	  [time]
  	, [TSI]
  FROM [GMAST_DATA].[dbo].[naval_res_lab_tsi_records_since_1610]")
summary(dfTSIdata)

#plot data at first glance - there is a sewasonality ands a slow trend
ggplot(data = dfTSIdata, mapping = aes(x = time)) +
  geom_line(mapping = aes(y = TSI), linewidth = 1, alpha = .5) +
  scale_x_continuous(breaks = seq(from = 1610, to = 2025, by = 25)) +
  labs(x = 'Year', y = 'TSI (W/m2)') +
  ggtitle("TSI since 1610")

#explore to identify possible seasonality - 11yr cycle - cycle #10-11 
dfTSIdata11yrCl <- dbGetQuery(conn = con,
  "SELECT
  	  [time]
  	, [TSI]
  FROM [GMAST_DATA].[dbo].[naval_res_lab_tsi_records_since_1610]
  WHERE [time] BETWEEN 1855 AND 1877")
summary(dfTSIdata11yrCl)

ggplot(data = dfTSIdata11yrCl, mapping = aes(x = time)) +
  geom_line(mapping = aes(y = TSI), linewidth = 1, alpha = .5) +
  scale_x_continuous(breaks = seq(from = 1850, to = 1880, by = 10)) +
  labs(x = 'Year', y = 'T Anom °C') +
  ggtitle("Annual Temp. Anom. (°C) 1850-1880")

#getting data
dfDataAll <- dfTSIdata

#define a training model
dfDataTrain <- filter(dfDataAll, time < 1817)
#define a test model
dfDataTest <- filter(dfDataAll, time > 1816)
nTestPeriod <- nrow(dfDataTest)

#building exp. smooth. data model
#error, trend, seasonality in order => method = 'ZZZ';
#Z => error type:
#A => A(dditive); M => M(ultiplicative), N => N(one)

#our model => assumption
#ANN => add error, no trend, no seasonality
model_ses <- ets(dfDataTrain$TSI, model = 'ANN', alpha = .3)
#show forecast values
model_ses$fitted
#forecast in test set
#prediction model, horizon, conf. level = 0 => point forecast
pred_ses <- forecast(model_ses, h = nTestPeriod, level = 0)
#print(pred_ses) #all values are the same => naive forecast

#build simple exp. smooth. (ses) vector with mean for pred. values
pred_seses <- c(model_ses$fitted, pred_ses$mean)
#append to model
dfDataAll$TSIpred_ses <- pred_seses

#perform plot
ggplot(data = dfDataAll, mapping = aes(x = time)) +
  labs(x = 'Year', y = 'TSI (W/m2)') +
  ggtitle("TSI since 1610") +
  geom_line(mapping = aes(y = TSI), color = 'grey', linewidth = 1, alpha = .5) +
  geom_line(mapping = aes(y = TSIpred_ses), color = 'blue', linewidth = 1, alpha = .5) +
  geom_vline(aes(xintercept = 1817), color = 'black', linewidth = 1)
  scale_x_continuous(breaks = seq(from = 1610, to = 2025, by = 25))

#compute the accuracy metrics
accuracy(pred_ses, dfDataTest$TSI)
#as expected quite bad prediction as MPE for test set is > 50%

#let us use a conf level of 95% for the predictions
pred_sesCl95 <- forecast(model_ses, h = nTestPeriod, level = c(.95))
plot(pred_sesCl95)

#ses shows bad fit predictions for this kind of data
accuracy(pred_sesCl95, dfDataTest$TSI)
