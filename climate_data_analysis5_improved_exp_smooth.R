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

dfDataAll <- dfTSIdata

#define a training model
dfDataTrain <- filter(dfDataAll, time < 1817)
#define a test model
dfDataTest <- filter(dfDataAll, time > 1816)
nTestPeriod <- nrow(dfDataTest)

#our advanced exp. smooth. (aes) model => assumption
#AAN => add. error, add. trend, no seasonality
#we use a rescaled Temp. anom. value to have positive value for holt-winters (2+x)
model_man <- ets(dfDataTrain$TSI, model = 'AAN', alpha = .1)
#show forecast values
model_man$fitted
#predictions for the test set with point forecast
#forecasts are no longer identical as it happened with simpl. exp. smooth.
pred_man <- forecast(model_man, h = nTestPeriod, level = 0)

pred_man_complete <- c(model_man$fitted, pred_man$mean)
dfDataAll$TSIpred_man <- pred_man_complete

#perform plot - forecast not good at all for test data
ggplot(data = dfDataAll, mapping = aes(x = time)) +
  labs(x = 'Year', y = 'TSI (W/m2)') +
  ggtitle("TSI since 1610") +
  geom_line(mapping = aes(y = TSI), color = 'grey', linewidth = 1, alpha = .5) +
  geom_line(mapping = aes(y = TSIpred_man), color = 'blue', linewidth = 1, alpha = .5) +
  geom_vline(aes(xintercept = 1817), color = 'black', linewidth = 1)
scale_x_continuous(breaks = seq(from = 1610, to = 2025, by = 25))

#compute the accuracy metrics
accuracy(pred_man, dfDataTest$TSI)