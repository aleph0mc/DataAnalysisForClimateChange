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
dfTAnomNormSince1850 <- dbGetQuery(conn = con,
  "SELECT
  	  [time]
  	, [TAnom]
  	, [NormTAnom] NormTAnom
  FROM [GMAST_DATA].[dbo].[vw_TempAnomNormForStats]")
summary(dfTAnomNormSince1850)

#normalized and orig. data comparison
ggplot(data = dfTAnomNormSince1850, mapping = aes(x = time)) +
  labs(x = 'Year', y = 'T. Anom. Orig. vs Norm.') +
  ggtitle("Temp. Anom. Orig/Norm. [0.5,1.5] comparison") +
  geom_line(mapping = aes(y = TAnom, col = 'Orig'), linewidth = 1, alpha = .5) +
  geom_line(mapping = aes(y = NormTAnom, col = 'Norm'), linewidth = 1, alpha = .5) +
  scale_colour_manual(name = 'Scales',
                      breaks=c('Orig', 'Norm'),
                      values =c('Orig'='maroon', 'Norm' = 'grey'),
                      labels = c('Original', 'Normalized [0.5,1.5]')) +
  scale_x_continuous(breaks = seq(from = 1850, to = 2025, by = 25))

#all data
dfDataAll = dfTAnomNormSince1850
#training model
dfDataTrain <- filter(dfDataAll, time < 1937)
#test model
dfDataTest <- filter(dfDataAll, time > 1936)
nTestPeriod <- nrow(dfDataTest)

###############################################################

# MAN = multiplicative error, additive trend, no seasonality with alpha = .2
model_man <- ets(dfDataTrain$NormTAnom, model = 'MAN', alpha = .2)
#show forecast values
#model_man$fitted
#predictions for the test set with point forecast
#forecasts are no longer identical as it happened with simpl. exp. smooth.
pred_man <- forecast(model_man, h = nTestPeriod, level = 0)

pred_man_complete <- c(model_man$fitted, pred_man$mean)
dfDataAll$pred_NormTAnom <- pred_man_complete

#compute the accuracy metrics
accuracy(pred_man, dfDataTest$NormTAnom)

#plot shows estimation is not too bad
pMAN <- ggplot(data = dfDataAll, mapping = aes(x = time)) +
  labs(x = 'Year', y = 'Temp. Anom. Norm.') +
  ggtitle("Temp. Anom. Norm. since 1850 - model MAN") +
  geom_line(mapping = aes(y = NormTAnom), color = 'grey', linewidth = 1, alpha = .5) +
  geom_line(mapping = aes(y = pred_NormTAnom), color = 'blue', linewidth = 1, alpha = .5) +
  geom_vline(aes(xintercept = 1937), color = 'black', linewidth = 1) +
  scale_x_continuous(breaks = seq(from = 1850, to = 2025, by = 25))

###############################################################

# MMN = multiplicative error, multiplicative trend, no seasonality with alpha = .81
model_mmn <- ets(dfDataTrain$NormTAnom, model = 'MMN', alpha = .81)
#show forecast values
#model_mmn$fitted
#predictions for the test set with point forecast
#forecasts are no longer identical as it happened with simpl. exp. smooth.
pred_mmn <- forecast(model_mmn, h = nTestPeriod, level = 0)

pred_mmn_complete <- c(model_mmn$fitted, pred_mmn$mean)
dfDataAll$pred_NormTAnom <- pred_mmn_complete

#compute the accuracy metrics
accuracy(pred_mmn, dfDataTest$NormTAnom)
#alpha = 0.81 gives a better approximation

#plot shows estimation is not too bad
pMMN <- ggplot(data = dfDataAll, mapping = aes(x = time)) +
  labs(x = 'Year', y = 'Temp. Anom. Norm.') +
  ggtitle("Temp. Anom. Norm. since 1850 - model MMN") +
  geom_line(mapping = aes(y = NormTAnom), color = 'grey', linewidth = 1, alpha = .5) +
  geom_line(mapping = aes(y = pred_NormTAnom), color = 'blue', linewidth = 1, alpha = .5) +
  geom_vline(aes(xintercept = 1937), color = 'black', linewidth = 1) +
  scale_x_continuous(breaks = seq(from = 1850, to = 2025, by = 25))
###############################################################

grid.arrange(pMAN, pMMN, nrow = 2)

#test to forecast for the following 3 years - level 0 = point forecast
pred_mmn_f <- forecast(model_mmn, h = nTestPeriod+3, level = 0)
#extract last 3 predictions
tail(pred_mmn_f$mean, 3)

#test with confidence level 95%
pred_mmn95cl <- forecast(model_mmn, h = nTestPeriod, level = c(.95))
autoplot(pred_mmn95cl)
