setwd("C:/.../.../climateResearch")
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
library(TTR)

theme_set(theme_minimal())

#connect to sql server 2022 developer edition
con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "SQL Server", 
                      Server = ".\\...", 
                      Database = "GMAST_DATA", 
                      Trusted_Connection = "True")

################# DEFINED FUNCTIONS #################
### COMPUTE mean absolute error (MAE) or mean absolute deviation (MAD)
compute_mae <- function(obs, pred) {
  absdiff <- sum(abs(obs-pred))
  mae <- absdiff / length(pred)
  return (mae)
}

### COMPUTE average error (AE)
compute_ae <- function(obs, pred) {
  diff <- sum(obs-pred)
  ae <- diff / length(pred)
  return (ae)
}

#COMPUTE root mean squared error (RMSE)
compute_rmse <- function(obs, pred) {
  sqdiff <- sum((obs-pred)^2)
  rmse <- (sqdiff / length(pred))^.5
  return (rmse)
}

#COMPUTE mean absolute percentage error (MAPE)
compute_mape <- function(obs, pred) {
  perc <- sum(abs(obs-pred)/obs) * 100
  mape <- perc / length(pred)
  return (mape)
}

#COMPUTE mean absolute standard error
compute_mase <- function(obs_test, pred_test, obs_train) {
  absdiff_test <- sum(abs(obs_test-pred_test))
  mae_test <- absdiff_test / length(pred_test)
  vect_absdiff_train <- NULL
  for (i in 2:length(obs_train)) {
    vect_absdiff_train <- c(vect_absdiff_train, abs(obs_train[i]-obs_train[i-1]))
  }
  avg_naive_train <- sum(vect_absdiff_train) / (length(obs_train)-1)
  mase <- mae_test / avg_naive_train
  return (mase)
}

######################################################

#forecasting sample
dfTSI1610FullData <- dbGetQuery(conn = con,
  "SELECT
  	  [time]
  	, [TSI]
  FROM [GMAST_DATA].[dbo].[naval_res_lab_tsi_records_since_1610]")
summary(dfTSI1610FullData)
dim(dfTSI1610FullData)

#define train dataset - approximately half of the total set
dfTSI1610_train <- filter(dfTSI1610FullData, time < 1816)
summary(dfTSI1610_train)
dim(dfTSI1610_train)
#define test dataset
dfTSI1610_test <- filter(dfTSI1610FullData, time > 1815)
summary(dfTSI1610_test)
dim(dfTSI1610_test)

#compute trailing moving average in the train set
#order 4 train
ma4_train <- rollmean(dfTSI1610_test$TSI, 4, align = 'right', fill = NA)
length(ma4_train)
#order 11 train
ma11_train <- rollmean(dfTSI1610_test$TSI, 11, align = 'right', fill = NA)
length(ma11_train)
#order 22 train
ma22_train <- rollmean(dfTSI1610_test$TSI, 22, align = 'right', fill = NA)
length(ma22_train)


#build test dataset bases on the naive forecast => last available data
ma4_test <- rep(tail(ma4_train,1), length(ma4_train))
ma11_test <- rep(tail(ma11_train,1), length(ma11_train))
ma22_test <- rep(tail(ma22_train,1), length(ma22_train))

#compute MAE in test set
compute_mae(dfTSI1610_test$TSI, ma4_test)
compute_mae(dfTSI1610_test$TSI, ma11_test)
compute_mae(dfTSI1610_test$TSI, ma22_test)

#compute AE
compute_ae(dfTSI1610_test$TSI, ma4_test)
compute_ae(dfTSI1610_test$TSI, ma11_test)
compute_ae(dfTSI1610_test$TSI, ma22_test)

#compute RMSE
compute_rmse(dfTSI1610_test$TSI, ma4_test)
compute_rmse(dfTSI1610_test$TSI, ma11_test)
compute_rmse(dfTSI1610_test$TSI, ma22_test)

#compute MAPE
compute_mape(dfTSI1610_test$TSI, ma4_test)
compute_mape(dfTSI1610_test$TSI, ma11_test)
compute_mape(dfTSI1610_test$TSI, ma22_test)

#compute MASE
compute_mase(dfTSI1610_test$TSI, ma4_test, dfTSI1610_train$TSI)
compute_mase(dfTSI1610_test$TSI, ma11_test, dfTSI1610_train$TSI)
compute_mase(dfTSI1610_test$TSI, ma22_test, dfTSI1610_train$TSI)

#applying weighted moving average WMA
### WMA() function used from package TTR
# n = periods (order)
# wts = Vector of weights. Length of wts vector must equal the length of x, or n
wma4_train <- WMA(x = dfTSI1610_train$TSI, n = 4, wts = 1:4)
summary(wma4_train)
length(wma4_train)
#11-ord
wma11_train <- WMA(x = dfTSI1610_train$TSI, n = 11, wts = 1:11)

testlen <- length(dfTSI1610_test$TSI)
#forecast in the test set = naive value = last average
wma4_test <- rep(tail(wma4_train,1),testlen)
summary(wma4_test)
length(wma4_test)
#11-ord
wma11_test <- rep(tail(wma11_train,1),testlen)
length(wma11_test)

#create a variable with estimated TSI in the original dataset with both training and test set
dfTSI1610FullData$wma4 <- c(wma4_train,wma4_test)
dfTSI1610FullData$wma11 <- c(wma11_train,wma11_test)

ggplot(data = dfTSI1610FullData, mapping = aes(x = time)) +
  geom_line(mapping = aes(y = TSI), linewidth = 1, alpha = .5) +
  geom_line(mapping = aes(y = wma4), color = 'navy', linewidth = 1, alpha = .8) +
  geom_line(mapping = aes(y = wma11), color = 'maroon', linewidth = 1, alpha = .8) +
  scale_x_continuous(breaks = seq(from = 1600, to = 2025, by = 50)) +
  labs(x = 'Year', y = 'TSI (W/m2)') +
  ggtitle("TSI since 1610")

#compute errors
compute_mae(dfTSI1610_test$TSI, wma4_test)
compute_ae(dfTSI1610_test$TSI, wma4_test)
compute_rmse(dfTSI1610_test$TSI, wma4_test)
compute_mape(dfTSI1610_test$TSI, wma4_test)
compute_mase(dfTSI1610_test$TSI, wma4_test, dfTSI1610_train$TSI)
#11-ord
compute_mae(dfTSI1610_test$TSI, wma11_test)
compute_ae(dfTSI1610_test$TSI, wma11_test)
compute_rmse(dfTSI1610_test$TSI, wma11_test)
compute_mape(dfTSI1610_test$TSI, wma11_test)
compute_mase(dfTSI1610_test$TSI, wma11_test, dfTSI1610_train$TSI)

#table to recap all errors
# 				    MAE			    AE			  RMSE		  MAPE    		MASE
# 4-ORD TMA		0.3444743	-0.1657734	0.4023381	0.02531488	4.316976
# 11-ORD TMA	0.3777169	-0.2324765	0.4340973	0.02775869	4.733575
# 22-ORD TMA	0.3528606	-0.1839813	0.4101759	0.02593142	4.422073
# 				    WMAE  		WAE 		  	WRMSE 		WMAPE   		WMASE
# 4-ORD TMA		0.557067	0.5570476	0.6668562	0.04092944	6.981202
# 11-ORD TMA	0.5832769	0.5832769	0.6889173	0.0428555	7.309666
#best fit is 4-ord TMA
