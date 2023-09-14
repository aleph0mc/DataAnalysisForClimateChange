setwd("C:/.../.../climateResearch")getwd()

library(ggplot2)
library(gridExtra)
library(ggrepel)
library(ggcorrplot)
library(dplyr)
library(DBI)
library(nls2) #nonlinear regression
library(nlshelper)
library(zoo)

theme_set(theme_minimal())

#connect to sql server 2022 developer edition
con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "SQL Server", 
                      Server = ".\\...", 
                      Database = "GMAST_DATA", 
                      Trusted_Connection = "True")

#TSI since 1610
dfTSI1610 <- dbGetQuery(conn = con,
  "SELECT
  	  [time]
  	, (TSIprevYear-TSIcurr)*.05 DeltaT1S
	, (TSIprevYear-TSIcurr)*.4 DeltaT2S
	, ((TSIprevYear-TSIcurr)*.05 + (TSIprevYear-TSIcurr)*.4) ESS
  FROM
  (
  	SELECT
  		[time]
  		, [TSI] TSIcurr
  		, LAG([TSI]) OVER(ORDER BY [time]) TSIprevYear 
  	FROM [GMAST_DATA].[dbo].[naval_res_lab_tsi_records_since_1610]
  ) T")
summary(dfTSI1610)
head(dfTSI1610)
str(dfTSI1610)

ggplot(data = dfTSI1610, mapping = aes(x = time)) +
  geom_line(mapping = aes(y = DeltaT1S), linewidth = 1, alpha = .5) +
  geom_line(mapping = aes(y = DeltaT2S), linewidth = 1, alpha = .8) +
  geom_line(mapping = aes(y = ESS), color = 'blue', linewidth = 1, alpha = .5) +
  scale_x_continuous(breaks = seq(from = 1600, to = 2025, by = 50)) +
  labs(x = 'Year', y = 'T Anom °C') +
  ggtitle("Temp. Anom. (°C) since 1610")

#temp anomaly vs ln(ln(TSI)) since 1850
dfTempAnom <- dbGetQuery(conn = con,
  "SELECT
  	  A.[Time] [time]
  	, A.[Anomaly] [T_anom]
  	, B.TSI TSI
  FROM [GMAST_DATA].[dbo].[HadCRUT5_global_annual_temp_anomaly_since_1850] A
  INNER JOIN [dbo].[naval_res_lab_tsi_records_since_1610] B ON A.[Time] = B.[time]")
summary(dfTempAnom)
head(dfTempAnom)
str(dfTempAnom)

tAnom <- ggplot(data = dfTempAnom, mapping = aes(x = time)) +
  geom_line(mapping = aes(y = T_anom), linewidth = 1, alpha = .5) +
  scale_x_continuous(breaks = seq(from = 1850, to = 2025, by = 25)) +
  labs(x = 'Year', y = 'T Anom °C') +
  ggtitle("Temp. Anom. since 1850")

tsiVal <- ggplot(data = dfTempAnom, mapping = aes(x = time)) +
  geom_line(mapping = aes(y = TSI), linewidth = 1, alpha = .5) +
  scale_x_continuous(breaks = seq(from = 1850, to = 2025, by = 25)) +
  labs(x = 'Year', y = 'TSI (W/m2)') +
  ggtitle("TSI since 1850")

grid.arrange(tAnom, tsiVal, nrow = 2)

#empirical dataset
dfEmpData <- dbGetQuery(conn = con,
  "SELECT
  	  [time]
  	, DeltaT1S ESS1
  	, DeltaT2S ESS2
  	, DeltaT1S+DeltaT2S ESS
  FROM
  (
  	SELECT
  		  [time]
  		, ((TSI-prev_TSI)*0.053)-([T_anom]-prev_T_anom)*0.4 DeltaT1S
  		, ((TSI-prev_TSI)*0.4)-([T_anom]-prev_T_anom)*12 DeltaT2S
  	FROM
  	(
  		SELECT
  			  A.[Time] [time]
  			, A.[Anomaly] [T_anom]
  			, LAG(A.[Anomaly]) OVER(ORDER BY A.[Anomaly]) prev_T_anom
  			, B.TSI TSI
  			, LAG(B.TSI) OVER(ORDER BY B.TSI) prev_TSI
  		FROM [GMAST_DATA].[dbo].[HadCRUT5_global_annual_temp_anomaly_since_1850] A
  		INNER JOIN [GMAST_DATA].[dbo].[naval_res_lab_tsi_records_since_1610] B ON A.[Time] = B.[time]
  	) T
  ) T2
  WHERE DeltaT1S IS NOT NULL AND DeltaT2S IS NOT NULL")
summary(dfEmpData)

pESS1 <- ggplot(data = dfEmpData, mapping = aes(x = time)) +
  geom_line(mapping = aes(y = ESS1), linewidth = 1, alpha = .5) +
  scale_x_continuous(breaks = seq(from = 1850, to = 2025, by = 25)) +
  labs(x = 'Year', y = 'T Anom °C') +
  ggtitle("Temp. Anom. since 1850")

pESS2 <- ggplot(data = dfEmpData, mapping = aes(x = time)) +
  geom_line(mapping = aes(y = ESS2), linewidth = 1, alpha = .5) +
  scale_x_continuous(breaks = seq(from = 1850, to = 2025, by = 25)) +
  labs(x = 'Year', y = 'T Anom °C') +
  ggtitle("Temp. Anom. since 1850")

pESS <- ggplot(data = dfEmpData, mapping = aes(x = time)) +
  geom_line(mapping = aes(y = ESS), linewidth = 1, alpha = .5) +
  scale_x_continuous(breaks = seq(from = 1850, to = 2025, by = 25)) +
  labs(x = 'Year', y = 'T Anom °C') +
  ggtitle("Temp. Anom. since 1850")

grid.arrange(pESS1, pESS2, pESS, nrow = 3)

#sunspot numbers vs TSI
dfSSNvsTSI <- dbGetQuery(conn = con,
  "SELECT 
  	  [time]
  	, [SSN_yearly_mean]
  	, [TSI]
  FROM [GMAST_DATA].[dbo].[SSN_vs_TSI_since_1700]")
summary(dfSSNvsTSI)

pSSN <- ggplot(data = dfSSNvsTSI, mapping = aes(x = time)) +
  geom_line(mapping = aes(y = SSN_yearly_mean), linewidth = 1, alpha = .5) +
  scale_x_continuous(breaks = seq(from = 1700, to = 2025, by = 25)) +
  labs(x = 'Year', y = 'SSN') +
  ggtitle("SSN since 1700")

pTSI <- ggplot(data = dfSSNvsTSI, mapping = aes(x = time)) +
  geom_line(mapping = aes(y = TSI), linewidth = 1, alpha = .5) +
  scale_x_continuous(breaks = seq(from = 1700, to = 2025, by = 25)) +
  labs(x = 'Year', y = 'TSI (W/m2)') +
  ggtitle("TSI since 1700")

grid.arrange(pSSN, pTSI, nrow = 2)
