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
library(viridis)

theme_set(theme_minimal())

#connect to sql server 2022 developer edition
con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "SQL Server", 
                      Server = ".\\SQLSERVER01", 
                      Database = "GMAST_DATA", 
                      Trusted_Connection = "True")

#get the GHG emissions - data source HadCRUT5
dfOrigGlbVolTempAn = dbGetQuery(conn = con,
  "SELECT
  	  [time]
  	, [T_Anom_orig]
  	, ([T_Anom_vol] / 5.0 - .5) T_Anom_rescaled
  FROM [GMAST_DATA].[dbo].[vwOrigAndVolcanoTempAnomalies]
  WHERE [time] > 1945")
summary(dfOrigGlbVolTempAn)
head(dfOrigGlbVolTempAn)
str(dfOrigGlbVolTempAn)

#get N3.4 ENSO index - data source NOAA ERSST V5
dfN34ENSOIndexYrAvg = dbGetQuery(conn = con,
  "SELECT
  	  [time_year]
      , ([yearlyAvg] / 5.0 - 1.0) T_Anom_rescaled
  FROM [GMAST_DATA].[dbo].[vw_N34_ENSO_AvgTempAnomPerYear]")
summary(dfN34ENSOIndexYrAvg)
head(dfN34ENSOIndexYrAvg)
str(dfN34ENSOIndexYrAvg)

dfN34ENSOIndexAvg = dbGetQuery(conn = con,
  "SELECT
  	  [time_year]
  	, ([movAvg4yr] / 5.0 - 1.0) T_Anom_rescaled
  FROM [GMAST_DATA].[dbo].[vw_N34_ENSO_MovAvg4yearTempAnom]")
summary(dfN34ENSOIndexAvg)
head(dfN34ENSOIndexAvg)
str(dfN34ENSOIndexAvg)

ggplot() +
  geom_line(data = dfOrigGlbVolTempAn, mapping = aes(x = time, y = T_Anom_orig), color = 'blue', linewidth  = .4, linetype = 'solid') +
  geom_line(data = dfOrigGlbVolTempAn, mapping = aes(x = time, y = T_Anom_rescaled), color = 'grey23', linewidth  = .6, linetype = 'solid') +
  geom_line(data = dfN34ENSOIndexYrAvg, mapping = aes(x = time_year, y = T_Anom_rescaled),
            color = 'black', linewidth  = .5, alpha = .5) +
  geom_line(data = dfN34ENSOIndexAvg, mapping = aes(x = time_year, y = T_Anom_rescaled), color = 'black', linewidth  = 1, linetype = 'solid') +
  scale_y_continuous(breaks = seq(from = -1.5, to = 1.5, by = .5)) +
  scale_x_continuous(breaks = seq(from = 1950, to = 2023, by = 10)) +
  labs(x = 'Year', y = 'T. An. °C', Title = 'Orig. global ST')

#temperature composite rescaled for plotting
dfTempCompRescaled <- dbGetQuery(conn = con,
  "SELECT
  	  [time]
  	, [T_Anom_orig]
  	, [T_Anom_vol_rescaled]
  	, [T_Anom_yearlyAvg_rescaled]
  	, [T_Anom_movAvg4yr_rescaled]
  	, [T_Anom_orig_purged_rescaled]
  	, [T_Anom_orig_purged_4yr_mov_avg_rescaled]
  FROM [GMAST_DATA].[dbo].[vw_TempComponentsRescaled]")
summary(dfTempCompRescaled)
head(dfTempCompRescaled)
str(dfTempCompRescaled)

#max T_anom_orig_rescaled
dfMaxTAnomRescaled <- dbGetQuery(conn = con,
  "SELECT
  	  T2.[Time]
  	, T3.T_Anom_orig_rescaled maxT_Anom_orig_rescaled
  FROM
  (
  	SELECT
  		  [CycleNo]
  		, SSN_max = MAX([SSN_mean]) 
  	FROM [GMAST_DATA].[dbo].[solar_cycles_SSN_number]
  	GROUP BY
  		  [CycleNo]
  ) T
  INNER JOIN [GMAST_DATA].[dbo].[solar_cycles_SSN_number] T2 ON T.CycleNo = T2.CycleNo AND T.SSN_max = T2.SSN_mean
  INNER JOIN [GMAST_DATA].[dbo].[vw_TempComponentsRescaled] T3 ON T3.[time] = T2.[Time]")
summary(dfMaxTAnomRescaled)
head(dfMaxTAnomRescaled)
str(dfMaxTAnomRescaled)


#plotting
ggplot(data = dfTempCompRescaled, mapping = aes(x = time)) +
  geom_line(mapping = aes(y = T_Anom_orig), color = 'black', linewidth  = 1,
            linetype = 'solid') + #ORIG SIGNATURE
  geom_line(mapping = aes(y = T_Anom_vol_rescaled), color = 'black', linewidth  = .6,
            linetype = 'solid') + #VOLCANO SIGNATURE
  geom_line(mapping = aes(y = T_Anom_yearlyAvg_rescaled), color = 'black', linewidth  = 1,
            alpha = .5) + #ENSO SIGNATURE
  geom_line(mapping = aes(y = T_Anom_movAvg4yr_rescaled), color = 'black', linewidth  = .5,
            linetype = 'dashed') + #ENSO SIGNATURE 4 YEAR MOVING AVERAGE
  geom_line(mapping = aes(y = T_Anom_orig_purged_rescaled), color = 'grey2', linewidth  = .6,
            linetype = 'solid') + #ORIG PURGED => ORIG - VOL - ENSO + ENSO 4 YEAR MOV. AVG.
  geom_line(mapping = aes(y = T_Anom_orig_purged_4yr_mov_avg_rescaled), color = 'black', linewidth  = .5,
            linetype = 'dashed') + #4 YEAR MOV. AVG FOR ORIG PURGED
  geom_point(data = dfMaxTAnomRescaled, mapping = aes(x = Time, y = maxT_Anom_orig_rescaled),
             size = 3, shape = 8) + #TSI MAX
  geom_text(aes(x = 2025.5, y = 1.36), label = "TSI max", color = "black", nudge_x = .01, nudge_y = 0.006) +
  geom_text(aes(x = 2023, y = 1.2), label = "a", color = "black", nudge_x = .01, nudge_y = 0.006) +
  geom_text(aes(x = 2023, y = 0.8), label = "b", color = "black", nudge_x = .01, nudge_y = 0.006) +
  geom_text(aes(x = 2023, y = -0.5), label = "c", color = "black", nudge_x = .01, nudge_y = 0.006) +
  geom_text(aes(x = 2023, y = -1), label = "d", color = "black", nudge_x = .01, nudge_y = 0.006) +
  scale_y_continuous(breaks = seq(from = -1.5, to = 1.5, by = .5)) +
  scale_x_continuous(breaks = seq(from = 1950, to = 2023, by = 10)) +
  labs(x = 'Year', y = 'T. Anom. K') +
  ggtitle("Orig. global ST")

#TSI COMPARISON
dfTSIcomcons1978.model <- dbGetQuery(conn = con,
  "SELECT
  	  [time_frac]
  	, [TSI]
  FROM [GMAST_DATA].[dbo].[vw_community_Consensus_daily_TSI_Composite_JD_since_1978]")
summary(dfTSIcomcons1978.model)
head(dfTSIcomcons1978.model)
str(dfTSIcomcons1978.model)

dfPMOD_TSI1978.model <- dbGetQuery(conn = con,
  "SELECT
      [time_frac]
  	, ([TSI] - 5.0) TSI
  FROM [GMAST_DATA].[dbo].[vw_PMOD_until_2018]")
summary(dfPMOD_TSI1978.model)
head(dfPMOD_TSI1978.model)
str(dfPMOD_TSI1978.model)

dfEMPIRE1947_2017.model <- dbGetQuery(conn = con,
  "SELECT
      [time_frac]
    , ([TSI] - 10.0) TSI
  FROM [GMAST_DATA].[dbo].[vw_EMPIRE_recons_daily_1947_2017]
  WHERE [time_frac] > 1978.0")
summary(dfEMPIRE1947_2017.model)
head(dfEMPIRE1947_2017.model)
str(dfEMPIRE1947_2017.model)

ggplot() +
  geom_line(data = dfTSIcomcons1978.model, mapping = aes(x = time_frac, y = TSI),
            linewidth = 1, alpha = .5) +
  geom_text(aes(x = 2018, y = 1364), label = "COMMON CONSENSUS \n (TSI orig.)", color = "black",
            size = 3.0, nudge_x = .01, nudge_y = 0.006) +
  geom_line(data = dfPMOD_TSI1978.model, mapping = aes(x = time_frac, y = TSI),
            linewidth = 1, alpha = .5) +
  geom_text(aes(x = 2018, y = 1358), label = "PMOD \n (TSI - 5 W/m2)", color = "black",
            size = 3.0, nudge_x = .01, nudge_y = 0.006) +
  geom_line(data = dfEMPIRE1947_2017.model, mapping = aes(x = time_frac, y = TSI),
            linewidth = 1, alpha = .5) +
  geom_text(aes(x = 2018, y = 1352.5), label = "EMP. 1947-2017 \n (TSI - 10 W/m2)", color = "black",
            size = 3.0, nudge_x = .01, nudge_y = 0.006) +
  scale_x_continuous(breaks = seq(from = 1975, to = 2025, by = 5)) +
  labs(x = 'Year', y = 'TSI (W/m2)') +
  ggtitle("TSI comparison since 1978 (daily)")

#ESS1
dfESS.model <- dbGetQuery(conn = con,
  "SELECT
  	  [time]
  	, (k1S*DeltaI) ESS1
  	, (k2S*DeltaI) ESS2
  	, ((k1S*DeltaI) + (k2S*DeltaI)) ESS
  FROM
  (
  	SELECT
  		  [time]
  		, DeltaI
  		, -0.323812999508598 DeltaT1S0
  		, .053 k1S
  		, .4 tau1S
  		, 0.41 k2S
  		, 12 tau2S
  	FROM
  	(
  		SELECT
  			  [time]
  			, ([TSI] - prevTSI) DeltaI
  			, -0.323812999508598 DeltaT1S0
  			, .053 k1S
  			, .4 tau1S
  			, 0.41 k2S
  			, 12 tau2S
  		FROM
  		(
  		SELECT
  			  [time]
  			, [TSI]
  			, ISNULL((LAG([TSI]) OVER (ORDER BY [time])), [TSI]) prevTSI
  		FROM [GMAST_DATA].[dbo].[naval_res_lab_tsi_records_since_1610]
  		) T1
  	) T2
  ) T3")
summary(dfESS.model)
head(dfESS.model)
str(dfESS.model)

ggplot(data = dfESS.model, mapping = aes(x = time)) +
  geom_line(mapping = aes(y = ESS1), linewidth = 1, alpha = .5) +
  geom_line(mapping = aes(y = ESS2),
            linewidth = 1, alpha = .5) +
  geom_line(mapping = aes(y = ESS),
            linewidth = 1, alpha = .5) +
  scale_x_continuous(breaks = seq(from = 1600, to = 2025, by = 50)) +
  labs(x = 'Year', y = 'T Anom °C') +
  ggtitle("T Anomalies")

#EMPIRE DATA
dfEMPIRE <- dbGetQuery(conn = con,
  "SELECT
      [time_frac]
    , [TSI]
  FROM [GMAST_DATA].[dbo].[vw_EMPIRE_recons_daily_1947_2017]")
summary(dfEMPIRE)
head(dfEMPIRE)
str(dfEMPIRE)

#11 yr moving average
dfEMPIRE11yrMovAvg <- dbGetQuery(conn = con,
  "SELECT
  	  [time]
  	, AVG(avgTSIyr) OVER (ORDER BY [time] ROWS BETWEEN 11 PRECEDING AND CURRENT ROW) movAvg11yrTSI
  FROM
  (
  	SELECT DISTINCT
  		  FLOOR([time_frac]) [time]
  		, AVG([TSI]) OVER (PARTITION BY FLOOR([time_frac]) ORDER BY FLOOR([time_frac])) avgTSIyr
  	FROM [GMAST_DATA].[dbo].[vw_EMPIRE_recons_daily_1947_2017]
  ) T")
summary(dfEMPIRE11yrMovAvg)
head(dfEMPIRE11yrMovAvg)
str(dfEMPIRE11yrMovAvg)

ggplot() +
  geom_line(data = dfEMPIRE, mapping = aes(x = time_frac, y = TSI),
            linewidth = 1, alpha = .5) +
  geom_line(data = dfEMPIRE11yrMovAvg, mapping = aes(x = time, y = movAvg11yrTSI),
            linewidth = 1, alpha = .5) +
  scale_x_continuous(breaks = seq(from = 1945, to = 2020, by = 5)) +
  labs(x = 'Year', y = 'TSI (W/m2)') +
  ggtitle("EMPIRE data TSI for 1947-2017 with 11yr mov avg")

#ZOOM into a shorter period for seasonality 11-yr cycle 19 (1954-1963)
dfEMPIREcl19 <- dbGetQuery(conn = con,
"SELECT
      [time_frac]
    , [TSI]
  FROM [GMAST_DATA].[dbo].[vw_EMPIRE_recons_daily_1947_2017]
  WHERE [time_frac] BETWEEN 1953.99 AND 1963.999")
summary(dfEMPIREcl19)
head(dfEMPIREcl19)
str(dfEMPIREcl19)

ggplot() +
  geom_line(data = dfEMPIREcl19, mapping = aes(x = time_frac, y = TSI),
            linewidth = 1, alpha = .5) +
  scale_x_continuous(breaks = seq(from = 1952, to = 1966, by = 2)) +
  labs(x = 'Year', y = 'TSI (W/m2)') +
  ggtitle("EMPIRE data TSI for cycle 19 1954-1963")

#TSI since 1610 with 11yr mov average
dfTSI1610 <- dbGetQuery(conn = con,
  "SELECT
  	  [time]
  	, [TSI]
  	, AVG([TSI]) OVER (ORDER BY [time] ROWS BETWEEN 11 PRECEDING AND CURRENT ROW) movAvg11yrTSI
  FROM [GMAST_DATA].[dbo].[naval_res_lab_tsi_records_since_1610]")
summary(dfTSI1610)
head(dfTSI1610)
str(dfTSI1610)

pDt1610 <- ggplot(data = dfTSI1610, mapping = aes(x = time)) +
  geom_line(mapping = aes(y = TSI),
            linewidth = 1, alpha = .5) +
  geom_line(mapping = aes(y = movAvg11yrTSI),
            linewidth = 1) +
  scale_x_continuous(breaks = seq(from = 1600, to = 2025, by = 50)) +
  labs(x = 'Year', y = 'TSI (W/m2)') +
  ggtitle("TSI since 1610 with 11yr mov avg")

#TSI since 1610 with 11yr mov average - zoom in since 1950
dfTSI1950 <- filter(dfTSI1610, time > 1949)
min(dfTSI1950$TSI)
max(dfTSI1950$TSI)
min(dfTSI1950$movAvg11yrTSI)
max(dfTSI1950$movAvg11yrTSI)

pDt1950 <- ggplot(data = dfTSI1950, mapping = aes(x = time)) +
  geom_line(mapping = aes(y = TSI),
            linewidth = 1, alpha = .5) +
  geom_line(mapping = aes(y = movAvg11yrTSI),
            linewidth = 1) +
  scale_x_continuous(breaks = seq(from = 1950, to = 2025, by = 25)) +
  labs(x = 'Year', y = 'TSI (W/m2)') +
  ggtitle("TSI since 1950 with 11yr mov avg")

grid.arrange(pDt1610, pDt1950, ncol = 2)
