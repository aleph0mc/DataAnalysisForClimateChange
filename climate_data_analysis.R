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
#library(lipdR) #for LiPD files
#set white backgroud for ggplots
theme_set(theme_minimal())

#connect to sql server 2022 developer edition
con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "SQL Server", 
                      Server = ".\\...", 
                      Database = "GMAST_DATA", 
                      Trusted_Connection = "True")

#get the recordset for temperature anomaly in a data frame
dfTempAnom = dbGetQuery(conn = con,
  "SELECT 
  	  [time]
  	, [timebound_lower]
  	, [timebound_upper]
  	, [gmst_anomaly_around_14degC]
  	, (14.0 + [gmst_anomaly_around_14degC]) GMST
  FROM [GMAST_DATA].[dbo].[IPCC_yearly_global_mean_temperature]
  ORDER BY [time]")
#class(dfTempAnom) #confirm it is a data frame
#print(dfTempAnom)

ggplot(dfTempAnom, aes(x = time, y = gmst_anomaly_around_14degC)) +
  geom_point(position = 'jitter', alpha = .5) +
  geom_smooth(se = F) +
  geom_smooth(method = 'gam', color = 'red', linetype = 'dashed', se = F) + #gam = generalized additive mode smoothing
  geom_smooth(method = 'lm', se = F, color = 'green')

#get the GHG emissions using IPCC data
dfGhgEmission = dbGetQuery(conn = con,
    "SELECT
  	  [time]
  	, [timebound_lower]
  	, [timebound_upper]
  	, [gas]
  	, [units]
  	, [value]
  	, [gas_descr]
  FROM [GMAST_DATA].[dbo].[IPCC_GHG_emission]")

ggplot(dfGhgEmission, aes(x = time, y = value)) +
  geom_point(mapping = aes(color = gas), position = 'jitter', alpha = .5) +
  geom_smooth(mapping = aes(color = gas), method = 'gam', color = 'red', linetype = 'dashed', se = F) + #gam = generalized additive mode smoothing
  facet_grid(cols = vars(gas)) +
  labs(x = 'Year', y = 'GTons', Title = 'Gas vs Emission')


ggplot(dfGhgEmission, aes(x = time, y = value)) +
  geom_point() +
  facet_wrap(~gas)

fit <- lm(time ~ value + gas, data = dfGhgEmission)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))

summary(dfGhgEmission)
head(dfGhgEmission[, c(4,6)])

#get the global data
dfGlobalTempAndRF = dbGetQuery(conn = con,
  "SELECT 
  	  [time]
  	, [TGL]
  	, [GHG]
  	, [SOL]
  	, [VUL]
  	, [ENSO]
  FROM [GMAST_DATA].[dbo].[Global_temp_and_RF]")
summary(dfGlobalTempAndRF)
head(dfGlobalTempAndRF)

#plot time vs entity
ggplot(dfGlobalTempAndRF, aes(x = time)) +
  geom_line(aes(y = ENSO, col = "ENSO"), linewidth  = 1, linetype = 'solid') +
  geom_line(aes(y = GHG, col = "GHG"), linewidth  = 1, linetype = 'solid') +
  geom_line(aes(y = SOL, col = "SOL"), linewidth  = 1, linetype = 'solid') +
  geom_point(aes(y = TGL), col = 'maroon', cex = 2, position = 'jitter', alpha = .4) +
  geom_line(aes(y = TGL, col = "TGL"), linewidth  = 1, linetype = 'solid') +
  geom_line(aes(y = VUL, col = "VUL"), linewidth  = 1, linetype = 'solid') +
  scale_x_continuous(breaks = seq(from = 1866, to = 2022, by = 26)) + #add more labels (breaks) to x-axis
  scale_y_continuous(sec.axis = sec_axis( #add scale on the secondary (right) axis
    trans = ~ . * 1.0,
    name = "TGL Anom. °C",
    breaks = waiver(),
    labels = waiver(),
    guide = waiver()
  )) +
  labs(x = 'Years', y = 'RF', Title = 'Radiative Forcing and Temparature') +
  scale_color_manual(values = c('black', 'orange', 'red', 'blue', 'gray'),
                     name = 'Entity', #change the name of the legend
  )  
#correlation matrix
corr <- cor(dfGlobalTempAndRF[, 2:6])
print(corr)
#plot the correlation matrix
ggcorrplot(corr)

#correlation matrix GHG, SOL, VUL, ENSO
corr2 <- cor(dfGlobalTempAndRF[, 3:6])
print(corr2 * 100)
#plot the correlation matrix
ggcorrplot(corr2)

#plot the relationship between variables
plot(dfGlobalTempAndRF[,2:6])
#the only linear relationship is TGL vs GHG all the others are not linear
#let us plot one by one
gp1 <- ggplot(dfGlobalTempAndRF, aes(x = TGL, y = GHG)) +
  geom_point(position = 'jitter', alpha = .5) +
  geom_smooth(method = 'gam', color = 'red', linetype = 'solid', se = T) + #gam = generalized additive mode smoothing
  labs(x = 'TGL (An. °C)', y = 'RF-GHG Wm^(-2)', Title = 'Gas vs Emission')

gp2 <- ggplot(dfGlobalTempAndRF, aes(x = TGL, y = SOL)) +
  geom_point(position = 'jitter', alpha = .5) +
  geom_smooth(method = 'gam', color = 'red', linetype = 'solid', se = T) + #gam = generalized additive mode smoothing
  geom_smooth(method = 'gam', formula = y ~ poly(x, 4), color = 'midnightblue', linetype = 'solid', se = T) +
  geom_smooth(method = 'gam', formula = y ~ s(x, k = 3), color = 'darkorange', linetype = 'solid', se = T) +
  labs(x = 'TGL (An. °C)', y = 'RF-SOL Wm^(-2)', Title = 'Gas vs Emission')


gp3 <- ggplot(dfGlobalTempAndRF, aes(x = TGL, y = VUL)) +
  geom_point(position = 'jitter', alpha = .5) +
  geom_smooth(method = 'gam', color = 'green', linetype = 'solid', se = T) + #gam = generalized additive mode smoothing
  labs(x = 'TGL (An. °C)', y = 'RF-VUL Wm^(-2)', Title = 'Gas vs Emission')

gp4 <- ggplot(dfGlobalTempAndRF, aes(x = TGL, y = ENSO)) +
  geom_point(position = 'jitter', alpha = .5) +
  geom_smooth(method = 'gam', color = 'blue', linetype = 'solid', se = T) + #gam = generalized additive mode smoothing
  labs(x = 'TGL (An. °C)', y = 'RF-ENSO Wm^(-2)', Title = 'Gas vs Emission')

grid.arrange(gp1, gp2, gp3, gp4, ncol = 4)

#seems not following lin reg
fit1 <- lm(ENSO  ~  TGL, data = dfGlobalTempAndRF)
summary(fit1)
AIC(fit1)

#let us try a polinomial terms. I(...) is used to interpret the * as an arithmetic operator (multiplication)
#and not as a relationship operator in the lm() formula
fit2 <- lm(ENSO  ~  TGL + I(TGL * TGL), data = dfGlobalTempAndRF)
summary(fit2)
AIC(fit2)
par(mfrow = c(2,2))
plot(fit2)
par(mfrow = c(1,1))
#cube
fit3 <- lm(ENSO  ~  TGL + I(TGL * TGL) + I(TGL * TGL * TGL), data = dfGlobalTempAndRF)
summary(fit3)
AIC(fit3)

#using nls for nonlinear data
#SSlogis: helps create initial estimates parameters
#we use the following function to initialize the parameters
getInitial(TGL ~ SSlogis(ENSO, Asym, xmid, scal), data = dfGlobalTempAndRF)
fit4_nls <- nls(TGL ~ SSlogis(ENSO, Asym, xmid, scal), data = dfGlobalTempAndRF)
summary(fit4_nls)
#exact coefficients
alpha <- coef(fit4_nls)
plot(ENSO ~ TGL, data = dfGlobalTempAndRF, main = "TGL vs ENSO",
     xlab = "ENSO Wm^(-2)", ylab = "TGL An.°C") #census data
#fit the growth equation Asym/(1+exp(xmid-x)/scal)
curve(alpha[1]/(1 + exp((alpha[2] - x)/alpha[3])), add = T, col = "green")  # Fitted model
#check if residuals are normlly distributed
qqnorm(residuals(fit4_nls))
qqline(residuals(fit4_nls)) #residuals needs to e normally distributed
#compute r^2
RSS <- sum(residuals(fit4_nls)^2) #residual sum of squares
TSS <- sum(dfGlobalTempAndRF$ENSO - mean(dfGlobalTempAndRF$ENSO)^2) #total sum of squares
r.square <- 1 - RSS/TSS
r.square

#TSI 1610-2022
dfTSIHistory = dbGetQuery(conn = con,
  "SELECT
  	  FLOOR([time]) 'time'
  	, [TSI]
  FROM [GMAST_DATA].[dbo].[TSI_records_since_1610]
  WHERE [time] < 2023")
summary(dfTSIHistory)
head(dfTSIHistory)
ggplot(dfTSIHistory, aes(x = time, y = TSI)) +
  geom_point(position = 'jitter', alpha = .3, size = 2, color = 'orange') +
  geom_line(color = 'red') +
  geom_smooth(method = 'gam', color = 'darkblue', linetype = 'dashed', se = T) + #gam = generalized additive mode smoothing
  labs(x = 'Year', y = 'TSI (W/m²)', Title = 'TSI trend 1610-2022')

##TSI 1980-2022
dfTSIH8022 = dbGetQuery(conn = con,
  "SELECT
  	  FLOOR([time]) 'time'
  	, [TSI]
  FROM [GMAST_DATA].[dbo].[TSI_records_since_1610]
  WHERE [time] BETWEEN 1980 AND 2022")
summary(dfTSIH8022)
head(dfTSIH8022)

#TSI 1980-2022 FROM NAVAL RES. LAB (NLR)
dfTSInlr8022 = dbGetQuery(conn = con,
  "SELECT
  	  [time]
  	, [TSI]
  FROM [GMAST_DATA].[dbo].[naval_res_lab_tsi_records_since_1610]
  WHERE [time] >= 1980")
summary(dfTSInlr8022)
head(dfTSInlr8022)

ggplot() +
  geom_point(data = dfTSIH8022, mapping = aes(x = time, y = TSI), position = 'jitter', alpha = .8, size = 2, color = 'midnightblue', shape = 2) +
  geom_line(data = dfTSIH8022, mapping = aes(x = time, y = TSI), color = 'red') +
  geom_point(data = dfTSInlr8022, mapping = aes(x = time, y = TSI), position = 'jitter', alpha = .8, size = 2, color = 'midnightblue', shape = 0) +
  geom_line(data = dfTSInlr8022, mapping = aes(x = time, y = TSI),color = 'blue') +
  labs(x = 'Year', y = 'TSI (W/m²)', Title = 'TSI trend 1610-2022') +
  geom_text(x=2017, y=1361.17, label="2017", color = 'midnightblue') +
  geom_smooth(data = dfTSIH8022, mapping = aes(x = time, y = TSI),method = 'gam', linetype = 'dashed', se = T) + #gam = generalized additive mode smoothing
  geom_smooth(data = dfTSInlr8022, mapping = aes(x = time, y = TSI),method = 'gam', linetype = 'dotdash', se = T) #gam = generalized additive mode smoothing

ggplot()+
  geom_line(data=dfTSIH8022, mapping = aes(x=time, y=TSI, color='TSIS/TIM'), linetype = 'solid') +
  geom_line(data=dfTSInlr8022, mapping = aes(x=time, y=TSI, color='NRL'), linetype = 'dashed') +
  scale_colour_manual(name = 'TSI Source',
    breaks=c('TSIS/TIM', 'NRL'),
    values =c('TSIS/TIM'='midnightblue','NRL'='red'),
    labels = c('TSIS/TIM','NRL')) +
  geom_point(data = dfTSIH8022, mapping = aes(x = time, y = TSI),
    position = 'jitter', alpha = .8, size = 2, color = 'midnightblue', shape = 2) +
  geom_point(data = dfTSInlr8022, mapping = aes(x = time, y = TSI),
    position = 'jitter', alpha = .8, size = 2, color = 'red', shape = 0) +
  labs(x = 'Year', y = 'TSI (W/m²)', Title = 'TSI trend 1610-2022') +
  annotate(geom="text", x=2017, y=1361.17, label="2017", color="midnightblue")

#HadCRUT5 global surface temperature anomaly (land and sea) since 1850
dfTempAnomHd5 = dbGetQuery(conn = con,
  "SELECT
  	  [Time]
  	, [AVERAGED]
  FROM [GMAST_DATA].[dbo].[HadCRUT5_Temp_Anom_yearly_since_1850]")
summary(dfTempAnomHd5)
head(dfTempAnomHd5)

ggplot(dfTempAnomHd5, aes(x = Time, y = AVERAGED)) +
  geom_point(position = 'jitter', alpha = .3, size = 2, color = 'orange') +
  geom_line(color = 'red') +
  geom_smooth(method = 'gam', color = 'darkblue', linetype = 'dashed', se = T) + #gam = generalized additive mode smoothing
  labs(x = 'Year', y = 'Temperature Anom (°C)', Title = 'Global Surface Temperature since 1850')

#fit curve of degrees 2 and 5
ggplot(dfTempAnomHd5, aes(x = Time, y = AVERAGED)) +
  geom_line(color = 'black', linewidth = 1.2) +
  geom_smooth(mapping = aes(col = 'Degree 2'),
              method = 'gam',
              formula = y ~ poly(x, 2),
              linetype = 'dotdash',
              se = F) +
  geom_smooth(mapping = aes(col = 'Degree 5'),
              method = 'gam',
              formula = y ~ poly(x, 5),
              linetype = 'solid',
              se = F) +
  scale_colour_manual(name = 'Polyn. degree',
                breaks = c("Degree 2", "Degree 5"),
                values =c('Degree 2'='grey42','Degree 5'='grey6'),
                labels = c('Degree 2','Degree 5')) +
  labs(x = 'Year', y = 'Temperature Anom (°C)',
       caption = '',
       title = 'Global Surface Temperature 1850-2023') +
  geom_text(x=1882.1, y=0.98, label='Degree 2 fit: 2.05781866*x^2+4.19149718*x-0.07313126',
            color = 'grey42',
            size = 2.3) +
  geom_text(x=1910, y=0.88, label='Degree 5 fit: 0.12537957*x^5+0.40243593*x^4+0.35936203*x^3+2.05781866*x^2+4.19149718*x-0.07313126',
            color = 'grey6',
            size = 2.3) +
  scale_x_continuous(breaks = seq(from = 1850, to = 2023, by = 10)) +
  scale_y_continuous(sec.axis = sec_axis( #add scale on the secondary (right) axis
    trans = ~ . * 1.0,
    name = waiver(),
    breaks = waiver(),
    labels = waiver(),
    guide = waiver()
  )) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

#model using a quadratic fit 
model <- lm(AVERAGED ~ poly(Time, 2), data = dfTempAnomHd5)
summary(model)
#model2 using a fifth degree fit
model2 <- lm(AVERAGED ~ poly(Time, 5), data = dfTempAnomHd5)
summary(model)
anova(model, model2)
coef(model)
coef(model2)

#New values of Time (PREDICTOR) to predict AVERAGED temp. anom. (RESPONSE)
new_vals <- c(2024, 2025, 2026)
#Predict using the fitted model - in the dataframe the column name needs to be the same as the predictor (time)
#the variable predictions contains values for ENSO
preds <- predict(model, newdata = data.frame(Time = new_vals)) #sing model
preds2 <- predict(model2, newdata = data.frame(Time = new_vals)) #using model2
#View the predicted values
print(preds)
print(preds2)

#DATA RECONSTRUCTION 1600 ONWARDS
dfTempRec = dbGetQuery(conn = con, #THE FIRST SELECT HAS ITS SCALE UNTIL 1979
"SELECT
	  [Year]
	, [T_Anomaly]
FROM
(
	SELECT
		  [Year]
		, [T_Anomaly] T_Anomaly
	FROM [GMAST_DATA].[dbo].[temperature_reconstr_2kyr_to_1979_moberg]
	WHERE [Year] > 1599
	UNION ALL
	SELECT
		  [Time] [Year]
		, [AVERAGED] T_Anomaly
	FROM [dbo].[HadCRUT5_Temp_Anom_yearly_since_1850]
	WHERE [Time] > 1979) D")
summary(dfTempRec)
head(dfTempRec)

#composite temperature averaged
dfcompTempRec = dbGetQuery(conn = con,
  "SELECT
  	  [Year_CE]
  	, [GMST_averaged]
  FROM [GMAST_DATA].[dbo].[composite_avg_temperature_recons_past_2kyr]
  WHERE [Year_CE] > 1599")
summary(dfcompTempRec)
head(dfcompTempRec)

#composite temperature averaged
dfcompMedFullEnsamble = dbGetQuery(conn = con,
  "SELECT
	  [Year_CE]
	, [GMST_averaged]
	, [GMST_median_data_from_system]
  FROM [GMAST_DATA].[dbo].[composite_avg_temperature_recons_past_2kyr]
  WHERE [Year_CE] > 1599")
summary(dfcompMedFullEnsamble)
head(dfcompMedFullEnsamble)

ggplot() +
  geom_point(data = dfTempRec, mapping = aes(x = Year, y = T_Anomaly), position = 'jitter', alpha = .3, size = 1, color = 'red') +
  geom_line(data = dfTempRec, mapping = aes(x = Year, y = T_Anomaly), color = 'red') +
  # geom_smooth(data = dfTempRec, mapping = aes(x = Year, y = T_Anomaly), method = 'gam', color = 'darkblue', linetype = 'dashed', se = T) +
  geom_point(data = dfcompTempRec, mapping = aes(x = Year_CE, y = GMST_averaged), position = 'jitter', alpha = .3, size = 1, color = 'gray26') +
  geom_line(data = dfcompTempRec, mapping = aes(x = Year_CE, y = GMST_averaged), color = 'gray26') +
  # geom_smooth(data = dfcompTempRec, mapping = aes(x = Year_CE, y = GMST_averaged), method = 'gam', color = 'cyan', linetype = 'dashed', se = T) +
  geom_point(data = dfcompMedFullEnsamble, mapping = aes(x = Year_CE, y = GMST_median_data_from_system), position = 'jitter', alpha = .3, size = 1, color = 'gray67') +
  geom_line(data = dfcompMedFullEnsamble, mapping = aes(x = Year_CE, y = GMST_median_data_from_system), color = 'gray67') +
  # geom_smooth(data = dfcompMedFullEnsamble, mapping = aes(x = Year_CE, y = GMST_median_data_from_system), method = 'gam', color = 'cyan', linetype = 'dashed', se = T) +
  scale_x_continuous(breaks = seq(from = 1600, to = 2000, by = 50)) +
  scale_y_continuous(breaks = seq(from = -0.6, to = 0.8, by = 0.4)) +
  labs(x = 'Year', y = 'Temperature Anom (°C)', Title = 'Global Surface Temperature since 1600')

#plot simplified
ggplot() +
  geom_line(data = dfTempRec, mapping = aes(x = Year, y = T_Anomaly, col = 'HadCRUT5')) +
  geom_line(data = dfcompMedFullEnsamble, mapping = aes(x = Year_CE, y = GMST_median_data_from_system, col = 'Comp Median')) +
  scale_x_continuous(breaks = seq(from = 1600, to = 2000, by = 50)) +
  scale_y_continuous(breaks = seq(from = -0.6, to = 0.8, by = 0.4)) +
  scale_colour_manual(name = 'Scale Type',
                      breaks=c('HadCRUT5', 'Comp Median'),
                      values =c('HadCRUT5'='midnightblue', 'Comp Median' = 'gray26'),
                      labels = c('HadCRUT5', 'Comp Median')) +
  labs(x = 'Year', y = 'Temperature Anom (°C)', Title = 'Global Surface Temperature since 1600', caption = 'Global Surface Temperature since 1600')

#data from 1000 onwards - hockey stick
dfTempRecHS = dbGetQuery(conn = con, #THE FIRST SELECT HAS ITS SCALE UNTIL 1979
    "SELECT
    	  [Year]
    	, [T_Anomaly]
    FROM
    (
    	SELECT
    		  [Year]
    		, [T_Anomaly] T_Anomaly
    	FROM [GMAST_DATA].[dbo].[temperature_reconstr_2kyr_to_1979_moberg]
    	WHERE [Year] > 999
    	UNION ALL
    	SELECT
    		  [Time] [Year]
    		, [AVERAGED] T_Anomaly
    	FROM [dbo].[HadCRUT5_Temp_Anom_yearly_since_1850]
    	WHERE [Time] > 1979) D")
summary(dfTempRec)
head(dfTempRec)

#plot hockey stick
ggplot(data = dfTempRecHS, mapping = aes(x = Year, y = T_Anomaly)) +
  geom_line(color = 'black', linewidth = 1) +
  scale_x_continuous(breaks = seq(from = 1000, to = 2000, by = 100)) +
  labs(x = 'Year', y = 'Temperature Anom (°C)') +
  ggtitle("Hockey Stick - Temperature since 1000 AD")

#Temperature anomaly
dfTempAn60yr = dbGetQuery(conn = con,
  "SELECT
      [Time]
    , [AVERAGED]
  FROM [GMAST_DATA].[dbo].[HadCRUT5_Temp_Anom_yearly_since_1850]")
summary(dfTempAn60yr)
head(dfTempAn60yr)

#model using nls
getInitial(AVERAGED ~ SSlogis(Time, Asym, xmid, scal), data = dfTempAn60yr)
fit5_nls <- nls(AVERAGED ~ SSlogis(Time, Asym, xmid, scal), data = dfTempAn60yr)
summary(fit5_nls)
qqnorm(residuals(fit5_nls))
qqline(residuals(fit5_nls)) #residuals needs to e normally distributed
coef(fit5_nls)
coef(summary(fit5_nls))
pred_years <- data.frame(Time = seq(from = 1850, to = 2100, by = 5))
#Predict using the fitted model - in the dataframe the column name needs to be the same as the predictor (time)
#the variable predictions contains values for Temp An.
preds <- predict(fit5_nls, newdata = pred_years)
#View the predicted attribute
head(attr(preds, "gradient"))
#get only values from predictions
vals <- numeric()
for(i in c(1:length(preds))) vals[i] <- preds[i]
cor(dfTempAn60yr$AVERAGED, predict(fit5_nls))
newdata_nls <- data.frame(Time = pred_years, AVERAGED = vals)

#model using lm^2
fit6_lm2 <- lm(AVERAGED ~ poly(Time,2), data = dfTempAn60yr)
summary(fit6_lm2)
qqnorm(residuals(fit6_lm2))
qqline(residuals(fit6_lm2)) #residuals needs to e normally distributed
coef(fit6_lm2)
fit6_lm2$fitted.values
cor(dfTempAn60yr$AVERAGED, predict(fit6_lm2))
preds_lm2 <- predict(fit6_lm2, newdata = pred_years)
newdata_lm2 <- data.frame(Time = pred_years, AVERAGED = preds_lm2)
newdata_lm2

#60 year cycle averaged by data group of 60 years
dfTempAnInChuncks = dbGetQuery(conn = con,
  "SELECT
	  [Time]
	, [T_Anomaly]
  FROM
  (
  	SELECT TOP 100 PERCENT
  		  [Time]
  		, [T_Anomaly]
  		, (ROW_NUMBER() OVER (ORDER BY [Time])) ROWNO
  	FROM
  	(
  	SELECT
    		  [Year] 'Time'
    		, [T_Anomaly]
  	FROM [GMAST_DATA].[dbo].[temperature_reconstr_2kyr_to_1979_moberg]
  	UNION
  	SELECT
    			[Time]
    		, ROUND([AVERAGED],4) [T_Anomaly]
  	FROM [dbo].[HadCRUT5_Temp_Anom_yearly_since_1850]
  	WHERE [Time] > 1979
  	) T
  ) T1
  WHERE (T1.ROWNO % 20 = 0)")
summary(dfTempAnInChuncks)
head(dfTempAnInChuncks)
fit6_chunks <- lm(T_Anomaly ~ poly(Time,5), data = dfTempAnInChuncks)
summary(fit6_chunks)
qqnorm(residuals(fit6_chunks))
qqline(residuals(fit6_chunks)) #residuals needs to e normally distributed
coef(fit6_chunks)
cor(dfTempAnInChuncks$T_Anomaly, predict(fit6_chunks))
pred_avgYr <- data.frame(Time = seq(from = 0, to = 2100))
preds_chunks <- predict(fit6_chunks, newdata = pred_avgYr)
newdata_60yr <- data.frame(Time = pred_avgYr, T_Anomaly = preds_chunks)
newdata_60yr

#plot line
ggplot() +
  geom_line(data = filter(dfTempAnInChuncks, Time > 1850), mapping = aes(x = Time, y = T_Anomaly), color = 'black', linewidth = 1) +
  geom_line(data = filter(newdata_60yr, Time > 1850), mapping = aes(x = Time, y = T_Anomaly), color = 'blue', linewidth = 1) +
  scale_x_continuous(breaks = seq(from = 0, to = 2100, by = 100)) +
  labs(x = 'Year', y = 'Temperature Anom (°C)') +
  ggtitle("Temperature Anom. during a 60 year cycle")

#####
##### ATLANTIC AND PACIFIC OSCILLATIONS
#####
#AMO - atlantic multidecadal oscillation
dfTempAMOYrAvg = dbGetQuery(conn = con,
  "SELECT
    [time]
  , ROUND([YearlyAvg],3) [YearlyAvg]
  FROM [GMAST_DATA].[dbo].[atlantic_multidec_oscil_AMO_data_since_1870]
  WHERE [time] < 2021")
summary(dfTempAMOYrAvg)
head(dfTempAMOYrAvg)

#AMO - YEARLY FRACTION
dfTempAMOYrFrac = dbGetQuery(conn = con,
  "SELECT
	  [year_fraction] [time]
    , [T_value]
  FROM [GMAST_DATA].[dbo].[AMO_date_value_year_fraction_since_1870]")
summary(dfTempAMOYrFrac)
head(dfTempAMOYrFrac)

#plot line 
amop <- ggplot() +
  geom_line(data = dfTempAMOYrAvg, mapping = aes(x = time, y = YearlyAvg), color = 'black', linewidth = 1) +
  geom_area(data = dfTempAMOYrAvg, mapping = aes(x = time, y = YearlyAvg), color = 'red', alpha = .6) +
  geom_line(data = dfTempAMOYrFrac, mapping = aes(x = time, y = T_value), color = 'grey', linewidth = 1, linetype = 'solid', alpha = .6) +
  geom_line(aes(x = c(1870:2020), y = 0)) +
  scale_x_continuous(breaks = seq(from = 1870, to = 2020, by = 20)) +
  labs(x = 'Year', y = 'AMO Temperature Anom (°C)') +
  ggtitle("AMO Montly and averaged SST values 1870-2020")

#PDO - pacific decadal oscillation
dfTempPDOYrAvg = dbGetQuery(conn = con,
  "SELECT
      [time]
  	, [YearlyAvg]
  FROM [GMAST_DATA].[dbo].[pacific_decadal_oscillation_PDO_data_since_1854]
  WHERE [time] < 2021")
summary(dfTempPDOYrAvg)
head(dfTempPDOYrAvg)

#AMO - YEARLY FRACTION
dfTempPDOYrFrac = dbGetQuery(conn = con,
  "SELECT
	    [year_frac] 'time'
	  , [T_value]
  FROM [GMAST_DATA].[dbo].[PDO_date_value_year_fraction_since_1854]
  WHERE [year_frac] < 2021")
summary(dfTempPDOYrFrac)
head(dfTempPDOYrFrac)

#plot line 
pdop <- ggplot() +
  geom_line(data = dfTempPDOYrAvg, mapping = aes(x = time, y = YearlyAvg), color = 'black', linewidth = 1) +
  geom_area(data = dfTempPDOYrAvg, mapping = aes(x = time, y = YearlyAvg), color = 'red', alpha = .6) +
  geom_line(data = dfTempPDOYrFrac, mapping = aes(x = time, y = T_value), color = 'grey', linewidth = 1, linetype = 'solid', alpha = .6) +
  geom_line(aes(x = c(1854:2020), y = 0)) +
  scale_x_continuous(breaks = seq(from = 1854, to = 2020, by = 20)) +
  labs(x = 'Year', y = 'PDO Temperature Anom (°C)') +
  ggtitle("PDO Montly and averaged SST values 1854-2020")

grid.arrange(amop, pdop, ncol = 2)

#GHGs assessment data
dfGHGs = dbGetQuery(conn = con,
  "SELECT
      FLOOR([time]) 'time'
    , [GtCO2]
    , [F-gas-GtCO2eq]
    , [N2O-GtCO2eq] 'N2O'
    , [CH4-GtCO2eq] 'CH4'
  FROM [GMAST_DATA].[dbo].[IPCC_GHG_Tot_Emis_CO2eq]")
summary(dfGHGs)
head(dfGHGs)
str(dfGHGs)
#GHGs assessment chart
ggplot(data = dfGHGs) +
  geom_line(mapping = aes(x = time, y = GtCO2, col = 'CO2'), linewidth = 1, linetype = 'solid') +
  geom_line(mapping = aes(x = time, y = N2O, col = 'N2O'), linewidth = 1, linetype = 'dashed') +
  geom_line(mapping = aes(x = time, y = CH4, col = 'CH4'), linewidth = 1, linetype = 'dotdash') +
  scale_x_continuous(breaks = seq(from = 1750, to = 2023, by = 20)) +
  scale_colour_manual(name = 'GHG Type',
                      breaks=c('CO2', 'N2O', 'CH4'),
                      values =c('CO2'='black', 'N2O' = 'grey26', 'CH4' = 'grey'),
                      labels = c('CO2', 'N2O', 'CH4')) +
  labs(x = 'Year', y = 'GTons') +
  ggtitle("GHGs assessment chart")

#CO2 details
pCO2 <- ggplot(data = dfGHGs) +
  geom_line(mapping = aes(x = time, y = GtCO2), color = 'black', linewidth = 1, alpha = .5) +
  scale_x_continuous(breaks = seq(from = 1750, to = 2023, by = 50)) +
  labs(x = 'Year', y = 'GTons') +
  ggtitle("CO2 values 1750-2022")

#CH4 details
pCH4 <- ggplot(data = dfGHGs) +
  geom_line(mapping = aes(x = time, y = CH4), color = 'black', linewidth = 1, alpha = .5) +
  scale_x_continuous(breaks = seq(from = 1750, to = 2023, by = 50)) +
  labs(x = 'Year', y = 'GTons - CO2eq') +
  ggtitle("CH4 values 1750-2022")

#N2O details
pN2O <- ggplot(data = dfGHGs) +
  geom_line(mapping = aes(x = time, y = N2O), color = 'black', linewidth = 1, alpha = .5) +
  scale_x_continuous(breaks = seq(from = 1750, to = 2023, by = 50)) +
  labs(x = 'Year', y = 'GTons - CO2eq') +
  ggtitle("N2O values 1750-2022")

#radiative forcing (W/m2) assessment data
dfGHGsRF = dbGetQuery(conn = con,
  "SELECT 
	  [time]
	, [CO2]
	, [CH4]
	, [N2O]
FROM [GMAST_DATA].[dbo].[IPCC_effective_RF]")
summary(dfGHGsRF)
head(dfGHGsRF)
str(dfGHGsRF)

#RF due to CO2 details
pCO2RF <- ggplot(data = dfGHGsRF) +
  geom_line(mapping = aes(x = time, y = CO2), color = 'black', linewidth = 1, alpha = .5) +
  scale_x_continuous(breaks = seq(from = 1750, to = 2023, by = 50)) +
  labs(x = 'Year', y = 'W/m2') +
  ggtitle("Radiative forcing due to CO2")

#RF due to CH4 details
pCH4RF <- ggplot(data = dfGHGsRF) +
  geom_line(mapping = aes(x = time, y = CH4), color = 'black', linewidth = 1, alpha = .5) +
  scale_x_continuous(breaks = seq(from = 1750, to = 2023, by = 50)) +
  labs(x = 'Year', y = 'W/m2') +
  ggtitle("Radiative forcing due to CH4")

#RF due to CH4 details
pN2ORF <- ggplot(data = dfGHGsRF) +
  geom_line(mapping = aes(x = time, y = N2O), color = 'black', linewidth = 1, alpha = .5) +
  scale_x_continuous(breaks = seq(from = 1750, to = 2023, by = 50)) +
  labs(x = 'Year', y = 'W/m2') +
  ggtitle("Radiative forcing due to N2O")

grid.arrange(pCO2, pCH4, pN2O,
             pCO2RF, pCH4RF, pN2ORF,
             nrow = 2, ncol = 3)

#sea surface temperature
dfSST = dbGetQuery(conn = con,
  "SELECT
  	  [dec_year]
  	, [SST_anomaly_monthly]
  FROM [GMAST_DATA].[dbo].[sea_surface_temperature_year_frac_since_1850]")
summary(dfSST)
head(dfSST)
str(dfSST)

ggplot(data = dfSST) +
  geom_line(mapping = aes(x = dec_year, y = SST_anomaly_monthly), color = 'black', linewidth = .6, alpha = .5) +
  scale_x_continuous(breaks = seq(from = 1850, to = 2023, by = 50)) +
  labs(x = 'Year', y = 'Temp. Anom. °C') +
  ggtitle("Sea surface temperature anomaly since 1850")

#CO2 concentration ppm paleodata
dfCO2ppm = dbGetQuery(conn = con,
  "SELECT
      [Year]
    , [CO2ppm]
  FROM [GMAST_DATA].[dbo].[CO2_concentration_ppm_paleodata]")
summary(dfCO2ppm)
head(dfCO2ppm)
str(dfCO2ppm)

pCO2ppm <- ggplot(data = dfCO2ppm) +
  geom_line(mapping = aes(x = Year, y = CO2ppm), color = 'black', linewidth = .6, alpha = .5) +
  scale_x_continuous(breaks = c(-803720,-400000,2020)) +
  labs(x = 'Year', y = 'CO2 ppm') +
  ggtitle("CO2 concentration ppm - paleodata")

#CH4 and N2O concentration ppb since 1750
dfCH4N2Oppm = dbGetQuery(conn = con,
  "SELECT
  	  [Year]
  	, [CH4ppb]
  	, [N2Oppb]
  FROM [GMAST_DATA].[dbo].[CH4_N2O_concentrations_ppb_since_1750]")
summary(dfCH4N2Oppm)
head(dfCH4N2Oppm)
str(dfCH4N2Oppm)

pCH4N2Oppb <- ggplot(data = dfCH4N2Oppm) +
  geom_line(mapping = aes(x = Year, y = CH4ppb, col = 'CH4'), linewidth = .8, alpha = .5) +
  geom_line(mapping = aes(x = Year, y = N2Oppb, col = 'N2O'), linewidth = .8, alpha = .5) +
  scale_x_continuous(breaks = seq(from = 1750, to = 2023, by = 50)) +
  scale_colour_manual(name = 'GHG Type',
                      breaks=c('CH4', 'N2O'),
                      values =c('CH4'='black', 'N2O' = 'grey26'),
                      labels = c('CH4', 'N2O')) +  labs(x = 'Year', y = 'ppb') +
  ggtitle("CH4 and N2O concentration ppm - paleodata")

grid.arrange(pCO2ppm, pCH4N2Oppb, ncol=2)
