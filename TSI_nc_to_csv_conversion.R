#nc to csv conversion for TSI data
#data source https://lasp.colorado.edu/lisird/data/nrl2_tsi_P1Y

setwd("C:/.../.../climateResearch")
getwd()

library(ggplot2) # package for plotting
library(ncdf4) # package for netcdf manipulation
library(lubridate)

#### FUNCTIONS

#compute Julian day by date
computeDateToJD <- function(strDate) {
  date <- as.Date(strDate)
  dtY <- year(date)
  dtM <- month(date)
  dtD <- day(date)
  
  if (dtM > 2) {
    y <- dtY
    m <- dtM
  }
  else {
    y <- dtY-1
    m <- dtM+12
  }
  
  dtGreg <- as.Date('1582-10-15')
  if (date >= dtGreg) {
    a <- floor(y/100)
    b <- 2-a+floor(a/4)
  }
  else {
    a <- b <- 0
  }
  
  jd <- floor(365.25*(y+4716))+floor(30.6*(m+1))+dtD+b-1524.5
  
  return (jd) 
}

#compute date by Julian day
computeJDtoDate <- function(jd) {
  jd <- jd+.5
  z <- floor(jd) #integer part
  f <- jd %% 1 #decimal part
  if (z < 2299161) {
    a <- z
  }
  else {
    alpha <- floor((z-1867216.25)/36524.24)
    a <- z+1+alpha-floor(alpha/4)
  }
  
  b <- a+1524
  c <- floor((b-122.1)/365.25)
  d <- floor(365.25*c)
  e <- floor((b-d)/30.6001)
  dtD <- b-d-floor(30.6001*e)+f
  dtM <- if(e < 13.5) e-1 else e-13
  dtY <- if(dtM < 2.5) c-4715 else c-4716
  date <- make_date(year = dtY, month = dtM, day = dtD)
  return (date)
}

#compute date year by Julian day
computeJDtoDateYear <- function(jd) {
  jd <- jd+.5
  z <- floor(jd) #integer part
  f <- jd %% 1 #decimal part
  if (z < 2299161) {
    a <- z
  }
  else {
    alpha <- floor((z-1867216.25)/36524.24)
    a <- z+1+alpha-floor(alpha/4)
  }
  
  b <- a+1524
  c <- floor((b-122.1)/365.25)
  d <- floor(365.25*c)
  e <- floor((b-d)/30.6001)
  dtM <- if(e < 13.5) e-1 else e-13
  dtY <- if(dtM < 2.5) c-4715 else c-4716
  return (dtY)
}

#### END FUNCTIONS

#### TESTS
#jd <- computeDateToJD('1582-10-15')
#print(jd)
#dt <- computeJDtoDate(jd)
#as.character(dt)
####

nc_data <- nc_open('tsi_v02r01_yearly_s1610_e2022_c20230120.nc')
sink('nc_sample_data_metadata.txt')
print(nc_data)
sink()

time <- ncvar_get(nc_data, "time", verbose = F)
TSI <- ncvar_get(nc_data, "TSI")
TSI_uncertainty <- ncvar_get(nc_data, "TSI_UNC")
dfTSI <- data.frame(time = time, TSI = TSI, TSI_uncertainty = TSI_uncertainty)
#Julian day for 1610-01-01 
JD1610 <-computeDateToJD('1610-01-01')
dfTSI$time <- JD1610 + dfTSI$time
dfTSI$year <- sapply(dfTSI$time, FUN = computeJDtoDateYear)

dfTSI2Exp = data.frame(time = dfTSI$year, TSI = dfTSI$TSI)

#export data frame to csv file
write.csv(dfTSI2Exp, "TSI_since1610.csv", row.names=T)
