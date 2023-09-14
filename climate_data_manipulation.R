setwd("C:/.../.../climateResearch")
getwd()

library(DBI)
library(reshape2)

#connect to sql server 2022
con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "SQL Server", 
                      Server = ".\\...", 
                      Database = "GMAST_DATA", 
                      Trusted_Connection = "True")
#retreive data
dfDbData = dbGetQuery(conn = con,
  "SELECT [time]
      , CONVERT(VARCHAR(20), ([time] + 0.0/12)) + ',' + CONVERT(VARCHAR(10), [Jan])  'd_0'
      , CONVERT(VARCHAR(20), ([time] + 1.0/12)) + ',' + CONVERT(VARCHAR(10), [Feb])  'd_1'
      , CONVERT(VARCHAR(20), ([time] + 2.0/12)) + ',' + CONVERT(VARCHAR(10), [Mar])  'd_2'
      , CONVERT(VARCHAR(20), ([time] + 3.0/12)) + ',' + CONVERT(VARCHAR(10), [Apr])  'd_3'
      , CONVERT(VARCHAR(20), ([time] + 4.0/12)) + ',' + CONVERT(VARCHAR(10), [May])  'd_4'
      , CONVERT(VARCHAR(20), ([time] + 5.0/12)) + ',' + CONVERT(VARCHAR(10), [Jun])  'd_5'
      , CONVERT(VARCHAR(20), ([time] + 6.0/12)) + ',' + CONVERT(VARCHAR(10), [Jul])  'd_6'
      , CONVERT(VARCHAR(20), ([time] + 7.0/12)) + ',' + CONVERT(VARCHAR(10), [Aug])  'd_7'
      , CONVERT(VARCHAR(20), ([time] + 8.0/12)) + ',' + CONVERT(VARCHAR(10), [Sep])  'd_8'
      , CONVERT(VARCHAR(20), ([time] + 9.0/12)) + ',' + CONVERT(VARCHAR(10), [Oct])  'd_9'
      , CONVERT(VARCHAR(20), ([time] + 10.0/12)) + ',' + CONVERT(VARCHAR(10), [Nov]) 'd_10' 
      , CONVERT(VARCHAR(20), ([time] + 11.0/12)) + ',' + CONVERT(VARCHAR(10), [Dec]) 'd_11' 
  FROM [GMAST_DATA].[dbo].[atlantic_multidec_oscil_AMO_data_since_1870]
  WHERE [time] < 2021")
summary(dfDbData)
head(dfDbData)

#reshape data
