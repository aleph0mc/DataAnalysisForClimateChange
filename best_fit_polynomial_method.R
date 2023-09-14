setwd("C:/.../.../climateResearch")
getwd()

library(ggplot2)
library(DBI)
library(nls2) #nonlinear regression
library(nlshelper)

#set white background for ggplots
theme_set(theme_minimal())

#connect to sql server
con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "SQL Server", 
                      Server = ".\\...", 
                      Database = "GMAST_DATA", 
                      Trusted_Connection = "True")

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

#CHECK THE BEST POLYNOMIAL FOR TGL VS ENSO
#Set a seed value for reproducible results
set.seed(70)
# Split the data FOR TRAIN 75% AND TEST 25%
ind <- sample(x = nrow(dfGlobalTempAndRF), size = floor(0.75 * nrow(dfGlobalTempAndRF)))
# Store the value in train and test dataframes
train <- dfGlobalTempAndRF[ind,]
test <- dfGlobalTempAndRF[-ind,]
#CREATE POLYNOMIALS OF DIFFERENT DEGREES
# Order 1
poly_reg1 <- lm(formula = ENSO ~ poly(TGL,1), data = train)
# Order 3
poly_reg3 <- lm(formula = ENSO ~ poly(TGL,3), data = train)
# Order 5
poly_reg5 <- lm(formula = ENSO ~ poly(TGL,5), data = train)
# Order 9
poly_reg9 <- lm(formula = ENSO ~ poly(TGL,9), data = train)
#plot the polynomials
ggplot(train) +
  geom_point(aes(TGL, ENSO, col = "Original"), cex = 2) +
  stat_smooth(method = "lm", formula = y~poly(x,1), aes(TGL, poly_reg1$fitted.values, col = "Order 1")) +
  stat_smooth(method = "lm", formula = y~poly(x,3), aes(TGL, poly_reg3$fitted.values, col = "Order 3")) +
  stat_smooth(method = "lm", formula = y~poly(x,5), aes(TGL, poly_reg5$fitted.values, col = "Order 5")) +
  stat_smooth(method = "lm", formula = y~poly(x,9), aes(TGL, poly_reg9$fitted.values, col = "Order 9")) +
  scale_color_manual(values = rev(heat.colors(5)),
                     name = 'Degree', #change the name of the legend
  )

#Measuring the RSS Value on Train and Test Data
# Predicting values using test data by each model
poly1_pred <- predict(object = poly_reg1, newdata =  data.frame(TGL = test$TGL))
poly3_pred <- predict(object = poly_reg3, newdata =  data.frame(TGL = test$TGL))
poly5_pred <- predict(object = poly_reg5, newdata =  data.frame(TGL = test$TGL))
poly9_pred <- predict(object = poly_reg9, newdata =  data.frame(TGL = test$TGL))

# RSS for train data based on each model
train_rss1 <- mean((train$ENSO - poly_reg1$fitted.values)^2)  # Order 1
train_rss3 <- mean((train$ENSO - poly_reg3$fitted.values)^2)  # Order 3
train_rss5 <- mean((train$ENSO - poly_reg5$fitted.values)^2)  # Order 5
train_rss9 <- mean((train$ENSO - poly_reg9$fitted.values)^2)  # Order 9

# RSS for test data based on each model
test_rss1 <- mean((test$ENSO - poly1_pred)^2)  # Order 1
test_rss3 <- mean((test$ENSO - poly3_pred)^2)  # Order 3
test_rss5 <- mean((test$ENSO - poly5_pred)^2)  # Order 5
test_rss9 <- mean((test$ENSO - poly9_pred)^2)  # Order 9

# Visualizing train and test RSS for each model
# excluding degree 1
train_rss <- scale(c(train_rss3, train_rss5, train_rss9)) # scaling 
test_rss <- scale(c(test_rss3, test_rss5, test_rss9))      # scaling
orders <- c(3, 5, 9)
ggplot() +
  geom_line(aes(orders, train_rss, col = "Train RSS")) +
  geom_line(aes(orders, test_rss, col = "Test RSS")) +
  scale_colour_manual("", breaks = c("Train RSS", "Test RSS"), values = c("green", "red")) +
  ylab("RSS")
#from he chart best fit is at order 5, when the 2 lines for train and test change direction
alpha <- coef(poly_reg5)
print(alpha)

#we can also define our polynomial
#dfGlobalTempAndRF[,c(2,6)]
poly5 <- function(x, a, b, c, d, e, f) a + b*x + c*x^2 + d*x^3 + e*x^4 + f*x^5

#RESPONSE ~ PREDICTOR 
nls_rf <- nls(ENSO ~ poly5(TGL, a, b, c, d, e, f),
              data=dfGlobalTempAndRF,
              start=list(a=alpha[1], b=alpha[2], c=alpha[3], d=alpha[4], e=alpha[5], f=alpha[6]))
summary(nls_rf)
plot_nls(nls_rf, ylim=c(-2,2.3), xlim=c(-.3,1.3))

#New values of TGL (PREDICTOR) to predict ENSO (RESPONSE)
new_TGL <- c(1.6,1.65,1.67)
#Predict using the fitted model - in the dataframe the column name needs to be the same as the predictor (TGL)
#the variable predictions contains values for ENSO
predictions <- predict(nls_rf, newdata = data.frame(TGL = new_TGL))
#View the predicted values
print(predictions)
