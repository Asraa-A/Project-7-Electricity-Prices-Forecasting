#Random Forest

#Month-ahead forecasting
######
# Splitting data to training data and testing data
#It would be easier to use window function and 
#for loop but for the analysis purposes I have done it manually

Train_month<- data.frame(prices %>% filter(Datetime < as.Date("2019-01-01") & Datetime > as.Date("2017-01-01")))
Test_month <- data.frame(prices %>% filter(Datetime < as.Date("2019-02-01") & Datetime >= as.Date("2019-01-01")))

# Train_month<- data.frame(prices %>% filter(Datetime < as.Date("2019-02-01") & Datetime > as.Date("2017-01-01")))
# Test_month <- data.frame(prices %>% filter(Datetime < as.Date("2019-03-01") & Datetime >= as.Date("2019-02-01")))
# 
# Train_month<- data.frame(prices %>% filter(Datetime < as.Date("2019-03-01") & Datetime > as.Date("2017-01-01")))
# Test_month <- data.frame(prices %>% filter(Datetime < as.Date("2019-04-01") & Datetime >= as.Date("2019-03-01")))
# 
# Train_month<- data.frame(prices %>% filter(Datetime < as.Date("2019-04-01") & Datetime > as.Date("2017-01-01")))
# Test_month <- data.frame(prices %>% filter(Datetime < as.Date("2019-05-01") & Datetime >= as.Date("2019-04-01")))
# 
# Train_month<- data.frame(prices %>% filter(Datetime < as.Date("2019-05-01") & Datetime > as.Date("2017-01-01")))
# Test_month <- data.frame(prices %>% filter(Datetime < as.Date("2019-06-01") & Datetime >= as.Date("2019-05-01")))
# 
# Train_month<- data.frame(prices %>% filter(Datetime < as.Date("2019-06-01") & Datetime > as.Date("2017-01-01")))
# Test_month <- data.frame(prices %>% filter(Datetime < as.Date("2019-07-01") & Datetime >= as.Date("2019-06-01")))
# 
# Train_month<- data.frame(prices %>% filter(Datetime < as.Date("2019-07-01") & Datetime > as.Date("2017-01-01")))
# Test_month <- data.frame(prices %>% filter(Datetime < as.Date("2019-08-01") & Datetime >= as.Date("2019-07-01")))
# 
# Train_month<- data.frame(prices %>% filter(Datetime < as.Date("2019-08-01") & Datetime > as.Date("2017-01-01")))
# Test_month <- data.frame(prices %>% filter(Datetime < as.Date("2019-09-01") & Datetime >= as.Date("2019-08-01")))
# 
# Train_month<- data.frame(prices %>% filter(Datetime < as.Date("2019-09-01") & Datetime > as.Date("2017-01-01")))
# Test_month <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-01") & Datetime >= as.Date("2019-09-01")))
# 
# Train_month<- data.frame(prices %>% filter(Datetime < as.Date("2019-10-01") & Datetime > as.Date("2017-01-01")))
# Test_month <- data.frame(prices %>% filter(Datetime < as.Date("2019-11-01") & Datetime >= as.Date("2019-10-01")))
# 
# Train_month<- data.frame(prices %>% filter(Datetime < as.Date("2019-11-01") & Datetime > as.Date("2017-01-01")))
# Test_month <- data.frame(prices %>% filter(Datetime < as.Date("2019-12-01") & Datetime >= as.Date("2019-11-01")))
# 
# Train_month<- data.frame(prices %>% filter(Datetime < as.Date("2019-12-01") & Datetime > as.Date("2017-01-01")))
# Test_month <- data.frame(prices %>% filter(Datetime < as.Date("2020-01-01") & Datetime >= as.Date("2019-12-01")))

#*****************************************************
rf_dm <- NULL
rf_result1<- NULL
smap1 <- NULL
mase1<-NULL
#*****************************************************
#STL decomposition
# convert data to ts object
Train_ts <- ts(Train_month$Price, freq = 24*7)
decomp_ts <- stl(na.remove(Train_ts), s.window = "periodic", robust = TRUE)$time.series


#Plot time series decompsition
Train_ts %>% na.remove() %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot()

#Convert data to multiple seasonal object
Train_msts <- msts(Train_month$Price, seasonal.periods = c(24, 24*7))
#Plot msts object
Train_msts %>% mstl() %>% autoplot() 

#***************************************************************************
# #find the best K values to model daily and weekly seasonality with Fourier terms
# aic_vals_temps <- NULL
# aic_vals <- NULL
# for ( i in 1:9)
# {
#   for(j in 1:9) 
#   {
#     
#     #xreg1 <- fourier(msts_Train,K=i,h=24)
#     #xreg2 <- fourier(msts_Train,K=j,h=168)
#     #xreg3 <- fourier(msts_Train,K=m,h=8766)
#     #xtrain <- cbind(xreg1,xreg2,xreg3)
#     xtrain= fourier(Train_msts,K=c(i,j))
#     fitma1 <- auto.arima(Train_msts,xreg=xtrain,D=0,max.p = 0,max.q=0)
#     aic_vals_temps <- cbind(i,j,fitma1$aic)
#     aic_vals <- rbind(aic_vals,aic_vals_temps)
#     
#     
#   }
# }
# colnames(aic_vals)<- c("FourierTerm24","FourierTerm168","AICValue")
# aic_vals <- data.frame(aic_vals)
# minAICVal <- min(aic_vals$AICValue)
# minvals<- aic_vals[which(aic_vals$AICValue==minAICVal),]
#***********************************************************************
#number of observations in tesing data
n_Test <- nrow(Test_month)
#number of observations in training data
n_Train <- nrow(Train_month)
period <- 24


#Trend part
trend_part <- ts(decomp_ts[,2])

#fit ARIMA to trend
trend_fit <- auto.arima(trend_part) 
# trend forecast
trend_for <- as.vector(forecast(trend_fit, n_Test)$mean) 

# Fourier features to model (Daily and Weekly)
fourier_part <- fourier(Train_msts, K = c(5,1)) 

window <- (n_Train / period) - 1


# detrended original time series
new_Price <- rowSums(decomp_ts[, c(1,3)]) 
# lag part to model
lag_part <- decomp_ts[1:(period*window), 1] 

#training data-> lag prices+ Fourier part + seasonal and remainder components
matrix_train <- data.table(Price = tail(new_Price,window*period),
                           fourier_part[seq_along((period+1):n_Train),],
                           Lag = lag_part)

#*************************************************************************
# apply RF model to the training data

rf_model <- randomForest(Price ~ ., data = data.frame(matrix_train),
                         ntree = 200, mtry = 4, nodesize = 5, importance = TRUE)

#Figure 3-6: Random Forest model
varImpPlot(rf_model, main = "Variable importance")

# RF model prediction
pred_rf <- predict(rf_model, data.frame(matrix_test)) + mean(trend_for)
#forecasting error to use it for DM test
rf_dm <- rbind(rf_dm, as.vector(Test_month$Price-pred_rf))

# calculate accuracy measures.
rf_accuracy1<- forecast::accuracy(as.vector(pred_rf),na.remove(Test_month$Price))
rf_smap1<- smape(Test_month$Price, as.vector(pred_rf)) 
rf_mase1 <- mase(Test_month$Price, as.vector(pred_rf))

# bind the result of each month  
mase1<- rbind(rf_mase1,mase1)
smap1<- rbind(rf_smap1,smap1)
rf_result1<- rbind(rf_accuracy1,rf_result1)
#****************************************************************************
# matrix with all forecasting accuracy measures as columns and
# forecasting different months as rows.
rf_result1 <- cbind(rf_result1,smap1,mase1)
#*******************************************************************************
# plot the prediction results with the real values of prices
pred_rf <- data.table(value = pred_rf, Var2 = 1:n_Test, Var1 = "RF")

pred_true <- data.table(value = na.remove(Test_month$Price), Var2 = 1:n_Test, Var1 = "Real")
preds_all <- rbindlist(list(pred_rf, pred_true), use.names = T)

ggplot(preds_all, aes(Var2, value, color = as.factor(Var1))) +
  geom_line(alpha = 0.7, size = 1.2) +
  labs(x = "Time", y = "Price", title = "Random Forest Forecast") +
  guides(color=guide_legend(title="Method")) +
  theme_ts
#*******************************************************************************
#####
#Week-ahead forecasting
#######
# Splitting data to training data and testing data
#It would be easier to use window function and 
#for loop but for the analysis purposes I have done it manually
#forecasting one week ahead
Train_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-08-20") & Datetime > as.Date("2017-01-01")))
Test_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-08-27") & Datetime >= as.Date("2019-08-20")))

Train_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-08-27") & Datetime > as.Date("2017-01-01")))
Test_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-09-03") & Datetime >= as.Date("2019-08-27")))

Train_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-09-03") & Datetime > as.Date("2017-01-01")))
Test_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-09-10") & Datetime >= as.Date("2019-09-03")))

Train_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-09-10") & Datetime > as.Date("2017-01-01")))
Test_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-09-17") & Datetime >= as.Date("2019-09-10")))


Train_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-09-17") & Datetime > as.Date("2017-01-01")))
Test_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-09-24") & Datetime >= as.Date("2019-09-17")))

Train_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-09-24") & Datetime > as.Date("2017-01-01")))
Test_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-01") & Datetime >= as.Date("2019-09-24")))

Train_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-01") & Datetime > as.Date("2017-01-01")))
Test_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-08") & Datetime >= as.Date("2019-10-01")))

Train_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-08") & Datetime > as.Date("2017-01-01")))
Test_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-15") & Datetime >= as.Date("2019-10-08")))

Train_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-15") & Datetime > as.Date("2017-01-01")))
Test_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-22") & Datetime >= as.Date("2019-10-15")))

Train_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-22") & Datetime > as.Date("2017-01-01")))
Test_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-29") & Datetime >= as.Date("2019-10-22")))

Train_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-29") & Datetime > as.Date("2017-01-01")))
Test_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-11-05") & Datetime >= as.Date("2019-10-29")))


Train_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-11-05") & Datetime > as.Date("2017-01-01")))
Test_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-11-12") & Datetime >= as.Date("2019-11-05")))

Train_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-11-12") & Datetime > as.Date("2017-01-01")))
Test_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-11-19") & Datetime >= as.Date("2019-11-12")))

Train_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-11-19") & Datetime > as.Date("2017-01-01")))
Test_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-11-26") & Datetime >= as.Date("2019-11-19")))

Train_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-11-26") & Datetime > as.Date("2017-01-01")))
Test_week <- data.frame(prices %>% filter(Datetime < as.Date("2019-12-02") & Datetime >= as.Date("2019-11-26")))

#*******************************************
rf_result2<- NULL
smap2 <- NULL
mase2<- NULL
#*******************************************
#STL decomposition
# convert data to ts object
Train_ts <- ts(Train_week$Price, freq = 24*7)
decomp_ts <- stl(na.remove(Train_ts), s.window = "periodic", robust = TRUE)$time.series


#Plot time series decompsition
Train_ts %>% na.remove() %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot()

#Convert data to multiple seasonal object
Train_msts <- msts(na.remove(Train_week$Price), seasonal.periods = c(24, 24*7))
#Plot msts
Train_msts %>% mstl() %>% autoplot() 

#*****************************************************************
#number of observations of testing data
n_Test <- nrow(Test_week)
#number of observations of training data
n_Train <- nrow(Train_week)
period <- 24


#Trend part
trend_part <- ts(decomp_ts[,2])

#fit ARIMA to trend
trend_fit <- auto.arima(trend_part) 
# trend forecast
trend_for <- as.vector(forecast(trend_fit, n_Test)$mean) 

# Fourier features to model (Daily and Weekly)
fourier_part <- fourier(Train_msts, K = c(5,1)) 

window <- (n_Train / period) - 1


# detrended original time series
new_Price <- rowSums(decomp_ts[, c(1,3)]) 
# lag part to model
lag_part <- decomp_ts[1:(period*window), 1] 

matrix_train <- data.table(Price = tail(new_Price,window*period),
                           fourier_part[seq_along((period+1):n_Train),],
                           Lag = lag_part)
#*************************************************************************
rf_model <- randomForest(Price ~ ., data = data.frame(matrix_train),
                         ntree = 1000, mtry = 4, nodesize = 5, importance = TRUE)


#variable importance plots
varImpPlot(rf_model, main = "Variable importance")

#RF predictions 
pred_rf <- predict(rf_model, data.frame(matrix_test)) + mean(trend_for)

#Calculate accuracy measures
rf_accuracy2<- forecast::accuracy(as.vector(pred_rf),na.remove(Test_week$Price))
rf_smap2<- smape(Test_week$Price, as.vector(pred_rf)) 
rf_mase2 <- mase(Test_week$Price, as.vector(pred_rf))
mase2<- rbind(rf_mase2,mase2)
smap2<- rbind(rf_smap2,smap2)
rf_result2<- rbind(rf_accuracy2,rf_result2)
#******************************************************************
#matrix with all forecasting accuracy measures as columns and
# forecasting different months as rows.
rf_result2 <- cbind(rf_result2,smap2,mase2)
#******************************************************************
# plot the prediction results with the real values of prices
pred_rf <- data.table(value = pred_rf, Var2 = 1:n_Test, Var1 = "RF")

pred_true <- data.table(value = na.remove(Test_week$Price), Var2 = 1:n_Test, Var1 = "Real")
preds_all <- rbindlist(list(pred_rf, pred_true), use.names = T)

ggplot(preds_all, aes(Var2, value, color = as.factor(Var1))) +
  geom_line(alpha = 0.7, size = 1.2) +
  labs(x = "Time", y = "Price", title = "Random Forest Forecast") +
  guides(color=guide_legend(title="Method")) +
  theme_ts

#********************************************************************
#######
#Forecasting Day ahead
###########
# Splitting data to training data and testing data
#It would be easier to use window function and 
#for loop but for the analysis purposes I have done it manually
#forecasting one week ahead
Train_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-01") & Datetime > as.Date("2017-01-01")))
Test_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-02") & Datetime >= as.Date("2019-10-01")))

Train_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-02") & Datetime > as.Date("2017-01-01")))
Test_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-03") & Datetime >= as.Date("2019-10-02")))

Train_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-03") & Datetime > as.Date("2017-01-01")))
Test_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-04") & Datetime >= as.Date("2019-10-03")))

Train_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-04") & Datetime > as.Date("2017-01-01")))
Test_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-05") & Datetime >= as.Date("2019-10-04")))

Train_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-05") & Datetime > as.Date("2017-01-01")))
Test_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-06") & Datetime >= as.Date("2019-10-05")))


Train_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-06") & Datetime > as.Date("2017-01-01")))
Test_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-07") & Datetime >= as.Date("2019-10-06")))

Train_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-07") & Datetime > as.Date("2017-01-01")))
Test_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-08") & Datetime >= as.Date("2019-10-07")))

Train_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-08") & Datetime > as.Date("2017-01-01")))
Test_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-09") & Datetime >= as.Date("2019-10-08")))

Train_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-09") & Datetime > as.Date("2017-01-01")))
Test_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-10") & Datetime >= as.Date("2019-10-09")))

Train_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-10") & Datetime > as.Date("2017-01-01")))
Test_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-11") & Datetime >= as.Date("2019-10-10")))

Train_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-11") & Datetime > as.Date("2017-01-01")))
Test_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-12") & Datetime >= as.Date("2019-10-11")))

Train_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-12") & Datetime > as.Date("2017-01-01")))
Test_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-13") & Datetime >= as.Date("2019-10-12")))

Train_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-13") & Datetime > as.Date("2017-01-01")))
Test_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-14") & Datetime >= as.Date("2019-10-13")))


Train_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-14") & Datetime > as.Date("2017-01-01")))
Test_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-15") & Datetime >= as.Date("2019-10-14")))

Train_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-15") & Datetime > as.Date("2017-01-01")))
Test_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-16") & Datetime >= as.Date("2019-10-15")))

Train_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-16") & Datetime > as.Date("2017-01-01")))
Test_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-17") & Datetime >= as.Date("2019-10-16")))

Train_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-17") & Datetime > as.Date("2017-01-01")))
Test_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-18") & Datetime >= as.Date("2019-10-17")))

Train_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-18") & Datetime > as.Date("2017-01-01")))
Test_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-19") & Datetime >= as.Date("2019-10-18")))
#**********************************************************************
rf_result3<- NULL
smap3 <- NULL
mase3<- NULL
#*********************************************************************
#STL decomposition
# convert data to ts object
Train_ts <- ts(Train_Day$Price, freq = 24*7)
decomp_ts <- stl(na.remove(Train_ts), s.window = "periodic", robust = TRUE)$time.series


#Plot time series decompsition
Train_ts %>% na.remove() %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot()

#Convert data to multiple seasonal object
Train_msts <- msts(na.remove(Train_Day$Price), seasonal.periods = c(24, 24*7))
#Plot msts
Train_msts %>% mstl() %>% autoplot() 

# #*******************************************************
# #find the best K values to model daily and weekly seasonality
# aic_vals_temps <- NULL
# aic_vals <- NULL
# for ( i in 1:5)
# {
#   for(j in 1:5) 
#   {
#     
#     #xreg1 <- fourier(msts_Train,K=i,h=24)
#     #xreg2 <- fourier(msts_Train,K=j,h=168)
#     #xreg3 <- fourier(msts_Train,K=m,h=8766)
#     #xtrain <- cbind(xreg1,xreg2,xreg3)
#     xtrain= fourier(Train_msts,K=c(i,j))
#     fitma1 <- auto.arima(Train_msts,xreg=xtrain,D=0,max.p = 0,max.q=0)
#     aic_vals_temps <- cbind(i,j,fitma1$aic)
#     aic_vals <- rbind(aic_vals,aic_vals_temps)
#     
#     
#   }
# }
# colnames(aic_vals)<- c("FourierTerm24","FourierTerm168","AICValue")
# aic_vals <- data.frame(aic_vals)
# minAICVal <- min(aic_vals$AICValue)
# minvals<- aic_vals[which(aic_vals$AICValue==minAICVal),]
# #*****************************************************************************
#number of observations of testing data
n_Test <- nrow(Test_Day)
#number of observations of training data
n_Train <- nrow(Train_Day)
period <- 24


#Trend part
trend_part <- ts(decomp_ts[,2])

#fit ARIMA to trend
trend_fit <- auto.arima(trend_part) 
# trend forecast
trend_for <- as.vector(forecast(trend_fit, n_Test)$mean) 

# Fourier features to model (Daily and Weekly)
fourier_part <- fourier(Train_msts, K = c(5,1)) 

window <- (n_Train / period) - 1


# detrended original time series
new_Price <- rowSums(decomp_ts[, c(1,3)]) 
# lag part to model
lag_part <- decomp_ts[1:(period*window), 1] 

matrix_train <- data.table(Price = tail(new_Price,window*period),
                           fourier_part[seq_along((period+1):n_Train),],
                           Lag = lag_part)


#**************************************************************
#RF model is applied to the training data 
rf_model <- randomForest(Price ~ ., data = data.frame(matrix_train),
                         ntree = 1000, mtry = 4, nodesize = 5, importance = TRUE)

#variable importance
varImpPlot(rf_model, main = "Variable importance")

#RF predictions 
pred_rf <- predict(rf_model, data.frame(matrix_test)) + mean(trend_for)

#Calculate accuracy measures
rf_accuracy3<- forecast::accuracy(as.vector(pred_rf),Test_Day$Price)
rf_smap3<- smape(Test_Day$Price, as.vector(pred_rf))
rf_mase3 <- mase(Test_Day$Price, as.vector(pred_rf))
smap3<- rbind(rf_smap3,smap3)
mase3<- rbind(rf_mase3,mase3)
rf_result3<- rbind(rf_accuracy3,rf_result3)
#**************************************************************************
##matrix with all forecasting accuracy measures as columns and
# forecasting different months as rows.
rf_result3 <- cbind(rf_result3,smap3,mase3)
#***************************************************************************
#plot the predictions and the real values 
pred_rf <- data.table(value = pred_rf, Var2 = 1:n_Test, Var1 = "RF")

pred_true <- data.table(value = na.remove(Test_Day$Price), Var2 = 1:n_Test, Var1 = "Real")
preds_all <- rbindlist(list(pred_rf, pred_true), use.names = T)

ggplot(preds_all, aes(Var2, value, color = as.factor(Var1))) +
  geom_line(alpha = 0.7, size = 1.2) +
  labs(x = "Time", y = "Price", title = "Random Forest Forecast") +
  guides(color=guide_legend(title="Method")) +
  theme_ts
