#******************************************************
#Month-ahead forecasting
#####
# Splitting data to training data and testing data
#It would be easier to use window function and 
#for loop but for the analysis purposes I have done it manually
Train_month<- data.frame(prices %>% filter(Datetime < as.Date("2019-01-01") & Datetime > as.Date("2017-01-01")))
Test_month <- data.frame(prices %>% filter(Datetime < as.Date("2019-02-01") & Datetime >= as.Date("2019-01-01")))

Train_month<- data.frame(prices %>% filter(Datetime < as.Date("2019-02-01") & Datetime > as.Date("2017-01-01")))
Test_month <- data.frame(prices %>% filter(Datetime < as.Date("2019-03-01") & Datetime >= as.Date("2019-02-01")))

Train_month<- data.frame(prices %>% filter(Datetime < as.Date("2019-03-01") & Datetime > as.Date("2017-01-01")))
Test_month <- data.frame(prices %>% filter(Datetime < as.Date("2019-04-01") & Datetime >= as.Date("2019-03-01")))

Train_month<- data.frame(prices %>% filter(Datetime < as.Date("2019-04-01") & Datetime > as.Date("2017-01-01")))
Test_month <- data.frame(prices %>% filter(Datetime < as.Date("2019-05-01") & Datetime >= as.Date("2019-04-01")))

Train_month<- data.frame(prices %>% filter(Datetime < as.Date("2019-05-01") & Datetime > as.Date("2017-01-01")))
Test_month <- data.frame(prices %>% filter(Datetime < as.Date("2019-06-01") & Datetime >= as.Date("2019-05-01")))

Train_month<- data.frame(prices %>% filter(Datetime < as.Date("2019-06-01") & Datetime > as.Date("2017-01-01")))
Test_month <- data.frame(prices %>% filter(Datetime < as.Date("2019-07-01") & Datetime >= as.Date("2019-06-01")))

Train_month<- data.frame(prices %>% filter(Datetime < as.Date("2019-07-01") & Datetime > as.Date("2017-01-01")))
Test_month <- data.frame(prices %>% filter(Datetime < as.Date("2019-08-01") & Datetime >= as.Date("2019-07-01")))

Train_month<- data.frame(prices %>% filter(Datetime < as.Date("2019-08-01") & Datetime > as.Date("2017-01-01")))
Test_month <- data.frame(prices %>% filter(Datetime < as.Date("2019-09-01") & Datetime >= as.Date("2019-08-01")))

Train_month<- data.frame(prices %>% filter(Datetime < as.Date("2019-09-01") & Datetime > as.Date("2017-01-01")))
Test_month <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-01") & Datetime >= as.Date("2019-09-01")))

Train_month<- data.frame(prices %>% filter(Datetime < as.Date("2019-10-01") & Datetime > as.Date("2017-01-01")))
Test_month <- data.frame(prices %>% filter(Datetime < as.Date("2019-11-01") & Datetime >= as.Date("2019-10-01")))

Train_month<- data.frame(prices %>% filter(Datetime < as.Date("2019-11-01") & Datetime > as.Date("2017-01-01")))
Test_month <- data.frame(prices %>% filter(Datetime < as.Date("2019-12-01") & Datetime >= as.Date("2019-11-01")))


Train_month<- data.frame(prices %>% filter(Datetime < as.Date("2019-12-01") & Datetime > as.Date("2017-01-01")))
Test_month <- data.frame(prices %>% filter(Datetime < as.Date("2020-01-01") & Datetime >= as.Date("2019-12-01")))
#*********************************************************
arima_dm<- NULL
arima_result1<- NULL
smap1 <- NULL
mase1<-NULL
#*********************************************************
#conver training data and testing data to mts object
msts_Train <- msts(Train_month$Price,seasonal.periods=c(24,168,8766)) 
msts_Test <- msts(Test_month$Price,seasonal.periods=c(24,168,8766))

#***********************************************************
# # Multiple seasonality is modelled with the help of fourier terms ith different periods
#find the best K values with minimum AIC
# aic_vals_temps <- NULL
# aic_vals <- NULL
# for ( i in 1:5)
# {
#   for(j in 1:5) 
#   {
#     for (m in 1:5)
#     {
#       #xreg1 <- fourier(msts_Train,K=i,h=24)
#       #xreg2 <- fourier(msts_Train,K=j,h=168)
#       #xreg3 <- fourier(msts_Train,K=m,h=8766)
#       #xtrain <- cbind(xreg1,xreg2,xreg3)
#       xtrain= fourier(msts_Train,K=c(i,j,m))
#       fitma1 <- auto.arima(msts_Train,xreg=xtrain,D=0,max.p = 0,max.q=0)
#       aic_vals_temps <- cbind(i,j,m,fitma1$aic)
#       aic_vals <- rbind(aic_vals,aic_vals_temps)
#       
#     }
#     
#   }
# }
# colnames(aic_vals)<- c("FourierTerm24","FourierTerm168","FourierTerm8766","AICValue")
# aic_vals <- data.frame(aic_vals)
# minAICVal <- min(aic_vals$AICValue)
# minvals<- aic_vals[which(aic_vals$AICValue==minAICVal),]
#***************************************************************
#Fit DR model
arima_1<- auto.arima(msts_Train,xreg = fourier(msts_Train,K=c(5,1,1)),seasonal = F,lambda=0)

# number of days per testing data 
n <- dim(Test_month)[1] / 24

#Calculate the accuracy measures 
arima_forcast <- forecast(arima_1, xreg = fourier(msts_Train, c(5,1,1), h = n*24),h=n*24)
arima_accuracy1 <- forecast::accuracy(as.vector(arima_forcast$mean),msts_Test)
arima_smap1<- smape(msts_Test, as.vector(arima_forcast$mean))
arima_mase1<- mase(msts_Test, as.vector(arima_forcast$mean))
#calculate error
arima_dm<- rbind(arima_dm,as.vector(Test_month$Price-arima_forcast$mean))

mase1<- rbind(arima_mase1,mase1)
smap1<- rbind(arima_smap1,smap1)
arima_result1<- rbind(arima_accuracy1,arima_result1)
#***************************************************************
###matrix with all forecasting accuracy measures as columns and
# forecasting different months as rows.
arima_result1 <- cbind(arima_result1,smap1,mase1)
#####

#*************************************************************************************************
#weekly forecasting
######
# Splitting data to training data and testing data
#It would be easier to use window function and 
#for loop but for the analysis purposes I have done it manually
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
#***************************************************************
arima_result2<- NULL
smap2 <- NULL
mase2<- NULL
#***************************************************************
#convert Training data and testing data to msts object
msts_Train <- msts(Train_week$Price,seasonal.periods=c(24,168,8766)) 
msts_Test <- msts(Test_week$Price,seasonal.periods=c(24,168,8766),start=c(2019,08,27))
#******************************************************************
#fit the DR model with best k values k=c(5,1,1)
arima_1<- auto.arima(msts_Train,xreg = fourier(msts_Train,K=c(5,1,1)),seasonal = F,lambda=0)

#number of days per testing data
n <- dim(Test_week)[1] / 24
#Calculate the accuracy measures 
arima_forcast <- forecast(arima_1, xreg = fourier(msts_Train, c(5,1,1), h = n*24))
arima_accuracy2 <- forecast::accuracy(as.vector(arima_forcast$mean),msts_Test)
arima_smap2<- smape(msts_Test, as.vector(arima_forcast$mean))
arima_mase2<- mase(msts_Test, as.vector(arima_forcast$mean))
mase2<- rbind(arima_mase2,mase2)
smap2<- rbind(arima_smap2,smap2)
arima_result2<- rbind(arima_accuracy2,arima_result2)
#********************************************************************
##matrix with all forecasting accuracy measures as columns and
# forecasting different months as rows.
arima_result2 <- cbind(arima_result2,smap2,mase2)
#*********************************************************************
###########

#Day Forecasting
############
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
#*************************************************************************
arima_result3<- NULL
smap3 <- NULL
mase3<- NULL
#************************************************************************
#Convert Training data to msts object
msts_Train <- msts(Train_Day$Price,seasonal.periods=c(24,168,8766)) 
msts_Test <- msts(Test_Day$Price,seasonal.periods=c(24,168,8766))
#************************************************************************
#fit the DR model with best k values c(5,1,1)
arima_1<- auto.arima(msts_Train,xreg = fourier(msts_Train,K=c(5,1,1)),seasonal = F,lambda=0)

#number of days per testing data
n <- dim(Test_Day)[1] / 24

#calculate accuracy measures 
arima_forcast <- forecast(arima_1, xreg = fourier(msts_Train, c(5,1,1), h = n*24))
arima_accuracy3 <- forecast::accuracy(as.vector(arima_forcast$mean),msts_Test)
arima_smap3<- smape(msts_Test, as.vector(arima_forcast$mean))
arima_mase3<- mase(msts_Test, as.vector(arima_forcast$mean))
mase3<- rbind(arima_mase3,mase3)
smap3<- rbind(arima_smap3,smap3)
arima_result3<- rbind(arima_accuracy3,arima_result3)
#*************************************************************************
##matrix with all forecasting accuracy measures as columns and
# forecasting different months as rows.
arima_result3 <- cbind(arima_result3,smap3,mase3)
#####
