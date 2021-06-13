#************************************************************
#Box-Cox transformation
lambda<- BoxCox.lambda(prices$Price)
prices$Price <- BoxCox(prices$Price,lambda = lambda)
#************************************************************
#GAM Month-ahead forecasting
###########################
# Splitting data to training data and testing data
#It would be easier to use window function and 
#for loop but for the analysis purposes I have done it manually
#split the data to training and testing data
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
#**************************************************************************
gam_result1<- NULL
smap1 <- NULL
mase1<- NULL
gam_dm <-NULL
#*************************************************************************
#fit M1,M2,M3,M4,M5,M6
gam1 <-  gam(Price ~ s(Day,bs="cc",k=7)+s(Hour, bs="cc",k=24)+ s(Month, bs = "cc", k = 12),data=Train_week) 

gam2 <-  gam(Price ~ s(Hour,Month,by=as.factor(Day),bs="tp"),data=Train_month)

gam3 <- gam(Price ~ s(as.numeric(Datetime),bs="cr")+s(DOY,bs="cc",k=20)+s(Hour,Day,Month,bs= "tp"), data = Train_month)

gam4 <- gam(Price ~ s(as.numeric(Datetime),bs="cr")+ s(DOY,bs="cc",k=20)+as.factor(Day)+as.factor(Wd_Hd)+s(Hour,Month,Day,bs="tp"), data = Train_month)

gam5 <- gam(Price ~ s(DOY,bs="cc",k=20)+s(as.numeric(Datetime),bs="cr")+as.factor(Wd_Hd)+s(Hour,Day,Month,bs= "tp")+as.factor(Day)+s(Hour,bs="cc",k=24)+s(Month,bs="cc",k=12), data = Train_month)

gam6 <- gam(Price ~ s(DOY,bs="cc",k=20)+ as.numeric(Datetime) +as.factor(Wd_Hd)+s(Hour,Day,Month,bs= "tp")+as.factor(Day)+s(Hour,bs="cc",k=24)+s(Month,bs="cc",k=12), data = Train_month)

#compare between them 
AIC(gam1,gam2,gam4,gam5,gam6)

#We choose model gam5 and fit it to the training data 
gam5_ar0<- gam(Price ~ s(DOY,bs="cc",k=20)+s(as.numeric(Datetime),bs="cr")+as.factor(Wd_Hd)+s(Hour,Day,Month,bs= "tp")+as.factor(Day)+s(Hour,bs="cc",k=24)+s(Month,bs="cc",k=12), data = Train_month)
#************************************************************************************
#Calculate model performance 
pred_gam5_ar0 <- predict(gam5_ar0,Test_month[,-2],type = "response")
error<- as.vector(InvBoxCox(Test_month$Price,lambda)- InvBoxCox(as.vector(pred_gam5_ar0),lambda))
gam_dm <- rbind(error,gam_dm)

result1 <-forecast::accuracy(InvBoxCox(as.vector(pred_gam5_ar0),lambda),InvBoxCox(Test_month$Price,lambda))
gam_smap1<-smape(InvBoxCox(as.vector(pred_gam5_ar0),lambda),InvBoxCox(Test_month$Price,lambda))
gam_mase1<- mase(InvBoxCox(as.vector(pred_gam5_ar0),lambda),InvBoxCox(Test_month$Price,lambda))

smap1<- rbind(gam_smap1,smap1)
mase1<- rbind(gam_mase1,mase1)
gam_result1<- rbind(result1,gam_result1)
#************************************************************************************
#matrix with all forecasting accuracy measures as columns and
# forecasting different months as rows for GAM model
gam_result1 <- cbind(gam_result1,smap1,mase1)
#************************************************************************************

############################
#GAM Week-ahead forecasting 
###########################
# Splitting data to training data and testing data
#It would be easier to use window function and 
#for loop but for the analysis purposes I have done it manually
#split the data to training and testing data

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
#********************************************************************************************
gam_result2<- NULL
smap2 <- NULL
mase2<- NULL
#********************************************************************************************
#fit the best Model 
gam5_ar0<- gam(Price ~ s(DOY,bs="cc",k=20)+s(as.numeric(Datetime),bs="cr")+s(Hour,Month,bs= "tp",by=as.factor(Day))+s(Hour,bs="cc",k=24)+s(Month,bs="cc",k=12)+as.factor(Day)+as.factor(Wd_Hd), data = Train_week)

#calculate accuracy measures 
pred_gam5_ar0 <- predict(gam5_ar0,Test_week[,-2],type = "response")
result2 <-forecast::accuracy(InvBoxCox(as.vector(pred_gam5_ar0),lambda),InvBoxCox(Test_week$Price,lambda))
gam_smap2<-smape(InvBoxCox(as.vector(pred_gam5_ar0),lambda),InvBoxCox(Test_week$Price,lambda))
gam_mase2<- mase(InvBoxCox(as.vector(pred_gam5_ar0),lambda),InvBoxCox(Test_week$Price,lambda))


smap2<- rbind(gam_smap2,smap2)
mase2<- rbind(gam_mase2,mase2)
gam_result2<- rbind(result2,gam_result2)
#*******************************************************************************************
#matrix with all forecasting accuracy measures as columns and
# forecasting different months as rows for GAM model
gam_result2 <- cbind(gam_result2,smap2,mase2)
#*******************************************************************************************
###########################
#GAM Forecasting Day ahead
##########################
# Splitting data to training data and testing data
#It would be easier to use window function and 
#for loop but for the analysis purposes I have done it manually
#split the data to training and testing data
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
#************************************************************************************
gam_result3<- NULL
length(gam_result3) <- 7
smap3 <- NULL
mase3 <-NULL
#************************************************************************************
#fit the best model
gam5_ar0<- gam(Price ~ s(DOY,bs="cc",k=20)+s(as.numeric(Datetime),bs="cr")+as.factor(Wd_Hd)+s(Hour,Day,Month,bs= "tp")+as.factor(Day)+s(Hour,bs="cc",k=24)+s(Month,bs="cc",k=12), data = Train_Day)


#calculate accuracy measures 
pred_gam5_ar0 <- predict(gam5_ar0,Test_Day[,-2],type = "response")
result3 <-forecast::accuracy(InvBoxCox(as.vector(pred_gam5_ar0),lambda),InvBoxCox(Test_Day$Price,lambda))
gam_smap3<-smape(InvBoxCox(as.vector(pred_gam5_ar0),lambda),InvBoxCox(Test_Day$Price,lambda))
gam_mase3<- mase(InvBoxCox(as.vector(pred_gam5_ar0),lambda),InvBoxCox(Test_Day$Price,lambda))

smap3<- rbind(gam_smap3,smap3)
mase3<- rbind(gam_mase3,mase3)
gam_result3<- rbind(result3,gam_result3)
#***************************************************************************************
#matrix with all forecasting accuracy measures as columns and
# forecasting different months as rows for GAM model
gam_result3 <- cbind(gam_result3,smap3,mase3)
#***************************************************************************************
############################
#GAMM Month-ahead forecasting
###########################
# Splitting data to training data and testing data
#It would be easier to use window function and 
#for loop but for the analysis purposes I have done it manually
#split the data to training and testing data
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

#***************************************************************************************************

gam_result4<- NULL
length(gam_result4) <- 7
smap4<- NULL
mase4<- NULL
#**************************************************************************************************
#number of observations for testing data 
n_month<- nrow(Test_month)

#fit the gam model
gam5_ar0<- gam(Price ~ s(DOY,bs="cc",k=20)+s(as.numeric(Datetime),bs="cr")+as.factor(Wd_Hd)+s(Hour,Day,Month,bs= "tp")+as.factor(Day)+s(Hour,bs="cc",k=24)+s(Month,bs="cc",k=12), data = Train_month)

#***************************************************************************************************
#calculate GAM model residuals 
gam_errors <- residuals(gam5_ar0, type = "response")

#ARIMA model for GAM errors
error_mod <- Arima(gam_errors, order=c(5,0,2),seasonal = list(order=c(1,1,2),period=24))
#checkresiduals(error_mod, theme = theme_bw())
#***************************************************************************************************


#Forecast
error_fcast <- forecast(error_mod, h = n_month)$mean
gam_fcast <- predict(gam5_ar0, newdata = Test_month)
fcast <- gam_fcast + error_fcast

#calculate accuracy
result4 <-forecast::accuracy(InvBoxCox(as.vector(fcast),lambda),InvBoxCox(Test_month$Price,lambda))
gam_smap4<-smape(InvBoxCox(as.vector(fcast),lambda),InvBoxCox(Test_month$Price,lambda))
gam_mase4<- mase(InvBoxCox(as.vector(fcast),lambda),InvBoxCox(Test_month$Price,lambda))

smap4<- rbind(gam_smap4,smap4)
mase4<- rbind(gam_mase4,mase4)
gam_result4<- rbind(result4,gam_result4)
#***************************************************************************************************
#matrix with all forecasting accuracy measures as columns and
# forecasting different months as rows for GAM model
gam_result4 <- cbind(gam_result4,smap4,mase4)
#****************************************************************************************************
###########################
#GAMM Week-ahead forecasting
###########################
# Splitting data to training data and testing data
#It would be easier to use window function and 
#for loop but for the analysis purposes I have done it manually
#split the data to training and testing data
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

#*******************************************************************************************
gam_result5<- NULL
smap5<- NULL
mase5<- NULL
#******************************************************************************************
#number of testing data observations
n_week <- nrow(Test_week)

#fit the GAM model
gam5_ar0<- gam(Price ~ s(DOY,bs="cc",k=20)+s(as.numeric(Datetime),bs="cr")+as.factor(Wd_Hd)+s(Hour,Day,Month,bs= "tp")+as.factor(Day)+s(Hour,bs="cc",k=24)+s(Month,bs="cc",k=12), data = Train_week)
#*******************************************************************************************
#calculate GAM model residuals 
gam_errors <- residuals(gam5_ar0, type = "response")
#fit ARIMA to GAM errors
error_mod <- Arima(gam_errors, order=c(5,0,2),seasonal = list(order=c(1,1,2),period=24))
#checkresiduals(error_mod, theme = theme_bw())

#*******************************************************************************************
#forecast
error_fcast <- forecast(error_mod, h = n_week)$mean
gam_fcast <- predict(gam5_ar0, newdata = Test_week)
fcast <- gam_fcast + error_fcast

#Calculate accuracy measures 
result5 <-forecast::accuracy(InvBoxCox(as.vector(fcast),lambda),InvBoxCox(Test_week$Price,lambda))
gam_smap5<-smape(InvBoxCox(as.vector(fcast),lambda),InvBoxCox(Test_week$Price,lambda))
gam_mase5<- mase(InvBoxCox(as.vector(fcast),lambda),InvBoxCox(Test_week$Price,lambda))

smap5<- rbind(gam_smap5,smap5)
mase5<- rbind(gam_mase5,mase5)
gam_result5<- rbind(result5,gam_result5)
#************************************************************************************************
#matrix with all forecasting accuracy measures as columns and
# forecasting different months as rows for GAM model
gam_result5 <- cbind(gam_result5,smap5,mase5)
#**********************************************************************************************
###########################
#GAMM Day-ahead forecasting
###########################
# Splitting data to training data and testing data
#It would be easier to use window function and 
#for loop but for the analysis purposes I have done it manually
#split the data to training and testing data

Train_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-01") & Datetime > as.Date("2017-01-01")))
Test_Day <- data.frame(prices %>% filter(Datetime < as.Date("2019-10-02") & Datetime >= as.Date("2019-10-01")))


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
#***************************************************************************************************
gam_result6<- NULL
smap6<- NULL
mase6<- NULL
#*************************************************************************************************
#number of testing data observations 
n_Day<- nrow(Test_Day)

#fit the GAM model 
gam5_ar0<- gam(Price ~ s(DOY,bs="cc",k=20)+s(as.numeric(Datetime),bs="cr")+as.factor(Wd_Hd)+s(Hour,Day,Month,bs= "tp")+as.factor(Day)+s(Hour,bs="cc",k=24)+s(Month,bs="cc",k=12), data = Train_Day)
#*************************************************************************************************
#Calculate GAM model residuals
gam_errors <- residuals(gam5_ar0, type = "response")
#fit ARIMA to the GAM errors
error_mod <- Arima(gam_errors, order=c(5,0,2),seasonal = list(order=c(1,1,2),period=24))
#checkresiduals(error_mod, theme = theme_bw())
#*************************************************************************************************
#forecast
error_fcast <- forecast(error_mod, h = n_Day)$mean
gam_fcast <- predict(gam5_ar0, newdata = Test_Day)
fcast <- gam_fcast + error_fcast

#calculate accuracy measures
result6 <-forecast::accuracy(InvBoxCox(as.vector(fcast),lambda),InvBoxCox(Test_Day$Price,lambda))
gam_smap6<-smape(InvBoxCox(as.vector(fcast),lambda),InvBoxCox(Test_Day$Price,lambda))
gam_mase6<- mase(InvBoxCox(as.vector(fcast),lambda),InvBoxCox(Test_Day$Price,lambda))

smap6<- rbind(gam_smap6,smap6)
mase6<- rbind(gam_mase6,mase6)
gam_result6<- rbind(result6,gam_result6)
#***************************************************************************************************
#matrix with all forecasting accuracy measures as columns and
# forecasting different months as rows for GAM model
gam_result6 <- cbind(gam_result6,smap6,mase6)
#***************************************************************************************************

############################################################################################################
#model visualization
########
datas <- data.table(Fitted_values = c(gam5_ar0$gam$fitted.values,
                                      error_mod$fitted),
                    Residuals = c(gam5_ar0$residuals,
                                  error_mod$residual),
                    Model = rep(c("GAM", "GAM and ARMA"), each = nrow(Train_week)))

ggplot(data = datas,
       aes(Fitted_values, Residuals)) +
  facet_grid(Model~., switch = "y") +
  geom_point(size = 1.7) +
  geom_smooth(method = "loess") +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  theme(panel.border = element_rect(fill = NA, color = "black"),plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 13, face = "bold"),
        strip.background = element_rect(color = "black")) +
  labs(title = "Fitted values vs Residuals")

#Figure 3-5: GAM model partial dependence plots.
par(mfrow=c(2,2))
plot(gam5_ar0, select=3, rug=FALSE,main='s(Hour,Day,Month)', cex.axis=1.5, cex.lab=1.5)
plot(gam5_ar0, select=5, rug=FALSE,main='Tensor product', cex.axis=1.5, cex.lab=1.5)

#Figure C-1: GAM model hour vs week.
vis.gam(gam2,ticktype="detailed",color="heat",theta=35,phi=32)






