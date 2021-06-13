#*********************************************************
#Month-ahead
#####
# Splitting data to training data and testing data
#It would be easier to use window function and 
#for loop but for the analysis purposes I have done it manually
#Divide the data to training and testing data
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
#****************************************************************
tbat_result1<-NULL
stlm_result1<-NULL
stlm_smap1<-NULL
tbat_smap1<-NULL
stlm_mase1 <-NULL
tbat_mase1 <- NULL
tbats_dm <-NULL
stlm_dm <- NULL
#***************************************************************
#number of days per testing data
n <- dim(Test_month)[1]/ 24
#Convert Training data and testing data to msts object 
msts_Train <- msts(Train_month$Price,seasonal.periods=c(24,168,8766)) 
msts_Test <- msts(Test_month$Price,seasonal.periods=c(24,168,8766)) 


#************************************************************************
# Fit TBATS Model to Training data
tbats_mod <- tbats(log(msts_Train), seasonal.periods = c(24,168,8766),use.trend = FALSE,use.damped.trend = FALSE)
#plot tbats decomposition Figure 3-3: Trigonometric decomposition of electricity prices data
plot(tbats_mod)
#TBATS forecast
tbats_model <-  forecast(tbats_mod,h=24*n)
#plot prediction interval 
#Figure 4-3: Example of Weekly forecast with prediction intervals
plot(tbats_model)

#***************************************************************************
#plot STL decomposition (Figure 3-4: Multiple STL of electricity prices data)
msts_Train %>% mstl() %>% autoplot() 

#fit STLM model to training data
stlm_mod<- msts_Train %>% stlm(lambda = 0)
stlm_model <- msts_Train %>% stlm(lambda = 0) %>% forecast(h = 24*n) 
#plot prediction interval
#Figure 4-3: Example of Weekly forecast with prediction intervals
plot(stlm_model)
#**************************************************************************
#Calculate errors to use it for DM test
tbats_dm <- rbind(tbats_dm,as.vector(Test_month$Price-exp(tbats_model$mean))) 
stlm_dm <- rbind(stlm_dm,as.vector(Test_month$Price-stlm_model$mean))   
#*************************************************************************
#calculate accuracy measures for TBATS and STLM model

stlm_accuracy1<- forecast::accuracy(as.vector(stlm_model$mean) , msts_Test)
tbat_accuracy1<- forecast::accuracy(as.vector(exp(tbats_model$mean)), msts_Test)

stlm_result1<-rbind(stlm_accuracy1,stlm_result1)

tbat_result1<-rbind(tbat_accuracy1,tbat_result1)

stlm_smap1<- rbind(s_smap1,stlm_smap1)
tbat_smap1 <- rbind(t_smap1,tbat_smap1)
stlm_mase1<- rbind(s_mase1,stlm_mase1)
tbat_mase1 <- rbind(t_mase1,tbat_mase1)

#**************************************************************************

#sMAPE error of prediction 
s_smap1<- smape(msts_Test, as.vector(stlm_model$mean))
t_smap1<- smape(msts_Test, as.vector(exp(tbats_model$mean)))
s_mase1<- mase(msts_Test, as.vector(stlm_model$mean))
t_mase1<- mase(msts_Test, as.vector(exp(tbats_model$mean)))

#plot the two models with actual data
accuracyData <- data.frame(datetime= Test_month$Datetime ,
                           actual = as.vector(msts_Test) ,
                           stlmForecast = as.vector(stlm_model$mean) ,
                           tbatsForecast = as.vector(exp(tbats_model$mean)))

accuracyData %>% 
  ggplot() +
  geom_line(aes(x = (Test_month$Datetime ), y = (Test_month$Price), colour = "actual"))+
  geom_line(aes(x = (Test_month$Datetime), y = stlm_model$mean, colour = "STLM"))+
  geom_line(aes(x = (Test_month$Datetime ), y = exp(tbats_model$mean),   colour = "TBATS "))+ 
  scale_y_continuous()+ylab("Price") + xlab("Date")+guides(color=guide_legend(title="Model"))+ ggtitle("STLM and TBATS Comparison")+theme_ts+ theme(plot.title = element_text(hjust = 0.5))
#****************************************************************************
#matrix with all forecasting accuracy measures as columns and
# forecasting different months as rows for TBATS and STLM models
tbat_result1 <- cbind(tbat_result1,tbat_smap1,tbat_mase1)
stlm_result1 <- cbind(stlm_result1,stlm_smap1,stlm_mase1)

#***************************************************************************************************
#####
#Week-ahead forecasting
#####
# Splitting data to training data and testing data
#It would be easier to use window function and 
#for loop but for the analysis purposes I have done it manually
#Divide the data to training and testing data

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
#***********************************************************************************
tbat_result2<-NULL
stlm_result2<-NULL
stlm_smap2<-NULL
tbat_smap2<-NULL
stlm_mase2 <-NULL
tbat_mase2 <- NULL
#************************************************************************************
#number of days per testing data
n <- dim(Test_week)[1] / 24
#convert training data and testing data to mts object
msts_Train <- msts(Train_week$Price,seasonal.periods=c(24,168,8766)) 
msts_Test <- msts(Test_week$Price,seasonal.periods=c(24,168,8766)) 


#*************************************************************************
#TBATS Model
tbats_mod <- tbats(log(msts_Train), seasonal.periods = c(24,168,8766),use.trend = FALSE,use.damped.trend = FALSE)
##plot TBATS decomposition Figure 3-3: Trigonometric decomposition of electricity prices data 
plot(tbats_mod)
#forecast
tbats_model <-  forecast(tbats_mod,h=24*n)
#plot prediction interval
plot(tbats_model)
#**************************************************************************
#plot time decomposition
msts_Train %>% mstl() %>% autoplot() 
#fit STLM model
stlm_mod <- msts_Train %>% stlm(lambda = 0)
stlm_model <- msts_Train %>% stlm(lambda = 0) %>% forecast(h = 24*n) 
#plot predction interval for STLM
plot(stlm_model)
#**************************************************************************
#calculate accuracy measures

stlm_accuracy2<- forecast::accuracy(as.vector(stlm_model$mean) , msts_Test)
tbat_accuracy2<- forecast::accuracy(as.vector(exp(tbats_model$mean)), msts_Test)

#sMAPE error of prediction 
s_smap2<- smape(msts_Test, as.vector(stlm_model$mean))
t_smap2<- smape(msts_Test, as.vector(exp(tbats_model$mean)))
s_mase2<- mase(msts_Test, as.vector(stlm_model$mean))
t_mase2<- mase(msts_Test, as.vector(exp(tbats_model$mean)))

stlm_result2<-rbind(stlm_accuracy2,stlm_result2)

tbat_result2<-rbind(tbat_accuracy2,tbat_result2)

stlm_smap2<- rbind(s_smap2,stlm_smap2)
tbat_smap2 <- rbind(t_smap2,tbat_smap2)
stlm_mase2<- rbind(s_mase2,stlm_mase2)
tbat_mase2 <- rbind(t_mase2,tbat_mase2)

#**************************************************************************************
#plot the two models with actual data
accuracyData <- data.frame(datetime= Test_week$Datetime ,
                           actual = as.vector(msts_Test) ,
                           stlmForecast = as.vector(stlm_model$mean) ,
                           tbatsForecast = as.vector(exp(tbats_model$mean)))

accuracyData %>% 
  ggplot() +
  geom_line(aes(x = (Test_week$Datetime ), y = (Test_week$Price), colour = "actual"))+
  geom_line(aes(x = (Test_week$Datetime), y = stlm_model$mean, colour = "STLM"))+
  geom_line(aes(x = (Test_week$Datetime ), y = exp(tbats_model$mean),   colour = "TBATS "))+ 
  scale_y_continuous()+ylab("Price") + xlab("Date")+guides(color=guide_legend(title="Model"))+ ggtitle("STLM and TBATS Comparison")+theme_ts+ theme(plot.title = element_text(hjust = 0.5))
#***************************************************************************************
#matrix with all forecasting accuracy measures as columns and
# forecasting different months as rows for TBATS and STLM models
tbat_result2 <- cbind(tbat_result2,tbat_smap2,tbat_mase2)
stlm_result2 <- cbind(stlm_result2,stlm_smap2,stlm_mase2)
#****************************************************************************************
#####
#**********************************************************
#Day-ahead forecasting
#####
# Splitting data to training data and testing data
#It would be easier to use window function and 
#for loop but for the analysis purposes I have done it manually
#Divide the data to training and testing data
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
#********************************************************************************
tbat_result3<-NULL
stlm_result3<-NULL
stlm_smap3<-NULL
tbat_smap3<-NULL
stlm_mase3 <-NULL
tbat_mase3 <- NULL
#********************************************************************************
#number of days per testing data
n <- dim(Test_Day)[1] / 24
#convert training and testing data to mts object
msts_Train <- msts(Train_Day$Price,seasonal.periods=c(24,168,8766)) 
msts_Test <- msts(Test_Day$Price,seasonal.periods=c(24,168,8766)) 
#********************************************************************************
#fit TBATS Model
tbats_mod <- tbats(log(msts_Train), seasonal.periods = c(24,168,8766),use.trend = FALSE,use.damped.trend = FALSE)
##plot TBATS decomposition Figure 3-3: Trigonometric decomposition of electricity prices data 
plot(tbats_mod)
#forecast
tbats_model <-  forecast(tbats_mod,h=24*n)
#plot prediction interval
plot(tbats_model)
#*******************************************************************************
#plot STL time decomposition
msts_Train %>% mstl() %>% autoplot() 
#stlm model
stlm_mod <- msts_Train %>% stlm(lambda = 0)
stlm_model <- msts_Train %>% stlm(lambda = 0) %>% forecast(h = 24*n) 
#plot prediction interval
plot(stlm_model)
#*******************************************************************************
#calculate accuracy measures
stlm_accuracy3<- forecast::accuracy(as.vector(stlm_model$mean) , msts_Test)
tbat_accuracy3<- forecast::accuracy(as.vector(exp(tbats_model$mean)), msts_Test)

#sMAPE error of prediction 
s_smap3<- smape(msts_Test, as.vector(stlm_model$mean))
t_smap3<- smape(msts_Test, as.vector(exp(tbats_model$mean)))
s_mase3<- mase(msts_Test, as.vector(stlm_model$mean))
t_mase3<- mase(msts_Test, as.vector(exp(tbats_model$mean)))

stlm_result3<-rbind(stlm_accuracy3,stlm_result3)

tbat_result3<-rbind(tbat_accuracy3,tbat_result3)

stlm_smap3<- rbind(s_smap3,stlm_smap3)
tbat_smap3 <- rbind(t_smap3,tbat_smap3)
stlm_mase3<- rbind(s_mase3,stlm_mase3)
tbat_mase3 <- rbind(t_mase3,tbat_mase3)


#matrix with all forecasting accuracy measures as columns and
# forecasting different months as rows for TBATS and STLM models
tbat_result3 <- cbind(tbat_result3,tbat_smap3,tbat_mase3)
stlm_result3 <- cbind(stlm_result3,stlm_smap3,stlm_mase3)
#*********************************************************************************
#plot the two models with actual data
accuracyData <- data.frame(datetime= Test_Day$Datetime ,
                           actual = as.vector(msts_Test) ,
                           stlmForecast = as.vector(stlm_model$mean) ,
                           tbatsForecast = as.vector(exp(tbats_model$mean)))

accuracyData %>% 
  ggplot() +
  geom_line(aes(x = (Test_Day$Datetime ), y = (Test_Day$Price), colour = "actual"))+
  geom_line(aes(x = (Test_Day$Datetime), y = stlm_model$mean, colour = "STLM"))+
  geom_line(aes(x = (Test_Day$Datetime ), y = exp(tbats_model$mean),   colour = "TBATS "))+ 
  scale_y_continuous()+ylab("Price") + xlab("Date")+guides(color=guide_legend(title="Model"))+ ggtitle("STLM and TBATS Comparison")+theme_ts+ theme(plot.title = element_text(hjust = 0.5))
#**************************************************************************************
#####







