#***********************************************************
## Exploratory data analysis
## Data processing
#***********************************************************
# Load librairies
##############################
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(imputeTS)
library(gridExtra)
library(splines)
library(lubridate)
library(zoo)
library(astsa)
library(forecast)
library(tseries)
library(urca)
library(mgcv)
library(car)
library(data.table)
library(modelr)
library(randomForest)
library(TSPred)
library(stats)
library(chron)
library(timeDate)
library(bizdays)
library(Metrics)
library(multDM)
############################## 
#************************************************************
#Read Data
#*************************************************************
#Dowload Data from CSV file
prices <- read.csv('prices.csv')

#Head of data
head(prices)

#************************************************************

#Data Cleaning and Manipulation 

#*************************************************************
#From factor to character
prices$Datetime <- as.character(prices$Datetime)

#add column Datetime converted to date-time class
prices$Datetime<-as.POSIXct(prices$Datetime,format="%Y-%m-%d %H:%M:%S")


# Examine NA values for the date time
which(is.na(prices$Datetime))
which(is.na(prices$Price))
# Fill the DateTime NA values
prices$Datetime[2186] <- "2017-03-26 01:00:00 BST"
prices$Datetime[10418] <- "2018-03-25 01:00:00 BST"
prices$Datetime[19322] <- "2019-03-31 01:00:00 BST"

# Fill the NA price values due to the change to/from daylight saving 
# time with average of the prices before an after
# 2187 <- (47.07+39.97)/2
# 10419 <- (59.99+54)/2
# 19323 <- (41.94+43.03)/2
prices$Price[2187] <- 43.52
prices$Price[10419] <- 56.995
prices$Price[19323] <- 42.485

#Dealing with missing values
#make a data frame with full series of dates
full_dates <- seq(min(prices$Datetime),max(prices$Datetime),by= "1 hour")
full_dates <- data.frame(Datetime = full_dates)
# merge it with the prices to add the missing dates with NA's for the price
prices <- merge(full_dates, prices,by= "Datetime",all.x=TRUE)

#Calendar adjustments
#add date column
prices$Date <- as.Date(prices$Datetime)
#add hour of the day column with 1-23
prices$Hour <- as.integer(format(as.POSIXct(prices$Datetime), format = "%H"))
#add month of the year column with values 1-12
prices$Month <- as.integer(format(as.POSIXct(prices$Datetime) ,format = "%m"))
#add year column with different years values 2016-2020
prices$Year <- format(as.POSIXct(prices$Datetime) ,format = "%Y")
#add day of the week column with Mon-FRi values
prices$Day <- weekdays(prices$Date)
#add week of the year column with 1-52 values
prices$Week <- week(prices$Date)
#convert year to integer
prices$Year <- as.integer(prices$Year)
#convert days of the week to numbers from 1-7
prices$Day <- as.integer(recode(prices$Day, "'Monday' ='1';'Tuesday'='2'; 'Wednesday'='3'; 'Thursday'='4';
                               'Friday'='5';'Saturday'='6';'Sunday'='7'"))

#add season column with 4 values 
yq <- as.yearqtr(as.yearmon(prices$Datetime, "%Y-%m-%d") + 1/12)
prices$Season <- factor(format(yq, "%q"), levels = 1:4, 
                        labels = c("winter", "spring", "summer", "fall"))
#convert season to integer
prices$Season <- as.integer(prices$Season)

#add type of the day column with 1-3 values weekday/weekend/public holiday.
prices$Wd_Hd[isWeekday(prices$Datetime)& !isHoliday(as.timeDate(prices$Date), holidays = holidayLONDON(2016:2020))] <- 1
prices$Wd_Hd[!isWeekend(prices$Datetime) & isHoliday(as.timeDate(prices$Date), holidays = holidayLONDON(2016:2020))] <- 3
prices$Wd_Hd[isWeekend(prices$Datetime)] <- 2

#add day of the year column 
prices$DOY <- as.numeric(strftime(prices$Datetime, format = "%j"))
  
#reorder columns
#prices <- prices[,c(1,3,6,5,4,7,8,9,2)]

#find values more than 150
which(prices$Price >=150)
#delete the outliers
prices$Price[91] <- NA
prices$Price[10389] <- NA
prices$Price[10653] <- NA
prices$Price[16748] <- NA
prices$Price[16749] <- NA
prices$Price[16845] <- NA
prices$Price[21483] <- NA
prices$Price[21485] <- NA

#*********************************************************************
#exploring the data
#*********************************************************************
#Plot data
p <- prices %>%
  ggplot( aes(x=Datetime, y= Price))+
  geom_line(color="#69b3a2") + 
  labs(x="Date",y="Electricity Prices (£)",title="Electricity spot price at UK")+theme_ts+
  theme(plot.title= element_text(hjust = 0.5))
q<- ggplotly(p)

#plot data after interpolation 
#convert data first to msts object
msts_prices <- msts(prices$Price,seasonal.periods=c(24,168,8766),start=c(2017,01))
prices_interp<-msts_prices %>% na.interp()%>%autoplot(series="Interpolated")+autolayer(msts_prices, series="Original")+scale_color_manual(values=c(Interpolated="red",Original="grey"))+theme_ts+labs(title="Electricity Spot Prices at UK",y="Prices(£)")+theme(plot.title= element_text(hjust = 0.5),legend.position="bottom")
msts_prices %>% na.interp()%>% autoplot()+theme_ts+labs(title="Electricity Spot Prices at UK",y="Prices(£)")+theme(plot.title= element_text(hjust = 0.5),legend.position="bottom")+theme(legend.position="bottom")

#plot average hourly prices for different months  for weekdays and weekends
labels <- c("Mar","May","Aug","Nov")
names(labels)<- c(3,5,8,11)

#Figure 3-2: Average hourly price for selected months in 2018
Hourly<-prices%>%filter(Year==2018 & Month %in% c(3,5,8,11))%>% group_by(Month,Hour,WD)%>% summarise(Price=mean(Price))
  ggplot(Hourly,aes(Hour,Price))+geom_line(aes(Hour,Price,color=as.factor(WD)))+facet_wrap(~Month,labeller=labeller(Month= labels))+labs(x="Hour",y="Price(£)",title="Average Hourly price 2018")+theme_ts+
  scale_colour_discrete(name ="", labels = c("weekdays","weekends"))+ scale_x_continuous(minor_breaks = seq(1, 24, 1))+theme(plot.title= element_text(hjust = 0.5))


Seasonaly<-prices%>%filter(Year==2019)%>% group_by(Season,Hour,WD)%>% summarise(Price=mean(Price))
  ggplot(Seasonaly,aes(Hour,Price))+geom_line(aes(Hour,Price,color=as.factor(WD)))+facet_wrap(~Season,labeller = labeller(Season=labels))+labs(x="Hour",y="Price(£)",title="Average Hourly price 2018")+theme_ts+
    scale_colour_discrete(name ="", labels = c("weekdays","weekends"))+ scale_x_continuous(minor_breaks = seq(1, 24, 1))
  
  labels = c("Winter", "Spring", "Summer", "Fall")
  names(labels)<- c(1,2,3,4)

#plot prices per hour
ggplot(prices, aes(Hour, log(Price))) +
  geom_boxplot()+xlab('Hour')


#plot prices for each season
ggplot(prices, aes(Season, log(Price))) +geom_boxplot()+xlab('Season')


# plot prices for each month
ggplot(prices, aes(Month, log(Price))) + geom_boxplot()+xlab('Month')

# plot prices for each Day
ggplot(prices, aes(Day, log(Price))) + geom_boxplot()+xlab('Day')



# plot correlogram,ACF and PCF, regular seasonal pattern,and due to the trend have positive values
acf(prices$Price,main="",na.action=na.pass)
pacf(prices$Price,na.action = na.pass)

# Figure 3-1: Electricity spot prices at UK
xacf <- acf(prices$Price, plot = FALSE,na.action = na.pass)
exRandAcf <- data.frame(lag=xacf$lag, acf=xacf$acf)
exRandData <- data.frame(t= prices$Datetime, d= prices$Price)
exRandp1<- ggplot(exRandData, aes(t,d)) + geom_line(color="blue") +
  xlab("Time") + ylab("Data") + ggtitle("Electricity Prices")+theme_ts+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")
exRandp2<- ggplot(exRandAcf, aes(lag, acf)) +
  geom_segment(aes(xend = lag, yend = 0)) +
  geom_hline(aes(yintercept = 0)) + xlab("Lag") + ylab("ACF") +
  geom_hline(aes(yintercept = 0.196), linetype = 2, color = 'blue') +
  geom_hline(aes(yintercept = -0.196), linetype = 2, color = 'blue') +
  ggtitle("Correlogram")+theme_ts+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom",legend.box = "horizontal")
grid.arrange(prices_interp, exRandp2, nrow=2)

#*****************************************************************
#Data processing
#*****************************************************************
# missing values interpolations
prices$Price<-na.interp(prices$Price)
# convert to msts object
msts_prices <- msts(prices$Price,seasonal.periods=c(24,168,8766)) 
#STL decomposition for electricity prices
autoplot(mstl(msts_prices))+ggtitle("Multiple STL for the electricity prices series")+theme(plot.title= element_text(hjust = 0.5))

#********************************************************************
#Theme used for all plots
theme_ts <- theme(panel.border = element_rect(fill = NA, 
                                              colour = "grey10"),
                  panel.background = element_blank(),
                  panel.grid.minor = element_line(colour = "grey85"),
                  panel.grid.major = element_line(colour = "grey85"),
                  panel.grid.major.x = element_line(colour = "grey85"),
                  axis.text = element_text(size = 13, face = "bold"),
                  axis.title = element_text(size = 15, face = "bold"),
                  plot.title = element_text(size = 16, face = "bold"),
                  strip.text = element_text(size = 16, face = "bold"),
                  strip.background = element_rect(colour = "black"),
                  legend.text = element_text(size = 15),
                  legend.title = element_text(size = 16, face = "bold"),
                  legend.background = element_rect(fill = "white"),
                  legend.key = element_rect(fill = "white"))
#*************************************************************************

#*************************************************************************
#after running all models,plot example of month-ahead,week-ahead and day-ahead forecasting
#with the actual data
#Figure 4-1: Example of monthly, weekly and daily forecast for all models.
#plot monthly forecast (First Month)
######
#plot the two models with actual data
accuracyData <- data.frame(datetime= Test_month$Datetime ,
                           Actual = as.vector(Test_month$Price) ,
                           STLM = as.vector(stlm_model$mean) ,
                           TBATS = as.vector(exp(tbats_model$mean)),
                           Regression = as.vector(arima_forcast$mean),
                           GAM = pred_gam5_ar0,
                           GAMM= fcast,
                           RF=as.vector(pred_rf))


accuracyData %>% 
  ggplot() +
  geom_line(aes(x = (Test_month$Datetime ), y = (Test_month$Price), colour = "Actual"),size=1.1)+
  geom_line(aes(x = (Test_month$Datetime), y = STLM, colour = "STLM"),size=1.1)+
  geom_line(aes(x = (Test_month$Datetime ), y = TBATS,   colour = "TBATS "),size=1.1)+ 
  geom_line(aes(x = (Test_month$Datetime ), y = Regression,   colour = "DR "),size=1.1)+
  geom_line(aes(x = (Test_month$Datetime ), y = GAM,   colour = "GAM "),size=1.1)+
  geom_line(aes(x = (Test_month$Datetime ), y = GAMM,   colour = "GAMM "),size=1.1)+
  geom_line(aes(x = (Test_month$Datetime ), y = RF,   colour = "RF "),size=1.1)+
  scale_y_continuous()+ylab("Price") + xlab("Date")+guides(color=guide_legend(title="Model"))+ 
  ggtitle("Forecast vs real time series")+ theme(plot.title = element_text(hjust = 0.5))+theme_ts 
###
#plot weekly forecasting plot (2nd week)
#plot the two models with actual data
accuracyData <- data.frame(datetime= Test_week$Datetime ,
                           Actual = as.vector(Test_week$Price) ,
                           STLM = as.vector(stlm_model$mean) ,
                           TBATS = as.vector(exp(tbats_model$mean)),
                           Regression = as.vector(arima_forcast$mean),
                           GAM = pred_gam5_ar0,
                           GAMM= fcast,
                           RF=as.vector(pred_rf))


accuracyData %>% 
  ggplot() +
  geom_line(aes(x = (Test_week$Datetime ), y = (Test_week$Price), colour = "Actual"),size=1.1)+
  geom_line(aes(x = (Test_week$Datetime), y = STLM, colour = "STLM"),size=1.1)+
  geom_line(aes(x = (Test_week$Datetime ), y = TBATS,   colour = "TBATS "),size=1.1)+ 
  geom_line(aes(x = (Test_week$Datetime ), y = Regression,   colour = "DR "),size=1.1)+
  geom_line(aes(x = (Test_week$Datetime ), y = GAM,   colour = "GAM "),size=1.1)+
  geom_line(aes(x = (Test_week$Datetime ), y = GAMM,   colour = "GAMM "),size=1.1)+
  geom_line(aes(x = (Test_week$Datetime ), y = RF,   colour = "RF "),size=1.1)+
  scale_y_continuous()+ylab("Price") + xlab("Date")+guides(color=guide_legend(title="Model"))+ ggtitle("Forecast vs real time series")+ theme(plot.title = element_text(hjust = 0.5))+theme_ts
#****************************************************************************************
#plot Daily data (second day)
#plot the two models with actual data
accuracyData <- data.frame(datetime= Test_Day$Datetime ,
                           Actual = as.vector(msts_Test) ,
                           STLM = as.vector(stlm_model$mean) ,
                           TBATS = as.vector(exp(tbats_model$mean)),
                           Regression = as.vector(arima_forcast$mean),
                           GAM = InvBoxCox(as.vector(pred_gam5_ar0),lambda=lambda),
                           GAMM=InvBoxCox(as.vector(fcast),lambda),
                           RF=as.vector(pred_rf))


accuracyData %>% 
  ggplot() +
  geom_line(aes(x = as.numeric(Test_Day$Hour), y = (Test_Day$Price), colour = "Actual"),size=1.1)+
  geom_line(aes(x = as.numeric(Test_Day$Hour), y = STLM, colour = "STLM"),size=1.1)+
  geom_line(aes(x = as.numeric(Test_Day$Hour), y = TBATS,   colour = "TBATS "),size=1.1)+ 
  geom_line(aes(x = as.numeric(Test_Day$Hour), y = Regression,   colour = "DR "),size=1.1)+
  geom_line(aes(x = as.numeric(Test_Day$Hour), y = GAM,   colour = "GAM "),size=1.1)+
  geom_line(aes(x = as.numeric(Test_Day$Hour), y = GAMM,   colour = "GAMM "),size=1.1)+
  geom_line(aes(x = as.numeric(Test_Day$Hour), y = RF,   colour = "RF "),size=1.1)+
  scale_y_continuous()+ylab("Price") + xlab("Date")+guides(color=guide_legend(title="Model"))+ggtitle("Forecast vs real time series")+ theme(plot.title = element_text(hjust = 0.5))+theme_ts+scale_x_continuous(breaks=seq(0,23,1))




# this function plots a forcast evaluation
forecast_eval = function(forcast, model_name = "", eval_set = test){
  acc = forecast::accuracy(forcast, eval_set)
  acc = round(acc[6], 2) 
  model_name = model_name
  forcast %>% 
    autoplot() + 
    autolayer(test, series = "Test set")+
    theme_classic()+
    labs(title = paste(model_name, " model of by ", acc, " on average across test set", sep = ""))
}

msts_Train <- msts(Train_week$Price,seasonal.periods=c(24,168,8766),end=as.Date("2019-08-26"))
msts_Test <- msts(Test_week$Price,seasonal.periods=c(24,168,8766),start=as.Date("2019-08-27"))

autoplot(tbats_model,include=500,main="TBATS Model Week-ahead Forecasting",xlab="Time Index",ylab="Prices") +autolayer(msts_Test,series="Test set")+theme_ts+theme(plot.title = element_text(hjust = 0.5))
autoplot(stlm_model,include=500,main="STLM Model Week-ahead Forecasting",xlab="Time Index",ylab="Prices") +autolayer(msts_Test,series="Test set")+theme_ts+theme(plot.title = element_text(hjust = 0.5))
#***********************************************************************************************
#####
#***********************************************************************************************
#Plot RMSE and MASE of all models 
#Figure 4-2: RMSE and MAE for all models
#####
RF_monthly <-read.table("RF_monthly.txt",sep="", header = TRUE)
GAM_monthly <-read.table("GAM_monthly1.txt",sep="", header = TRUE)
GAMM_monthly <-read.table("GAMM_monthly1.txt",sep="", header = TRUE)
Tbats_monthly <-read.table("Tbats_monthly.txt",sep="", header = TRUE)
DR_monthly <-read.table("DR_monthly.txt",sep="", header = TRUE)
stlm_monthly <-read.table("stlm_monthly.txt",sep="", header = TRUE)
str(RF_monthly)
data_rmse <- cbind(Tbats_monthly[,2],DR_monthly[,2],stlm_monthly[,2],GAMM_monthly[,2],RF_monthly[,2],GAM_monthly[,2])
data_rmse <- data_rmse[2:12,]
colnames(data_rmse) <- c("TBATS","DR","STLM","GAMM","RF","GAM")
m<-as.data.frame(melt(data_rmse))
mean_rmse <- read.table("RMSE_mean.txt",sep="", header = TRUE)
x1<-ggplot(m, aes(x = m[,2], y = m[,3]),fill=m[,2]) +
  geom_boxplot(aes(fill=m[,2]))+theme_ts+labs(x="Models",y="RMSE",title = "RMSE for month-ahead forecasting")+theme(plot.title = element_text(hjust = 0.5),legend.position = "none")+stat_summary(fun.y=mean, colour="darkred", geom="point",shape=18, size=3,show_guide = FALSE)


data_mae <- cbind(Tbats_monthly[,3],DR_monthly[,3],stlm_monthly[,3],GAMM_monthly[,3],RF_monthly[,3],GAM_monthly[,3])
data_mae<- data_mae[2:12,]
colnames(data_mae) <- c("TBATS","DR","STLM","GAMM","RF","GAM")
m1<-as.data.frame(melt(data_mae))

x2<-ggplot(m1, aes(x = m1[,2], y = m1[,3]),fill=m1[,2]) +
  geom_boxplot(aes(fill=m1[,2]))+theme_ts+labs(x="Models",y="MAE",title = "MAE for month-ahead forecasting")+theme(plot.title = element_text(hjust = 0.5),legend.position = "none")+stat_summary(fun.y=mean, colour="darkred", geom="point",shape=18, size=3,show_guide = FALSE)
grid.arrange(x1, x2, ncol=2)
#*******************************************************************************************************
#week-ahead RMSE and MASE
RF_weekly <-read.table("RF_weekly.txt",sep="", header = TRUE)
DR_weekly <-read.table("DR_weekly.txt",sep="", header = TRUE)
stlm_weekly <-read.table("stlm_weekly.txt",sep="", header = TRUE)
GAM_weekly <-read.table("GAM_weekly1.txt",sep="", header = TRUE)
GAMM_weekly <-read.table("GAMM_weekly1.txt",sep="", header = TRUE)
Tbats_weekly <-read.table("Tbats_weekly.txt",sep="", header = TRUE)

data_rmse <- cbind(Tbats_weekly[,2],DR_weekly[,2],stlm_weekly[,2],GAMM_weekly[,2],RF_weekly[,2],GAM_weekly[,2])
colnames(data_rmse) <- c("TBATS","DR","STLM","GAMM","RF","GAM")
data_rmse<- data_rmse[1:12,]
m<-as.data.frame(melt(data_rmse))

x1<-ggplot(m, aes(x = m[,2], y = m[,3]),fill=m[,2]) +
  geom_boxplot(aes(fill=m[,2]))+theme_ts+labs(x="Models",y="RMSE",title = "RMSE for week-ahead forecasting")+theme(plot.title = element_text(hjust = 0.5),legend.position = "none")+stat_summary(fun.y=mean, colour="darkred", geom="point",shape=18, size=3,show_guide = FALSE)


data_mae <- cbind(Tbats_weekly[,3],DR_weekly[,3],stlm_weekly[,3],GAMM_weekly[,3],RF_weekly[,3],GAM_weekly[,3])
colnames(data_mae) <- c("TBATS","DR","STLM","GAMM","RF","GAM")
data_mae<- data_mae[1:12,]
m1<-as.data.frame(melt(data_mae))

x2<-ggplot(m1, aes(x = m1[,2], y = m1[,3]),fill=m1[,2]) +
  geom_boxplot(aes(fill=m1[,2]))+theme_ts+labs(x="Models",y="MAE",title = "MAE for week-ahead forecasting")+theme(plot.title = element_text(hjust = 0.5),legend.position = "none")+stat_summary(fun.y=mean, colour="darkred", geom="point",shape=18, size=3,show_guide = FALSE)
grid.arrange(x1, x2, ncol=2)
#*************************************************************************************************************
#Day-ahead RMSE and MAE

RF_daily <-read.table("RF_daily.txt",sep="", header = TRUE)
DR_daily <-read.table("DR_daily.txt",sep="", header = TRUE)
stlm_daily <-read.table("stlm_daily.txt",sep="", header = TRUE)
GAM_daily <-read.table("GAM_daily1.txt",sep="", header = TRUE)
GAMM_daily <-read.table("GAMM_daily1.txt",sep="", header = TRUE)
Tbats_daily<-read.table("Tbats_daily.txt",sep="", header = TRUE)

data_rmse <- cbind(Tbats_daily[,2],DR_daily[,2],stlm_daily[,2],GAMM_daily[,2],RF_daily[,2],GAM_daily[,2])
colnames(data_rmse) <- c("TBATS","DR","STLM","GAMM","RF","GAM")
data_rmse<- data_rmse[1:16,]
m<-as.data.frame(melt(data_rmse))

x1<-ggplot(m, aes(x = m[,2], y = m[,3]),fill=m[,2]) +
  geom_boxplot(aes(fill=m[,2]))+theme_ts+labs(x="Models",y="RMSE",title = "RMSE for day-ahead forecasting")+theme(plot.title = element_text(hjust = 0.5),legend.position = "none")+stat_summary(fun.y=mean, colour="darkred", geom="point",shape=18, size=3,show_guide = FALSE)


data_mae <- cbind(Tbats_daily[,3],DR_daily[,3],stlm_daily[,3],GAMM_daily[,3],RF_daily[,3],GAM_daily[,3])
colnames(data_mae) <- c("TBATS","DR","STLM","GAMM","RF","GAM")
data_mae<- data_mae[1:16,]
m1<-as.data.frame(melt(data_mae))

x2<-ggplot(m1, aes(x = m1[,2], y = m1[,3]),fill=m1[,2]) +
  geom_boxplot(aes(fill=m1[,2]))+theme_ts+labs(x="Models",y="MAE",title = "MAE for day-ahead forecasting")+theme(plot.title = element_text(hjust = 0.5),legend.position = "none")+stat_summary(fun.y=mean, colour="darkred", geom="point",shape=18, size=3,show_guide = FALSE)
grid.arrange(x1, x2, ncol=2)

#*****************************************************************************************************
#####
#DM test
#####
#dm test between forecast error of each model for all forecasting forizons
#do two tests with each pair of model
#one sided test greater
#one sided test less

for(i in 1:744) {
  d<-dm.test(tbats_dm[,i],stlm_dm[,i],alternative = "less",h=i)
  p1_val[i]<- d$p.value
} 

for(i in 1:744) {
  d<-dm.test(tbats_dm[,i],stlm_dm[,i],alternative = "greater",h=i)
  p2_val[i]<- d$p.value
} 

for(i in 1:744) {
  d<-dm.test(tbats_dm[,i],arima_dm[,i],alternative = "greater",h=i)
  p_val[i]<- d$p.value
}

for(i in 1:744) {
  d<-dm.test(tbats_dm[,i],rf_dm[,i],alternative = "greater",h=i)
  p_val[i]<- d$p.value
}

for(i in 1:744) {
  d<-dm.test(tbats_dm[,i],gam_dm[,i],alternative = "greater",h=i)
  p_val[i]<- d$p.value
}

for(i in 1:744) {
  d<-dm.test(as.numeric(tbats_dm[,i]),gam_dm[,i],alternative = "greater",h=i)
  p_val[i]<- d$p.value
}


for(i in 1:744) {
  d<-dm.test(arima_dm[,i],stlm_dm[,i],alternative = "greater",h=i)
  p_val[i]<- d$p.value
}

for(i in 1:744) {
  d<-dm.test(tbats_dm[,i],rf_dm[,i],alternative = "greater",h=i)
  p_val[i]<- d$p.value
}

for(i in 1:744) {
  d<-dm.test(stlm_dm[,i],rf_dm[,i],alternative = "greater",h=i)
  p_val[i]<- d$p.value
}

for(i in 1:744) {
  d<-dm.test(arima_dm[,i],rf_dm[,i],alternative = "greater",h=i)
  p_val[i]<- d$p.value
}

ggplot(as.data.frame(p_val),aes(x=1:744,y=p_val))+geom_point(shape=17)+geom_line()+geom_hline(yintercept=c(-0.025,0.025), linetype="dashed", color = "red")+xlab("Hours")+ylab("P-values")+ggtitle("Random forest vs TABTS")+theme(plot.title = element_text(hjust = 0.5))



