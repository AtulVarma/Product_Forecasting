###Clear the Global Environment
rm(list=ls(all=TRUE))

### Library Call
library(forecast)
library(lubridate)
library(DataCombine)
library(imputeTS)
library(plyr)
library(dplyr)
library(TTR)
library(graphics)
library(data.table)
#library(Quandl)
library(DMwR)


# Set the working directory

setwd("C:\\Users\\atul\\Desktop")


### Read Data that is stored in csv format
data = read.csv("ExistingCustomersTransactionsData.csv")


##Summary
summary(data)



length(unique(data$BillNumber))

str(data)

##taking only "FUrniture"

data_fur<-data[data$ProductCategory=='Furniture',]



##going with basic info

summary(data_fur)
str(data_fur)
colSums(is.na(data_fur))
library(caret)
install.packages("magrittr")
library(magrittr)

##making date col in date formte
data$BillDate<-as.Date(data$BillDate,'%m/%d/%Y')

##grouping and sumarising(sum) qty and Billdate
data2_fur <- data %>% group_by(month=floor_date(BillDate, "month")) %>% summarize(Quantity=sum(Quantity))

data2_fur$month = as.Date(data2_fur$month,format="%Y-%m-%d")

summary(data2_fur$month)

sum(is.na(data2_fur))

##taking min and max of date

minDate = min(as.Date(data2_fur$month,format="%Y-%m-%d"))
maxDate = max(as.Date(data2_fur$month,format="%Y-%m-%d"))


# Generating the sequence of dates from start date to end date
seq <- data.frame("dateRange"=seq(minDate,maxDate,by="months"))

data3_fur <- seq %>% full_join(data2_fur,c("dateRange" = "month"))

sum(is.na(data3_fur))##no NAs

data3_fur <- data.frame(data3_fur)


datafinal <- data3_fur
head(data3_fur)

# 

# 
# * splitting is done by sequential splitting
Train <- data3_fur[1:(nrow(data3_fur) - 6),]
Test <- data3_fur[(nrow(data3_fur) - 5):nrow(data3_fur),]

##changing  time object

qty <- ts(Train$Quantity, frequency =12)
qty
par(mar=c(1,1,0,0))
plot(qty,type="l",lwd=3,col="red",xlab="month",ylab="qty",main="Time series plot for Book-xyzabc")


##decomaposing 

qtydecomposed = decompose(qty)

plot(qtydecomposed,col="Red")##in graph only trend we can see but there is no seanality

##Now ploting ACF and PACF plot
par(mfrow=c(1,2))
acf(qty)##Here we can see tend
pacf(qty)
ndiffs(qty)##here only one diff reqire



# now making some MA's

#WMA

fitwma<- WMA(qty,n=3,1:3)
### Define the metric MAPE 

wmaTrainMape<- regr.eval(qty[3:length(qty)],fitwma[3:length(qty)])


fitwma1<- WMA(qty,n=2,1:2)
forwma<-forecast(fitwma1, h=6)

wmaTrainMape<- regr.eval(qty[2:length(qty)],fitwma1[2:length(qty)])##





### Exponential Moving Averages
fitEma <- EMA(qty, n = 3)
### Define the metric MAPE 
emaTrainMape <- regr.eval(qty[3:length(qty)],fitEma[3:length(qty)])#
emaTrainMape
fitEma1 <- EMA(qty, n = 2)
emaTrainMape <- regr.eval(qty[2:length(qty)],fitEma1[2:length(qty)])##

##from plot 

par(mfrow=c(1,1))
plot(Train$Quantity, type="l", col="black")

lines(fitwma1, col="blue")
lines(fitEma1, col="brown")



#auto.arima(qty)

## Build a HoltWinters model  with season 
holtqtyforecast <- HoltWinters(qty,alpha = TRUE,gamma = TRUE)
head(holtqtyforecast$fitted)



## HoltWinters model  with trend  and Seasonality'additive'
qtyholtforecast <- HoltWinters(qty,alpha=TRUE, beta=TRUE, gamma=TRUE, seasonal="additive")
head(qtyholtforecast$fitted)





### Prediction on the Train
holtforecastTrain <- data.frame(qtyholtforecast2$fitted)
holtforecastTrainpredictions <- holtforecastTrain$xhat
head(holtforecastTrainpredictions)

### Prediction on test data
holtqtyforecast<-  forecast(qtyholtforecast,h = 6)
plot(holtqtyforecast,ylim = c(-30,30))



### Prediction on test data
holtqtyforecast2<-  forecast(qtyholtforecast2,h = 6)
plot(holtqtyforecast2,ylim = c(-30,30))



### Define the metric hw 
hwTestMape <- regr.eval(Test$Quantity,holtqtyforecast$mean)##
hwTestMape

hwTestMape1 <- regr.eval(Test$Quantity,holtqtyforecast2$mean)#
hwTestMape1

auto.arima(qty)##(0,1,1)(010)




### Arima Models

acf(diff(qty,lag = 1),48)
pacf(diff(qty,lag = 1),48)
plot(diff(qty))


model1 <- arima(qty, order=c(0,1,1), seasonal=c(0,1,0))
model1
forecasts1 <- forecast(model1, h=6)
plot(forecasts1)

arimaModel1TestMape <- regr.eval(Test$Quantity,forecasts1$mean)##rmse 556
arimaModel1TestMape

auto.arima(qty)

#taking tha auto amrima out and using it

model2 <- arima(qty, order=c(0,0,0), seasonal=c(0,1,0))
model2
forecasts2 <- forecast(model2, h=6)
plot(forecasts2)
arimaModel2 <- regr.eval(Test$Quantity,forecasts2$mean)##rmse 556
arimaModel2




###  Auto Arima
MODEL_ARIMA <- auto.arima(qty, ic='bic')
summary(MODEL_ARIMA)
forecasts_autArima<- forecast(MODEL_ARIMA,h=6)
plot(forecasts_autArima,flwd = 3)


##Auto.arima ful

MODEL_ARIMA2<-auto.arima(qty, d = NA, D = NA, max.p = 5, max.q = 5, max.P = 2,
           max.Q = 2, max.order = 5, max.d = 2, max.D = 1, start.p = 2,
           start.q = 2, start.P = 1, start.Q = 1, stationary = FALSE,
           seasonal = TRUE, ic = c=c("aicc", "aic", "bic"), stepwise = TRUE,
           test = c("kpss", "adf", "pp"),
           seasonal.test = c("seas", "ocsb", "hegy", "ch"), allowdrift = TRUE,
           allowmean = TRUE, lambda = "auto", biasadj = FALSE, parallel = FALSE)




3# Box test on our auto.arima model
Box.test(MODEL_ARIMA$residuals, lag = 40, type = "Ljung-Box")







### Now doing on full data




#####FULL data doing previous year seasonality also
auto.arima(qtyfull)

qtyfull <- ts(data3_fur$Quantity,frequency = 12)
qtyfull
###  Auto Arima
MODEL_fit <- auto.arima(qtyfull, ic='bic')
summary(MODEL_fit)
forecasts_fit<- forecast(MODEL_fit,h=6)
plot(forecasts_fit,flwd = 3)


####################
#210(101)

acf(diff(qty,lag = 1),48)
pacf(diff(qty,lag = ),48)
plot(diff(qty))
ndiffs(qtyfull)

qty1<-diff(qty,lag = 1)
qty1<-diff(qty,lag = 2)
plot(qty1)
plot(qty)
plot(decompose(diff(qty1,lag = 1)))



model1 <- arima(qtyfull, order=c(0,0,0), seasonal=c(0,1,0))##taking their forcast values
model1
forecasts1 <- forecast(model1, h=6)
plot(forecasts1)



model2 <- arima(qtyfull, order=c(0,1,1), seasonal=c(0,1,0))
model2
forecasts1 <- forecast(model2, h=6)
plot(forecasts1)

## HoltWinters model  with trend  and Seasonality'additive'
qtyholtforecast <- HoltWinters(qtyfull,alpha=TRUE, beta=TRUE, gamma=TRUE, seasonal="additive")
head(qtyholtforecast$fitted)


