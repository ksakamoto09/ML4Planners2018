y <- ts(1:10, start = 2000)

y
library(dplyr)
y %>% class()

ts(1:100, start = 2000, frequency = 12)
library(readr)
citiBike <- read_csv("Data/CitiBike.csv")

citiBike %>% head()

cbWeek <- citiBike %>% mutate(Date = lubridate::mdy(Date),
                             year = lubridate::year(Date),
                             week = lubridate::week(Date)) %>% 
    arrange(Date)%>% 
    group_by(year,week) %>% 
    summarise(trips = sum(`Trips over the past 24-hours (midnight to 11:59pm)`),
              miles = sum(`Miles traveled today (midnight to 11:59 pm)`)) %>% 
    ungroup() %>% 
    select(trips, miles)

cbWeek
cbWeekts <- ts(cbWeek, start = 2015, frequency = 52.18)
cbWeekts

cbWeekts2 <- window(cbWeekts, start=2018)
library(forecast)

autoplot(cbWeekts)
autoplot(cbWeekts2)

cbWeekts3 <- window(cbWeekts, start = 2017, end = 2018)
autoplot(cbWeekts3) + ggtitle("Citibike Info 2017-2018") + 
    xlab("Weeks") + ylab("Counts")

fpp::a10 %>% autoplot()

ggAcf(cbWeekts[,"trips"])

meanf(cbWeekts[,"trips"], h =4)

naive(cbWeekts[,"trips"], h = 4)

rwf(cbWeekts[,"trips"], h = 4)

snaive(cbWeekts[,"trips"], h = 4)

rwf(cbWeekts[,"trips"], h = 4, drift = TRUE)

autoplot(cbWeekts[,"trips"]) + 
    autolayer(meanf(cbWeekts[,"trips"], h =10), series = "meanf", PI = FALSE) +
    autolayer(naive(cbWeekts[,"trips"], h = 10), series = "naive", PI = FALSE) +
    autolayer(snaive(cbWeekts[,"trips"], h = 10), series = "snaive", PI = FALSE) +
    autolayer(rwf(cbWeekts[,"trips"], h = 10, drift = TRUE), series = "drift", PI = FALSE)


autoplot(cbWeekts[,"trips"]) +     
    autolayer(snaive(cbWeekts[,"trips"], h = 4), series = "snaive", PI = TRUE)

fitted(naive(cbWeekts[,"trips"])) %>% autoplot(series = "fitted") + 
    autolayer(cbWeekts[,"trips"], series = "data")


fc1 <- snaive(cbWeekts[, "trips"])

fc1 %>% class()
fc1$method
fc1$model
fc1$x
fc1$fitted
fc1$residuals
fc1$lower
res <- fc1 %>% residuals()
res

autoplot(res)
gghistogram(res, add.normal = TRUE)
ggAcf(res)

checkresiduals(fc1)

cbMonth <- citiBike %>% mutate(Date = lubridate::mdy(Date),
                              year = lubridate::year(Date),
                              month = lubridate::month(Date)) %>% 
    arrange(Date)%>% 
    group_by(year,month) %>% 
    summarise(trips = sum(`Trips over the past 24-hours (midnight to 11:59pm)`),
              miles = sum(`Miles traveled today (midnight to 11:59 pm)`)) %>% 
    ungroup() %>% 
    select(trips, miles)
cbMonthts <- ts(cbMonth, start = 2015, frequency = 12)
cbMonthts

monthDay <- cbind(Monthly = cbMonthts[,"trips"],
                  DailyAverage = cbMonthts[,"trips"]/
                      monthdays(cbMonthts[,"trips"]))

autoplot(monthDay, facet = TRUE)

lambdaVal <- BoxCox.lambda(cbMonthts[,"trips"])
autoplot(BoxCox(cbMonthts[, "trips"], lambdaVal), series = "boxcox") 
autoplot(cbMonthts[, "trips"], series = "data")

train <- window(cbWeekts[,"trips"], end = 2018.7)
test <- window(cbWeekts[,"trips"], start = 2018.7)

fc2 <- meanf(train, h = 20)
fc2
fc3 <- rwf(train, drift = TRUE, h = 20)
fc4 <- snaive(train, h = 20)
accuracy(fc2, test)
accuracy(fc3, test)
accuracy(fc4, test)

autoplot(cbind(training = train, test = test)) +
    autolayer(fc2, series = "meanf" , PI = FALSE) +
    autolayer(fc3, series = "drift", PI = FALSE) + 
    autolayer(fc4, series = "snaive", PI = FALSE)

cv1 <- train %>% tsCV(forecastfunction = snaive,h = 1)

cv1^2 %>% mean(na.rm = TRUE) %>% sqrt

train %>% snaive() %>% residuals -> res
res^2 %>% mean(na.rm=TRUE) %>% sqrt()

cv2 <- train %>% tsCV(forecastfunction = rwf, drift = TRUE, h = 1)
   
cv2^2 %>% mean(na.rm = TRUE) %>% sqrt

fc5 <- ses(cbMonthts[,"trips"], h = 5)
fc5
autoplot(cbMonthts[,"trips"]) + 
    autolayer(fc5, series = "ses")

fc6 <- holt(cbMonthts[,"trips"], h = 5)

autoplot(cbMonthts[,"trips"]) + 
    autolayer(fc6, series = "holt")

fc7 <- holt(cbMonthts[,"trips"], h = 5, damped = TRUE)

autoplot(cbMonthts[,"trips"]) + 
    autolayer(fc7, series = "holt")

fc8 <- hw(cbMonthts[,"trips"], h = 24, seasonal = "additive")

fc9 <- hw(cbMonthts[,"trips"], h = 24, seasonal = "multiplicative")
autoplot(cbMonthts[,"trips"]) + 
    autolayer(fc8, series = "additive", PI = FALSE) +
    autolayer(fc9, series = "multiplicative", PI =FALSE)

plot(fc8$model$states[,1:3], col = "blue", main = "Additive Components")

plot(fc9$model$states[,1:3], col = "blue", main = "Multiplicative Components")

cbDay <- ts(citiBike$`Trips over the past 24-hours (midnight to 11:59pm)`, start = 2015, frequency = 365.25)
cbDay

cbDay %>% autoplot()

cbDay %>% log() %>% autoplot()

cbDay %>% diff(lag= 365) %>% diff(lag=1) %>% autoplot()

fc10 <- auto.arima(cbMonthts[,"trips"])

fc10 %>% forecast(h =10) %>% autoplot()

ggAcf(cbMonthts[,"trips"])
ggPacf(cbMonthts[,"trips"])
