library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)
library(readxl)

elec<-read_xlsx("D:\\sem4\\Time Series Forecasting\\US Electricity.xlsx")
head(elec)
tail(elec)
elec.ts<-ts(elec$`Total Renewable Energy Production`,start=c(1973,1),end=c(2024,03),frequency=12)
print(elec.ts)
autoplot(elec.ts)
ggseasonplot(elec.ts)
ggsubseriesplot(elec.ts)

elec_window<-window(elec.ts,start=c(2015,1),end=c(2023,6))
autoplot(elec_window)

autoplot(elec_window) +
  autolayer(meanf(elec_window, h=12),
            series="Mean", PI=FALSE) +
  autolayer(naive(elec_window, h=12),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(elec_window, h=12),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts for monthly renewable energy production") +
  xlab("Year") + ylab("Energy in Quadrillion Btu") +
  guides(colour=guide_legend(title="Forecast"))
meanf(elec_window, h=12)
naive(elec_window, h=12)
snaive(elec_window, h=12)
elec_hw<-HoltWinters(elec_train,seasonal="additive")
elec_hw
autoplot(elec_window) + autolayer(holt(elec_window, h=12))

acf(elec_window)
adf.test(elec_window)
ndiffs(elec_window)

nsdiffs(elec_window)
diff_elec_window <- diff(elec_window)
autoplot(diff_elec_window)
acf(diff_elec_window)
adf.test(diff_elec_window)

pacf(diff_elec_window)
auto.arima(diff_elec_window,ic="aic",trace=TRUE)
auto.arima(diff_elec_window,stepwise=TRUE, approximation=FALSE,ic="bic",trace=TRUE,seasonal = TRUE)

m1<-arima(diff_elec_window, order=c(0,0,1), seasonal=list(order=c(0,1,2), period=12))
summary(m1)
checkresiduals
autoplot(forecast(m1,level=c(95), h=12))
best_model <- arima(diff_elec_window, order=c(1,0,1), seasonal=list(order=c(0,1,2), period=12))
summary(best_model)
checkresiduals(best_model)

acf(ts(best_model$residuals))
pacf(ts(best_model$residuals))
forecast(best_model,level=c(95), h=12)
autoplot(forecast(best_model,level=c(95), h=12))
Box.test(best_model$residuals, lag=20, type="Ljung-Box")

BoxCox.lambda(elec_window)
