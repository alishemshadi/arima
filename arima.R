library('ggplot2')
library('forecast')
library('tseries')

bitcoin.arima <- function(csv_path = '~/Downloads/BTC-USD (1).csv'){
  # Data Input
  daily_data = read.csv(csv_path, header=TRUE, stringsAsFactors=FALSE)
  daily_data$Date = as.Date(daily_data$Date,format="%d/%m/%Y")
  ggplot(daily_data, aes(Date, Close)) + geom_line() + scale_x_date('month')  + ylab("Daily Bitcoin Price") +
    xlab("")
  
  close_ts = ts(daily_data[, c('Close')])
  daily_data$clean_close = tsclean(close_ts)
  ggplot() +
    geom_line(data = daily_data, aes(x = Date, y = clean_close)) + ylab('Daily Bitcoin Price')
  
  daily_data$close_ma = ma(daily_data$clean_close, order=7) # using the clean count with no outliers
  daily_data$close_ma30 = ma(daily_data$clean_close, order=30)
  
  ggplot() +
    geom_line(data = daily_data, aes(x = Date, y = clean_close, colour = "Price")) +
    geom_line(data = daily_data, aes(x = Date, y = close_ma,   colour = "Weekly Moving Average"))  +
    geom_line(data = daily_data, aes(x = Date, y = close_ma30, colour = "Monthly Moving Average"))  +
    ylab(' Bitcoin Price')
  
  
  close_ma = ts(na.omit(daily_data$close_ma), frequency=30)
  decomp = stl(close_ma, s.window="periodic")
  deseasonal_cls <- seasadj(decomp)
  plot(decomp)
  
  adf.test(close_ma, alternative = "stationary")
  Acf(close_ma, main='')
  
  Pacf(close_ma, main='')
  
  close_d1 = diff(deseasonal_cls, differences = 1)
  plot(close_d1)
  adf.test(close_d1, alternative = "stationary")
  
  Acf(close_d1, main='ACF for Differenced Series')
  Pacf(close_d1, main='PACF for Differenced Series')
  
  auto.arima(deseasonal_cls, seasonal=FALSE)
  fit<-auto.arima(deseasonal_cls, seasonal=FALSE)
  tsdisplay(residuals(fit), lag.max=45, main='(1,2,1) Model Residuals')
  
  fit2 = arima(deseasonal_cls, order=c(1,1,7))
  
  fit2
  
  tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')
  fcast <- forecast(fit2, h=100)
  plot(fcast)
  
  #fit_w_seasonality = auto.arima(deseasonal_cls, seasonal=T)
  #fit_w_seasonality
  #seas_fcast <- forecast(fit_w_seasonality, h=30)
  #plot(seas_fcast)
  
}

