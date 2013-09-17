
zipforecasts.2012 <- data.frame(
          zipcode=character(0),
          time=integer(0),
          mle=double(0),
          low80=double(0),
          low95=double(0),
          high80=double(0),
          high95=double(0))
hood.forecast <- function(df) {
  try({
    zipcode <- unique(df$ZipCode)
    prices <- as.ts(log(df$ppsqft))
    model <- Arima(prices, order=c(0,1,1), seasonal=c(0,0,0), include.drift=T)
    n <- 18
    forecast.year <- forecast(model, n)
    ret <- data.frame(
      zipcode=zipcode,
      time=1:n,
      mle=as.vector(forecast.year$mean),
      low80=as.vector(forecast.year$lower[,"80%"]),
      low95=as.vector(forecast.year$lower[,"95%"]),
      high80=as.vector(forecast.year$upper[,"80%"]),
      high95=as.vector(forecast.year$upper[,"95%"]))
    zipforecasts.2012 <<- rbind(zipforecasts.2012,ret)
  })
}

#growthestimate <- prices.monthly[! is.na(ZipCode),list(growth=tail(ppsqft, n=1)-head(ppsqft, n=1)), by=ZipCode]

d_ply(prices.monthly[MonthYear <'2012-01'], .(ZipCode), hood.forecast)
head(zipforecasts.2012)
ddply(zipforecasts.2012, .(zipcode), function(df) { data.frame(change=exp(tail(df$mle, n=1)-head(df$mle, n=1)))})

comparison <- merge(
  prices.monthly[! is.na(ZipCode) & MonthYear %in% c('2012-01', '2012-02', '2012-03'), list(oldppsqft=mean(ppsqft)), by="ZipCode"]
  ,
  prices.monthly[MonthYear %in% c('2013-05', '2013-06', '2013-07'), list(newppsqft=mean(ppsqft)), by="ZipCode"]
  , by='ZipCode')

comparison[,list(ZipCode=ZipCode, pricechange=newppsqft-oldppsqft)]
