reload.project()

# Density of data:
#ddply(prices.monthly, .(ZipCode), summarise, n=length(ZipCode))

smoothed <- ddply(prices.monthly, .(ZipCode), function(df) { if(nrow(df)>50) { data.frame(date=as.Date(paste(df$MonthYear, "01", sep="-")), smoothed=ma(df$ppsqft, order=6, centre=T))}})
conn <- dbConnect(MySQL(), user="mcdon", dbname="prices")

dbRemoveTable(conn, name="smoothed")
dbWriteTable(conn, name="smoothed", value=smoothed)

zipforecasts <- data.frame(
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
    n <- 12
    forecast.year <- forecast(model, n)
    ret <- data.frame(
      zipcode=zipcode,
      time=1:n,
      mle=as.vector(forecast.year$mean),
      low80=as.vector(forecast.year$lower[,"80%"]),
      low95=as.vector(forecast.year$lower[,"95%"]),
      high80=as.vector(forecast.year$upper[,"80%"]),
      high95=as.vector(forecast.year$upper[,"95%"]))
    zipforecasts <<- rbind(zipforecasts,ret)
  })
}

dbRemoveTable(conn, name="zipforecasts")
dbWriteTable(conn, name="zipforecasts", value=zipforecasts)

wholecity <- as.ts(prices.monthly.wholecity[,log(ppsqft)])
model <- auto.arima(wholecity, ic="aic", seasonal=F)

model <- Arima(wholecity[1:108], order=c(0,1,1), seasonal=c(0,0,0), include.drift=T)
forecast.year <- forecast(model, 12)

plot(zipcode)
sm <- ma(zipcode, order=12, centre=F)
lines(sm,col=2)

prices.monthly$ppsqft <- ma(zipcode, order=12, centre=F)


prices.monthly[length(ZipCode)==120,by=ZipCode]

forecast.year$mean
forecast.year$lower[,"80%"]
forecast.year$lower[,"95%"]
forecast.year$upper[,"80%"]
forecast.year$upper[,"95%"]


nrow(prices.monthly[ZipCode==11233,])
prices.monthly[ZipCode==11233,]
zipcode <- as.ts(prices.monthly[ZipCode==11233, log(ppsqft)])
model <- auto.arima(zipcode, ic="aic", seasonal=F)
model <- auto.arima(zipcode, ic="bic", seasonal=F)
model <- Arima(zipcode, order=c(2,1,1), seasonal=c(0,0,0), include.drift=T)

plot(forecast(model, 12))
