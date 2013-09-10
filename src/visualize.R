
library(RMySQL)
library(ggplot2)
library(zoo)

conn <- dbConnect(MySQL(), user="mcdon", dbname="prices")

getquery <- "SELECT * FROM sales WHERE GrossSquareFeet > 100 AND SalePrice > 5000 AND TotalUnits > 0 AND NOT Address = 0"
allpricesquery <- "SELECT SaleDate, SalePrice / GrossSquareFeet AS ppsqft FROM sales WHERE SalePrice / GrossSquareFeet > 30 AND GrossSquareFeet > 100 AND SalePrice > 5000 AND TotalUnits > 0 AND NOT Address = 0 ORDER BY SaleDate"
daypricesquery <- "SELECT SaleDate, AVG(SalePrice / GrossSquareFeet) AS ppsqft FROM sales WHERE SalePrice / GrossSquareFeet > 30 AND GrossSquareFeet > 100 AND SalePrice > 5000 AND TotalUnits > 0 AND NOT Address = 0 GROUP BY SaleDate ORDER BY SaleDate"
wburgpricequery <- "SELECT SaleDate, SalePrice / GrossSquareFeet AS ppsqft FROM sales WHERE SalePrice / GrossSquareFeet > 30 AND GrossSquareFeet > 100 AND SalePrice > 5000 AND TotalUnits > 0 AND NOT Address = 0 AND (ZipCode='11211' OR ZipCode='11249') ORDER BY SaleDate"

wburgpricemonthquery <- "SELECT DATE_FORMAT(SaleDate, '%Y-%m-01') AS SaleDate, AVG(SalePrice / GrossSquareFeet) AS ppsqft FROM sales WHERE SalePrice / GrossSquareFeet > 30 AND GrossSquareFeet > 100 AND SalePrice > 5000 AND TotalUnits > 0 AND NOT Address = 0 AND (ZipCode='11249' OR ZipCode='11211') GROUP BY 1 ORDER BY 1"

query2df <- function(query) {
  ret <- dbGetQuery(conn, query)
  ret$SaleDate <- as.Date(ret$SaleDate, "%Y-%m-%d")
  ret
}

dayprices <- query2df(daypricesquery)
wburgprices <- query2df(wburgpricequery)
wburgmonth <- query2df(wburgpricemonthquery)

library(forecast)
head(wburgmonth)

wburg.fit.aic <- auto.arima(as.ts(log(wburgmonth$ppsqft)), ic="aic", seasonal=F)
wburg.fit.bic <- auto.arima(as.ts(log(wburgmonth$ppsqft)), ic="bic", seasonal=F)

nrow(wburgmonth)

plot(forecast(wburg.fit.aic, h=12))
plot(forecast(wburg.fit.bic, h=12))

seq(min(wburgprices), max(wburgprices), "min")

rollingsummary <- function(timeseries, windowsize) {
  ret <- zoo(, seq(min(time(timeseries), max(time(timeseries)), "min"))
  curtime <- time(timeseries[1])
  final <- time(tail(wburg.zoo, 1))
  cursor <- 1
  for (
  while (curtime + windowsize <= final) {
    vals <- c()
    cursor2 <- cursor
    while (time(timeseries[cursor]) < curtime+windowsize) {
      vals <- c(vals, timeseries[cursor])
      cursor2 <- cursor2 + 1
    }
    zoo[time(timeseries[cursor]) + windowsize-1] <- median(vals)
    newtime <- curtime
    while newtime == curtime {
      cursor <- cursor + 1 
      newtime <- time(timeser
    }
  }
}

wburg.zoo <- read.zoo(wburgprices[,c("SaleDate", "ppsqft")], index.column="SaleDate")
plot(rollmedian(wburg.zoo, 31))

rollmedianr(wburg.zoo, 3, na.pad=T)
auto.arima(rollmedian(wburg.zoo, 4))
zooreg(wburg.zoo)
wbrg.decomp <- decompose(as.ts(wburg.zoo))

library(xts)
library(TTR)
period.apply

x <- .xts(wburgprices$ppsqft, wburgprices$SaleDate)
xx <- na.locf(cbind(xts(, seq.POSIXt(from=start(x), to=end(x), by='day')), x))
plot(rollmedian(xx, k=7))

dayprice_medians <- ddply(allprices, .(SaleDate), summarise, price=median(ppsqft))
wburgprices$yearmonth <- as.yearmon(wburgprices$SaleDate)
wburg_medians <- ddply(wburgprices, .(yearmonth ), summarise, price=median(ppsqft, na.rm=T), tenperc=quantile(ppsqft, .10, na.rm=T), ninetyperc=quantile(ppsqft, .90, na.rm=T))


wbrgts <- ts(log(wburg_medians$price), start=2003, frequency=12)
wbrg.decomp <- decompose(wbrgts)
wbrg.stl <- stl(wbrgts, s.window="periodic")


plot(price ~ yearmonth, data=wburg_medians, ylim=c(0,1500))
ggplot(wburg_medians) + geom_line(aes(y=ppsqft, x=SaleDate))

library(forecast)

fit <- ets(wbrgts)
fit <- auto.arima(wbrgts)

