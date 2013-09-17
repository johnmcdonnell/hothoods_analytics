
# Zip code irregularities
prices[ZipCode==11249, ZipCode:=11211]

prices[ZipCode==10075, ZipCode:=10021]
prices[ZipCode==10065, ZipCode:=10021]

# Price per sq. foot
prices[,ppsqft:=SalePrice/GrossSquareFeet]

# Date fixing
prices[,as.Date(SaleDate, "%Y-%m-%d")]
prices[,SaleDate:=as.Date(SaleDate, "%Y-%m-%d")]
prices[,MonthYear:=format(SaleDate, "%Y-%m")]

# Monthly series
prices.monthly.wholecity <- prices[,list(ppsqft=median(SalePrice/GrossSquareFeet )), by=list(MonthYear)]
prices.monthly <- prices[,list(ppsqft=median(SalePrice/GrossSquareFeet )), by=list(MonthYear, ZipCode)]
prices.monthly[, list(ppsqft=ppsqft), by=ZipCode]
prices.monthly[length(ppsqft)==120,ma(ppsqft, order=12, centre=T), by=ZipCode]

