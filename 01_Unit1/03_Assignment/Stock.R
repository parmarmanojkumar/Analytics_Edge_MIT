IBM = read.csv("IBMStock.csv")
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")

GE = read.csv("GEStock.csv")
GE$Date = as.Date(GE$Date, "%m/%d/%y")

CocaCola = read.csv("CocaColaStock.csv")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")

ProcterGamble = read.csv("ProcterGambleStock.csv")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")

Boeing = read.csv("BoeingStock.csv")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

plot(CocaCola$Date, CocaCola$StockPrice, type='l', col = "red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice)
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1983-01-01")), lwd=2)
abline(v=as.Date(c("1984-01-01")), lwd=2)
IBM$Month = months(IBM$Date)
CocaCola$Month = months(CocaCola$Date)

tapply(IBM$StockPrice,IBM$Month,mean) > mean(IBM$StockPrice)

which.max(tapply(GE$StockPrice,months(GE$Date),mean))
which.max(tapply(CocaCola$StockPrice,months(CocaCola$Date),mean))

