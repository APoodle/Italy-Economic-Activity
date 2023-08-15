#Writing Assignment 2 Code

library(tseries)
library(vars)

#GDP data and IR data

gdp = read.table('Italy_GDP.csv', header=T, sep=',')
rgdp = ts(gdp[,2],start=1996, frequency=4)
dlogrgdp = 400*diff(log(rgdp))

ir = read.table('Italy_IR.csv', header=T, sep=',')
ir = ts(ir[,2], start=1978, frequency=12)
ir =aggregate(ir,nfrequency=4,mean)

gdp = window(dlogrgdp, start=c(1996,01), end=c(2020,1))
ir = window(ir, start=c(1996,01),end=c(2020,1))

plot(gdp,col="black",lwd=2, main='Real GDP Growth and 3 Month Interbank Interest Rate')
lines(ir,col="blue",lwd=2)
legend('bottomleft', legend=c("Real GDP Growth", "3 Month Interbank Interest Rate"),
       col=c("black", "blue"), lty=1:1, cex=1)

#VAR analysis

gdpir<-ts(cbind(gdp, ir))
gdpir<-na.omit(gdpir)
acf(gdpir)
ccf(gdp, ur)
pacf(gdpir)
VARselect(gdpir, lag.max=16, type='const')

vargdpir = VAR(gdpir, p=2)
summary(vargdpir)

irfgdpir<-irf(vargdpir, ortho=TRUE, ci=0.95, runs=100, n.ahead=16)
plot(irfgdpir, lwd=2)


