#3.4
library(astsa)
z = c(1,0,.9)
a = polyroot(z)[1]
arg = Arg(a)/(2*pi)
1/arg
x = arima.sim(list(order=c(2,0,0),ar=c(0,-0.9)),n=144)
plot(x,axes = F,xlab = "time")
axis(2);axis(1,at = seq(0,144,by=4));box()
abline(v = seq(0,144,by = 4),lty=2)
ACF = ARMAacf(ar = c(0,-.9),ma=0,50)
plot(ACF,type = "h")

#3.5
ACF1 = ARMAacf(ar = .6,ma=0,24)
PACF1 = ARMAacf(ar = .6,ma=0,24,pacf = T)
ACF2 = ARMAacf(ar = 0,ma=.9,24)
PACF2 = ARMAacf(ar = 0,ma=.9,24,pacf = T)
ACF3 = ARMAacf(ar = .6,ma=.9,24)
PACF3 = ARMAacf(ar = .6,ma=.9,24,pacf = T)

par(mfrow = c(1,2))
plot(ACF1,type = "h")
plot(PACF1,type = "h")
plot(ACF2,type = "h")
plot(PACF2,type = "h")
plot(ACF3,type = "h")
plot(PACF3,type = "h")

x = arima.sim(list(order=c(1,0,0),ar=.6),n=100)
acf2(x)
y = arima.sim(list(order=c(0,0,1),ma=.9),n=100)
acf2(y)
z = arima.sim(list(order=c(1,0,1),ar=.6,ma=.9),n=100)
acf2(z)

x2 = arima.sim(list(order=c(1,0,0),ar=.6),n=500)
acf2(x2)
y2 = arima.sim(list(order=c(0,0,1),ma=.9),n=500)
acf2(y2)
z2 = arima.sim(list(order=c(1,0,1),ar=.6,ma=.9),n=500)
acf2(z2)

#3.14
plot(gtemp)
dg =diff(gtemp)
acf2(dg,50)
sarima(gtemp,1,1,1)
sarima.for(gtemp,10,1,1,1)

#3.16
plot(sales)
l = log(sales)
acf2(l)
dl = diff(l)
acf2(dl)
sarima(l,0,1,4)
sarima(l,2,1,0)

lag2.plot(diff(lead),diff(sales),3)

d = ts.intersect(diff(sales),lag(diff(lead),-3))
y =d[,1];x=d[,2]
fit = lm(y~x,na.action=NULL)
summary(fit)
acf2(resid(fit),100)
sarima(resid(fit),1,0,0)

sarima(y,1,0,0,xreg=x)

#3.20
library(astsa)
x=unemp
lx = log(x); dlx = diff(lx); ddlx = diff(dlx, 12)
plot.ts(cbind(x,lx,dlx,ddlx),main="")
dev.new()
par(mfrow=c(2,1), mar=c(3,3,1,1), mgp=c(1.6,.6,0))
monthplot(dlx); monthplot(ddlx)
acf2(ddlx,50)
sarima(lx, 1,1,1, 0,1,1, 12)
sarima.for(lx,12,1,1 ,1 ,0 ,1 ,1 ,12)
