# 时间序列课后
#Chapter 1
library(astsa)
#1.2
plot(EQ5,col = "blue", lty = "dashed")
lines(EXP6,col = "red")
dev.off()
#1.3
set.seed(2012310894)
n = 150
w = rnorm(n)
x = filter(w, filter = c(0,-.9), method = "recursive")[-(1:50)]
v = filter(x,rep(1/4,4),side = 1)
plot.ts(x)
lines(v,lty = "dashed",col = "red")

t = 1:100
x = 2*cos(2*pi*t/4) + w[-(1:50)]
v = filter(x,rep(1/4,4),side = 1)
plot.ts(x)
lines(v,lty = "dashed",col = "red")

x = jj
v = filter(x,rep(1/4,4),side = 1)
plot.ts(x)
lines(v,lty = "dashed",col = "red")

#1.6
n = 1000
w = rnorm(n)
v = filter(w,c(1,2,1),side = 2)
plot.ts(v)
v = v[c(-1,-length(v))]
acf(v)
v

#1.18
w = rnorm(500,0,1) # 50 extra to avoid startup problems
x = filter(w, filter=c(1,-.9), method="recursive")
plot.ts(x, main="autoregression")
acf(x,lag = 50)
pacf(x,lag = 100)

ar2=arima.sim(n = 500,list( order=c(2,0,0),ar = c(1, -0.5)))
# mildly long-tailed
plot.ts(ar2)
acf(ar2)


#Chapter 2
library(astsa)
#2.1
xt = log(jj)
trend = time(jj) - 1970 # helps to `center' time
Q = factor(cycle(jj)) # make (Q)uarter factors
reg = lm(log(jj)~ 0 + trend + Q, na.action=NULL) # no intercept
model.matrix(reg) # view the model matrix
summary(reg) 

reg2 = lm(log(jj)~ trend + Q, na.action=NULL)
summary(reg2)

plot.ts(xt,ylab = expression(paste(x[t],(hat(x[t])))))
lines(ts(predict(reg),start = 1960,freq = 4),col = "red",lty = 2)
legend("topleft",lty = c(1,2),col = c(1,2),legend = expression(x[t],hat(x[t])))
plot(reg$resi,ylab = expression(x[t]-hat(x[t])))
title(expression(paste("Plot of Residuals",(x[t]-hat(x[t])))))
acf(reg$resi,lag = 200)
# plot.ts(jj,ylab = "jj (jjhat)")
# jjhat = ts(exp(predict(reg)),start = 1960,fre = 4)
# lines(jjhat,col = "red",lty = 2)
# legend("topleft",lty = c(1,2),col = c(1,2),legend = c("jj","jjhat"))

#2.2
temp = tempr-mean(tempr) 
temp2 = temp^2
trend = time(cmort)
dat = ts.intersect(cmort,trend,temp,temp2,part,lag(part,-4))
fit = lm(cmort~.,data = dat,na.action=NULL)
summary(fit)
num = length(cmort) 
AIC(fit)/num - log(2*pi) 
BIC(fit)/num - log(2*pi) 

#2.6
plot(varve)
var(varve[1:(length(varve)/2)])
var(varve[(length(varve)/2):length(varve)])

yt = log(varve)
var(yt[1:(length(yt)/2)])
var(yt[(length(yt)/2):length(yt)])
plot(yt)

par(mfrow = c(2,1))
hist(varve,freq= F)
hist(yt,freq = F)
dev.off()

plot.ts(yt)

acf(yt)

ut = filter(yt,c(1,-1),side = 1)
ut = ut[c(-1,-length(ut))]
acf(ut)

#2.7
plot(gtemp)
gtemp10= filter(gtemp, sides = 2, rep(1/10,10))
gtemp30= filter(gtemp, sides = 2, rep(1/30,30))
lines(gtemp10,col= "red")
lines(gtemp30,col= "blue",lty = "dashed")
legend("topleft",legend = c("k=5","k=15"),lty = c(1,2),col = c(2,4))

plot(gtemp)
lines(ksmooth(time(gtemp),gtemp,"normal",bandwidth = 10),col = "red")
lines(ksmooth(time(gtemp),gtemp,"normal",bandwidth = 30),col = "blue",lty = "dashed")
legend("topleft",legend = c("bandwidth = 20","bandwidth = 30"),
       lty = c(1,2),col = c(2,4))

plot(gtemp)
lines(lowess(gtemp),col = "red")
lines(lowess(gtemp,f = .30),col = "blue",lty = "dashed")
legend("topleft",legend = c("66.7%","30%"),lty = c(1,2),col = c(2,4))

cov = NULL
for(i in 1:83){
  cov = c(cov,cov(re[1:(84-i)],re[(i+1):84]))
}

sd = NULL
for(i in 1:84){
  sd = c(sd,sd(sample(re,40,replace = F)))
}
sd
