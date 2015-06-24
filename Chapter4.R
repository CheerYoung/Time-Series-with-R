library(astsa)
#4.1
n = 120
x1 = 2*cos(2*pi*1:n*.06) + 3*sin(2*pi*1:n*.06)
x2 = 4*cos(2*pi*1:n*.1) + 5*sin(2*pi*1:n*.1)
x3 = 6*cos(2*pi*1:n*.4) + 7*sin(2*pi*1:n*.4)
x = x1 + x2 + x3
par(mfrow=c(2,2))
plot.ts(x1, ylim=c(-10,10), main=expression(omega==.06~~~A^2==13))
plot.ts(x2, ylim=c(-10,10), main=expression(omega==.1~~~A^2==41))
plot.ts(x3, ylim=c(-10,10), main=expression(omega==.4~~~A^2==85))
plot.ts(x, ylim=c(-16,16), main="sum")

dev.off()
P = abs(2*fft(x)/100)^2; Fr = seq(0,100,length=n)
plot(Fr, P, type="o", xlab="frequency", ylab="periodogram")

n = 100
x1 = 2*cos(2*pi*1:n*.06) + 3*sin(2*pi*1:n*.06)
x2 = 4*cos(2*pi*1:n*.1) + 5*sin(2*pi*1:n*.1)
x3 = 6*cos(2*pi*1:n*.4) + 7*sin(2*pi*1:n*.4)
set.seed(1)
xNew = x1 + x2 + x3 + rnorm(100,0,5)
plot.ts(xNew, ylim=c(-22,25), main="sum")
P = abs(2*fft(xNew)/100)^2; Fr = 0:99/100
plot(Fr, P, type="o", xlab="frequency", ylab="periodogram")

#4.6
sun.per = mvspec(sunspotz,log="no")
abline(v=1/11, lty="dotted")

n = length(sunspotz)
par(mfrow=c(2,1), mar=c(3,3,1,1), mgp=c(1.6,.6,0))
plot(sunspotz, ylab="sunspot", xlab="")
Per = Mod(fft(sunspotz-mean(sunspotz)))^2/n
Freq = ((1:n-1)*2)/n  #some necessary adjustment for the biyearly data
plot(Freq[1:50], Per[1:50], type='h', lwd=3, ylab="Periodogram", xlab="Frequency")
(u = which.max(Per[1:50])) # 22 freq=(21*2)/459=0.09 cycles/day——1/11
(uu = which.max(Per[1:50][-u])) # 23 freq=(23*2)/459=0.1 cycles/day——1/10
1/Freq[21]; 1/Freq[23] # period = days/cycle
text(.06, 60000, "11 years cycle"); text(.13, 60000, "10 years cycle")

sun.per0 = mvspec(sunspotz)
sun.per$spec[42] # 1195.547
#sunspot pgram at freq 1/11 = 42/459

# conf intervals - returned value:
U = qchisq(.025,2) # 0.05063
L = qchisq(.975,2) # 7.37775
2*sun.per$spec[42]/L # 324.0948
2*sun.per$spec[42]/U # 47221.57


U = qchisq(.025,2) # 0.05063
L = qchisq(.975,2) # 7.37775
2*sun.per$spec[40]/L # 
2*sun.per$spec[40]/U # 
 plot((soi-mean(soi)))

spaic = spec.ar(sunspotz, log="no") # min AIC spec
abline(v=frequency(sunspotz)*1/52, lty="dotted") # El Nino Cycle
(sun.ar = ar(sunspotz, order.max=30)) # estimates and AICs

plot(1:30, sun.ar$aic[-1], type="o")

n = length(sunspotz)
AIC = rep(0, 30) -> BIC
for (k in 1:30){
  sigma2 = ar(sunspotz, order=k, aic=FALSE)$var.pred
  BIC[k] = log(sigma2) + (k*log(n)/n)
  AIC[k] = log(sigma2) + ((n+2*k)/n)
}
IC = cbind(AIC,BIC+1)
ts.plot(IC, type="o", xlab="p", ylab="AIC / BIC")



#4.11
spe = mvspec(speech)
spee = mvspec(speech,log="no")
spee$fr[which.max(spee$sp)]


#4.18
spaic = spec.ar(sunspotz,log="no") #min AIC spec
abline(v=1/11, lty="dotted") # Sunspot Cycle
(sun.ar = ar(sunspotz, order.max=30)) # estimates and AICs
dev.new()
plot(1:30, sun.ar$aic[-1], type="o") # plot AICs

n = length(sunspotz)
AIC = rep(0, 30) -> AICc -> BIC
for (k in 1:30){
  sigma2 = ar(sunspotz, order=k, aic=FALSE)$var.pred
  BIC[k] = log(sigma2) + (k*log(n)/n)
  AICc[k] = log(sigma2) + ((n+k)/(n-k-2))
  AIC[k] = log(sigma2) + ((n+2*k)/n)
}
IC = cbind(AIC, BIC+1)
ts.plot(IC, type="o", xlab="p", ylab="AIC / BIC")  # plot AICs and BICs

