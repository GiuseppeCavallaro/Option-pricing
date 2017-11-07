library(fOptions)
library(quantmod)
library(FinTS)
library(stats)
library(sde)
library(graphics)
getSymbols("GS", from="2015-12-17", to="2016-12-15")
GS<- GS$GS.Close

plot(GS, main="Goldman Sachs")
plot(log(GS))
#log return
Xgs<- na.omit(diff(log(GS)))

mean.gs<- mean(Xgs)
sd.gs<- sd(Xgs)
f2<- function(x) dnorm(x, mean = mean.gs, sd= sd.gs)
plot(density(na.omit(Xgs)), main="Density Goldman Sachs returns")
lines(density(Xgs), col="red", lwd=3)
curve(f2, min(Xgs), max(Xgs), add=TRUE, col="blue", lwd=3, lty=3)

qqnorm(as.numeric(Xgs), main="Q-Q Goldman Sachs"); qqline(Xgs, col='red')

shapiro.test(as.numeric(Xgs))#null hypothesis normal distributed
jarqueberaTest(as.numeric(Xgs))
ks.test(as.numeric(Xgs), "pnorm")



#Investigate the indipendence of increments for Goldman Sachs

Box.test(na.omit(Xgs), lag = 1, type = c("Box-Pierce"))
Box.test(na.omit(Xgs), lag = 1, type = c("Ljung-Box"))
Box.test(na.omit(Xgs), lag = 5, type = c("Box-Pierce"))
Box.test(na.omit(Xgs), lag = 5, type = c("Ljung-Box"))

Box.test(na.omit(Xgs)^2, lag = 1, type = c("Box-Pierce"))
Box.test(na.omit(Xgs)^2, lag = 1, type = c("Ljung-Box"))
Box.test(na.omit(Xgs)^2, lag = 5, type = c("Box-Pierce"))
Box.test(na.omit(Xgs)^2, lag = 5, type = c("Ljung-Box"))


acf(na.omit(Xgs), main="Autocorrelation Goldman Sachs")
acf(na.omit(Xgs)^2, main = "Autocorrelation Goldman Sachs returns^2")

#Volatility changing point
cp<- cpoint(as.numeric(Xgs))
cp
time(as.numeric(Xgs))[cp$k0]
returns <- as.numeric(Xgs)
plot(returns, type = "l", main="Volatility changing point", xlab="days")
abline(v= time(as.numeric(Xgs))[cp$k0]  , lty=2, col= "red")

library(bizdays)
set.seed(123)


r<- 0.0081 # the risk free rate is 0.8%
S0<- as.numeric(GS[252,])
S0
bizdays("2016-12-15","2016-12-30")
T <- 15*Delta
sigma2.gs<-var(Xgs,na.rm=TRUE)/Delta
sigma.gs<- as.numeric(sqrt(sigma2.gs)) #historical volatility

#Black Scholes Option Valuation call
GBSOption("c", S=S0, X=K, Time = T, r=r, b=r, sigma = sigma.gs)

K<- 220 #K is the strike price
p.call<-24.15
f<- function(x) GBSOption("c", S=S0, X=K, Time=T, r=r, b=r, sigma=x)@price #prezzo
f(sigma.gs)#prezzo teorico
curve(f, 0.05, 0.5 )
abline(h=f(sigma.gs), lty=3, col="blue")
sigma.impl.call<-GBSVolatility(p.call, "c", S=S0, X=K, Time=T, r=r, b=r)# sigma implied by p.call
abline(v= sigma.impl.call, col="red")
abline(v=sigma.gs, col="blue")
f(sigma.impl.call)
abline(h=p.call, lty=3, col="red")
 
#compare implied volatility with  historical volatility 

#put price
K1<-230
p.put<-1.24
f1<- function(x) GBSOption("p", S=S0, X=K1, Time=T, r=r, b=r, sigma=x)@price
f1(sigma.gs)
curve(f1, 0.05, 0.6)
abline(h=f1(sigma.gs), lty=3, col="blue")
sigma.impl.put<-GBSVolatility(p.put, "p", S=S0, X=K1, Time=T, r=r, b=r)
f1(sigma.impl.put)
abline(h=f1(sigma.gs), lty=3, col="blue")
abline(v=sigma.impl.put, col="red")
abline(h=p.put, lty=3, col="red")
abline(v=sigma.gs, col="blue")

# Volatility smiles call- goldman sachs
P.call <- c(33.45, 39.65, 55.55, 50.6, 43.45, 39, 34, 30.3, 24.15, 22.9, 20.1, 17, 15.5, 12.6, 10.6, 8.7, 7, 4.3, 2.45, 1.3, 0.7, 0.4)
K<- c(180, 185, 190, 195, 200, 205, 210, 215, 220, 222.5, 225, 227.5, 230,  232.5,  235, 237.5, 240, 245, 250, 255, 260, 265)
nP<- length(P.call)
smile<- sapply(1:nP, function(i) GBSVolatility(P.call[i],"c", S=S0, X=K[i], Time=T, r=r, b=r))
vals<- c(smile, sigma.gs)
plot(K, smile, type= "l", ylim = c(min(vals, na.rm = TRUE), max(vals, na.rm = TRUE)), main = "Volatility smile Call")
abline(v=S0, lty=3, col="red")
abline(h=sig.go, lty=3, col="red")

#Volatility smiles put- goldman sachs
P.put<- c(1.8, 2.05, 4.3, 2.55, 2.85, 3.2, 3.6, 33.8, 4.6, 5.2, 5.9, 6.7, 7.5, 9.5, 10.6, 11.8, 14.7, 16.3, 17.9, 30.8, 43.8, 110.5)
K1<- c(192.5, 195, 197.5, 200, 202.5, 205, 207.5, 210, 212.5, 215, 217.5, 220, 222.5, 225, 227.5, 230, 232.5, 235, 237.5, 240, 245, 250)
nP1<- length(Pt1)
smile1<- sapply(1:nP1, function(i) GBSVolatility(P.put[i],"c", S=S0, X=K1[i], Time=T, r=r, b=r))
vals1<- c(smile1, sig.go)
plot(K1, smile1, type="l", ylim= c(min(vals1, na.rm = TRUE), max(vals1, na.rm = TRUE)), main = "Volatility smile Put")
abline(v=S0, lty=3, col= "red")
abline(h=sig.go, lty=3, col="red")

library(yuima)
set.seed(123)
#naive estimation of GBM
Delta<- 1/252
alpha.gs<- mean(Xgs, na.rm = TRUE)/Delta
sigma.gs<- sqrt(var(Xgs, na.rm = TRUE)/Delta)
mu.gs<- alpha.gs+0.5*sigma.gs^2
set.seed(123)
n<-100
S0<- as.numeric(GS[252])
S<- sde.sim(t0=0, T=15*Delta, X0=S0, N=n, delta=Delta, model = "BS", theta= c(mu.gs, sigma.gs))
plot(S, main= "naive-gBm-GS")
abline(h=S0, lty=3, col="red")

#GBM for Goldman Sachs
gbm<- setModel("mu*x", diff="sig*x")
gs.gbm<- setYuima(model= gbm, data=setData(GS, delta = 1/252))
fit.gs.gbm<- qmle(gs.gbm, start= list(mu=mu.gs , sig=sigma.gs))
summary(fit.gs.gbm)
AIC(fit.gs.gbm)

toLatex(gbm)

#CIR
cir<- setModel(drift = "alpha*(mu-x)",diff="sig*sqrt(x)")
#for GS
gs.cir<- setYuima(model = cir, data = setData(GS, delta = 1/252 ))
fit.gs.cir<- qmle(gs.cir, start = list(alpha=1, mu= mu.gs, sig= 0.2))
fit.gs.cir<- qmle(gs.cir, start = list(alpha=1, mu= mean(GS), sig= 0.2))

summary(fit.gs.cir)
AIC(fit.gs.cir)

toLatex(cir)

#cir2
cir2<- setModel("mu*x",diff="sig*sqrt(x)")

#for GS
gs.cir2<- setYuima(model = cir2, data = setData(GS, delta = 1/252))
fit.gs.cir2<- qmle(gs.cir2, start = list(mu= 1, sig= 0.2))
summary(fit.gs.cir2)
AIC(fit.gs.cir2)

toLatex(cir2)

#ckls
ckls<- setModel(drift = "mu*x",diff="sig*x^gamma")
# for GS

lower<- list(mu=0.01, sig=0.01, gamma=0.01)
upper<- list(mu=10, sig=300, gamma=1.9)
gs.ckls<- setYuima(model = ckls, data = setData(GS, delta = 1/252))
fit.gs.ckls<- qmle(gs.ckls, start = list(mu=1, sig= 20, gamma=0.5), upper = upper, lower = lower, method = "L-BFGS-B")
summary(fit.gs.ckls)
AIC(fit.gs.ckls)

toLatex(ckls)


#Vasicek model
vas<- setModel(drift = "alpha*(mu-x)", diffusion = "sig")
gs.vas<- setYuima(model = vas, data = setData(GS, delta = 1/252))
fit.gs.vas<- qmle(gs.vas, start = list(alpha=0.01, mu=0.1, sig= 0.2))
summary(fit.gs.vas)
AIC(fit.gs.vas)
#
toLatex(vas)

MCPayoff<- function(mod,true.par,f,T,t0=0,r,x0=1,M=100,delta=1/252){set.seed(123)
  
  samp<- setSampling(Initial=t0, n=T/delta, delta=delta)
  
  p<- NULL
  
  for(i in 1:M){
    
    sim<- simulate(mod, true.par=true.par ,sampling=samp, xinit = x0)
    plot(sim)
    tmp<- as.numeric(get.zoo.data(sim)[[1]])
    p<- c(p, f(tmp))
  } 
  mean(p, na.rm=TRUE)*exp(-r *T )
  
}


f.call<- function(x){
  max(x[length(x)]-K,0)
}

f.put<- function(x){
  max(K1-x[length(x)],0)
}






#payoff function

K<- 220
K1<- 230
Delta<- 1/252

T<- Delta*15
r=0.0081
x0<- as.numeric(GS[length(GS)])


gbm.par<-as.list(coef(fit.gs.gbm))
cir.par<-as.list(coef(fit.gs.cir))
cir2.par<-as.list(coef(fit.gs.cir2))
ckls.par<-as.list(coef(fit.gs.ckls))

MCPayoff(mod=gbm,true.par=gbm.par, f=f.call, T=T, t0=0, r=r, x0=x0, M=100)
MCPayoff(mod=cir,true.par = cir.par, f=f.call, T=T, t0=0, r=r, x0=x0, M=100)
MCPayoff(mod=cir2,true.par=cir2.par, f=f.call, T=T, t0=0, r=r, x0=x0, M=100)
MCPayoff(mod=ckls, true.par = ckls.par, f=f.call, T=T, t0=0, r=r, x0=x0, M=100)

MCPayoff(mod=gbm,true.par=gbm.par, f=f.put, T=t, t0=0, r=r, x0=x0, M=100)
MCPayoff(mod=cir,true.par = cir.par, f=f.put, T=T, t0=0, r=r, x0=x0, M=100)
MCPayoff(mod=cir2,true.par=cir2.par, f=f.put, T=T, t0=0, r=r, x0=x0, M=100)
MCPayoff(mod=ckls, true.par = ckls.par, f=f.put, T=T, t0=0, x0=x0, r=r, M=100)
