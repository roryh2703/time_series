n = 1000 # length of series


######################################################################
## Generate simulated examples of MA(q) processes
par(mfrow=c(3,3), mar=c(1,1,3.5,1), cex.main=2) # three-by-three grid of plots

## Look at acf and pacf for MA(q) process as we change q = 0,3,10
ma0 = ts(rnorm(n=n))
ma3 = arima.sim(n=n, model=list(ma=c(1, .9,.8)))
ma10 = arima.sim(n=n, model=list(ma=c(1, .9,.8,.7,.6,.5,.4,.3,.2,.1)))

plot(ma0, main="MA(0)"); acf(ma0, main="ACF"); pacf(ma0, main="PACF")
plot(ma3, main="MA(3)"); acf(ma3, main=""); pacf(ma3, main="")
plot(ma10, main="MA10"); acf(ma10, main=""); pacf(ma10, main="")
#

######################################################################
## Next, consider non-stationary AR(1), alpha = 1

plot(0,0,xlim=c(0,100), ylim=c(-30, 30), xlab="time t",
     ylab=expression(X[t]), type="n", main="AR(1), alpha = 1")
for(i in 1:100){
  eps = rnorm(n=n, mean=0, sd=1) # white noise
  ar1 = ts(cumsum(c(0, eps))) # not stationary
  lines(ar1)
}
acf(ar1, main="ACF")
pacf(ar1, main="PACF")

## Now consider stationary AR(1) with different values of alpha
for(alpha in c(0.99, 0.5)){
  ar.process = arima.sim(n=n, model=list(ar=alpha))
  plot(ar.process, ylab=expression(X[t]),
       main=paste("AR(1), alpha =", alpha))
  acf(ar.process, main="")
  pacf(ar.process, main="")
}


######################################################################
## Next increase AR order, p
alpha = c(0.9, -0.8, 0.7)

ar.process = arima.sim(n=n, model=list(ar=alpha[1]))
plot(ar.process, ylab=expression(X[t]), main="AR(p), p = 1")
acf(ar.process, main="ACF")
pacf(ar.process, main="PACF")

for(p in 2:3){
  ar.process = arima.sim(n=n, model=list(ar=alpha[1:p]))
  plot(ar.process, ylab=expression(X[t]), main=paste("AR(p), p =", p))
  acf(ar.process, main="")
  pacf(ar.process, main="")
}


######################################################################
## Finally consider an ARIMA(p,d,q) process

par(mfrow=c(2,3))

## Firstly, a reminder of what AR(1) and MA(1) look like
ma1 = arima.sim(n=n, model=list(ma=0.9))
plot(ma1, ylab=expression(X[t]), main=paste("MA(1)"))
acf(ma1, main="ACF"); pacf(ma1, main="PACF")
ar1 = arima.sim(n=n, model=list(ar=0.9))
plot(ar1, ylab=expression(X[t]), main=paste("AR(1)"))
acf(ar1, main=""); pacf(ar1, main="")


## Now an ARIMA(1,1,0) model and its difference
X1 = arima.sim(n=n, model=list(order=c(1,1,0), ar=c(0.9)))
plot(X1, ylab=expression(X[t]), main=paste("ARIMA(1,1,0)"))
acf(X1, main="ACF")
pacf(X1, main="PACF")

## Very slow decay of ACF suggests differencing

Y1 = diff(X1)
plot(Y1, ylab=expression(nabla * X[t]),
     main=paste("diff ARIMA(1,1,0): ARMA(1,0)"))
acf(Y1, main="")
pacf(Y1, main="")



## ACF still has exponential decay, while pacf looks like AR(1).


## Now an ARIMA(0,1,1)

X2 = arima.sim(n=n, model=list(order=c(0,1,1), ma=c(0.9)))
plot(X2, ylab=expression(X[t]), main=paste("ARIMA(0,1,1)"))
acf(X2, main="ACF")
pacf(X2, main="PACF")

## Very slow decay of ACF suggests differencing

Y2 = diff(X2)
plot(Y2, ylab=expression(nabla * X[t]),
     main=paste("diff ARIMA(0,1,1): ARMA(0,1)"))
acf(Y2, main="")
pacf(Y2, main="")



## Lastly, an ARIMA(1,1,1)


X3 = arima.sim(n=n, model=list(order=c(1,1,1), ar=c(0.9), ma=c(0.9)))
plot(X3, ylab=expression(X[t]), main=paste("ARIMA(1,1,1)"))
acf(X3, main="ACF")
pacf(X3, main="PACF")

## Very slow decay of ACF suggests differencing

Y3 = diff(X3)
plot(Y3, ylab=expression(nabla * X[t]),
     main=paste("diff ARIMA(1,1,1): ARMA(1,1)"))
acf(Y3, main="")
pacf(Y3, main="")








