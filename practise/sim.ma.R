## Generate simulated examples of MA(q) processes

n = 100

par(mfrow=c(4,2), mar=c(4,4,3,0)+0.2)

ma0 = ts(rnorm(n=n))
plot(ma0, ylab="Xt", main="MA(0), white noise"); acf(ma0, main="")
ma1 = arima.sim(n=n, model=list(ma=c(1)))
plot(ma1, ylab="Xt", main="MA(1)"); acf(ma1, main="")
ma3 = arima.sim(n=n, model=list(ma=c(1, .9,.8)))
plot(ma3, ylab="Xt", main="MA(3)"); acf(ma3, main="")
ma10 = arima.sim(n=n, model=list(ma=c(1, .9,.8,.7,.6,.5,.4,.3,.2,.1)))
plot(ma10, ylab="Xt", main="MA(10)"); acf(ma10, main="")

## Now play with (i) repeated realisation to see the natural variation
## in MA data (ii) experimenting with different values of n (iii)
## experimenting with different values of q and beta.
