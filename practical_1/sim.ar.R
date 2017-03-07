## Generate simulated examples of MA(q) processes

n = 100

## First, consider non-stationary AR(1), alpha = 1
plot(0,0,xlim=c(0,100), ylim=c(-30, 30), xlab="Time",  ylab="Xt", type="n",
     main="AR(1), alpha = 1")
for(i in 1:100){
  eps = rnorm(n=n, mean=0, sd=1) # white noise
  ar1 = ts(cumsum(c(0, eps))) # not stationary
  lines(ar1)
}

## Now consider AR(1) with different values of alpha = 0.99, 0.5, 0.01

par(mfcol=c(2,3))

for(alpha in c(0.99, 0.5, 0.01)){
  ar.process = arima.sim(n=n, model=list(ar=alpha))
  plot(ar.process, ylab="Xt", main=paste("AR(1), alpha =", alpha))
  acf(ar.process, main="")
}

## Finally increase order, p

## Define alpha_k for k=1,...,9
alpha = ((9:1)/10)^6
alpha

## Now use the first one only for an AR(1), then the first 5 for an
## AR(5) and all for an AR(9).

for(p in c(1,5,9)){
  ar.process = arima.sim(n=n, model=list(ar=alpha[1:p]))
  plot(ar.process, ylab="Xt", main=paste("AR(p), p =", p))
  acf(ar.process, main="")
}
