## Exploration of airline passengers data, frequently analysed in the
## literature; see, eg, Cowpertwaite & Metcalfe pp4

## Monthly airline bookings (in 1000s) from Pan Am in USA from 1949-1960
data(AirPassengers)
ap = AirPassengers
ap

class(ap) # a "time series" object in R

## Characteristics of a time series
start(ap)      # start date
end(ap)        # end date
frequency(ap)  # number of observations per "main time unit" (ie year)

## Time series plot
plot(ap, ylab="Airline bookings (1000s)")


################################################

## Fit a linear trend
tt = 1:length(ap) # don't use "t" as it is a function in R!
fit1 = lm(ap~tt)

trend1 = fitted(fit1) # trend as a vector; convert it to a "ts" object
trend1 = ts(trend1, start=start(ap), end=end(ap), frequency=frequency(ap))

## Superimpose on time series plot
lines(trend1, col="blue")


## Find and plot residuals
resid1 = ts(residuals(fit1), start=start(ap), end=end(ap), 
            frequency=frequency(ap))

plot(resid1, ylab="Residuals")

#####################################################

## Observe (1) some trend remaining (2) cyclic pattern (3) increasing
## variance.  Clearly, our simplistic linear trend was not sufficient.


## Consider using a non-linear trend (in this case, quadratic).
tt2 = tt^2
fit2 = lm(ap ~ tt + tt2)
trend2 = fitted(fit2)
trend2 = ts(trend2, start=start(ap), end=end(ap), frequency=frequency(ap))

resid2 = ts(residuals(fit2), start=start(ap), end=end(ap), 
            frequency=frequency(ap))
plot(resid2)

###############################################

## No remaining evidence of a trend. 
## Next, try to deal with the non-constant variance by taking log(X)

log.ap = log(ap)
fit3 = lm(log.ap ~ tt + tt2)

trend3 = ts(fitted(fit3), start=start(ap), end=end(ap), frequency=frequency(ap))
resid3 = ts(residuals(fit3), start=start(ap), end=end(ap), 
            frequency=frequency(ap))
par(mfrow=c(2,1))
plot(log(ap), ylab="log Airline bookings (1000s)")
lines(trend3, col="blue")

###############################################

plot(resid3, ylab="Residuals")

###############################################

## Finally, fit seasonal effect

## Create dummy variables for each month
n = length(ap)
jan = as.numeric((1:n %% 12) == 1)
feb = as.numeric((1:n %% 12) == 2)
mar = as.numeric((1:n %% 12) == 3)
apr = as.numeric((1:n %% 12) == 4)
may = as.numeric((1:n %% 12) == 5)
jun = as.numeric((1:n %% 12) == 6)
jul = as.numeric((1:n %% 12) == 7)
aug = as.numeric((1:n %% 12) == 8)
sep = as.numeric((1:n %% 12) == 9)
oct = as.numeric((1:n %% 12) == 10)
nov = as.numeric((1:n %% 12) == 11)
dec = as.numeric((1:n %% 12) == 0)

## Fit model to our residuals using the dummy variables
fit4 = lm(resid3 ~ 0 + jan + feb + mar + apr + may + jun + jul + aug +
            sep + aug + oct + nov + dec)
fit4

## So, for example, log sales are typically 0.08 below the trend in January;
## 0.02 above the trend in March etc.

## Now see what variation is left after removal of trend and seasonal effects.
seasonal = ts(fitted(fit4), start=start(ap), end=end(ap),
              frequency=frequency(ap))
fv = trend3 + seasonal
resid5 = log.ap - fv
par(mfrow=c(2,1))
plot(log.ap, ylab="log airline bookings (1000s)")
lines(fv, col="blue")

############################################

plot(resid5, ylab="Residuals")

## No clearly visible structure remaining.  However, plotting the
## residual at time t+1 against the residual at time t shows that
## successive fluctuations are not independent.

par(mfrow=c(1,1))
plot(resid5[-1]~resid5[-n], xlab=expression(Y[t]),
     ylab=expression(Y[t+1]), pch=16)

################################################

## If we look at observations five time-points apart, this correlation
## has vanished:
plot(resid5[-(1:5)]~resid5[-((n-4):n)], xlab=expression(Y[t]),
     ylab=expression(Y[t+5]), pch=16)

###############################################

## But at 20 time points, it is back:
plot(resid5[-(1:20)]~resid5[-((n-19):n)], xlab=expression(Y[t]),
     ylab=expression(Y[t+20]), pch=16)
################################################