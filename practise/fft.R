#############################################################
## Toy example (example 7.1)
x = c(0,1,2,3)
dft.x = fft(x)/sqrt(length(x))
dft.x

fft(dft.x, inverse=T)/sqrt(length(x)) ## Note "inverse=T"

#############################################################
## Example 7.2

Mod(dft.x)^2

#############################################################
## Real example; co2 data
## First detrend and deseasonalise as normal

data(co2)
n = length(co2)
tt = as.numeric(time(co2))
trend = lm(co2 ~ tt)
Y = residuals(trend)
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
S = lm(Y ~ 0+jan+feb+mar+apr+may+jun+jul+aug+sep+oct+nov+dec)
Z = residuals(S)

## Compute DFT and I(f) for each series

dft.co2 = fft(co2)/sqrt(length(co2))
I.co2 = Mod(dft.co2)^2

dft.Y = fft(Y)/sqrt(length(Y))
I.Y = Mod(dft.Y)^2

dft.Z = fft(Z)/sqrt(length(Z))
I.Z = Mod(dft.Z)^2

## Plot these

op = par(mfrow=c(3,2))
plot(co2, ylab = expression(CO[2], X))
plot(x=(1:(n-1))/n, y = I.co2[-1], type="h", xlab=expression(f[j]), ylab=expression(I(f[j])))
title(main="Raw periodogram of co2 data")

plot(ts(Y, start=start(co2), freq=frequency(co2)), ylab="Y")
plot(y=I.Y[-1], x=(1:(n-1))/n, type="h", xlab=expression(f[j]), ylab=expression(I(f[j])))
title(main="Raw periodogram of Y")

plot(ts(Z, start=start(co2), freq=frequency(co2)), ylab="Z")
plot(y=I.Z[-1], x=(1:(n-1))/n, type="h", xlab=expression(f[j]), ylab=expression(I(f[j])))
title(main="Raw periodogram of Z")
par(op)


## Aliasing means we will normally only consider 0 < f <= 1/2.  Also,
## we often plot log I(f).

op = par(mfrow=c(3,2))
plot(co2, ylab = "X", main="Raw CO2 data")
plot(x=(1:(n/2))/n, y = I.co2[2:(n/2+1)], type="h", log="y",   xlab=expression(f[j]), ylab=expression(I(f[j])))
title(main="Raw periodogram of X")

plot(ts(Y, start=start(co2), freq=frequency(co2)), ylab="Y", main="Detrended CO2 data")
plot(x=(1:(n/2))/n, y = I.Y[2:(n/2+1)], type="h",   log="y",  xlab=expression(f[j]), ylab=expression(I(f[j])))
title(main="Raw periodogram of Y")

plot(ts(Z, start=start(co2), freq=frequency(co2)), ylab="Z", main="Detrended and deseasonalised CO2 data")
plot(x=(1:(n/2))/n, y = I.Z[2:(n/2+1)], type="h",    log="y", xlab=expression(f[j]), ylab=expression(I(f[j])))
title(main="Raw periodogram of Z")


par(op)

## Note large spike in periodogram for Y at about 1/12 - indicating
## annual effect.  It is present (but hard to see) in the periodogram
## of the original data and absent in the periodogram of Z.



#####################################################################
## Now the airline passenger data

ap = AirPassengers
tt = 1:length(ap) # don't use "t" as it is a function in R!
tt2 = tt^2
log.ap = log(ap)
fit3 = lm(log.ap ~ tt + tt2)
trend3 = ts(fitted(fit3), start=start(ap), end=end(ap), frequency=frequency(ap))
resid3 = ts(residuals(fit3), start=start(ap), end=end(ap), frequency=frequency(ap))
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
fit4 = lm(resid3 ~ 0 + jan + feb + mar + apr + may + jun + jul + aug +
  sep + aug + oct + nov + dec)
seasonal = ts(fitted(fit4), start=start(ap), end=end(ap),
  frequency=frequency(ap))
fv = trend3 + seasonal
resid5 = log.ap - fv


dft.ap = fft(log.ap)/sqrt(n)
I.ap = Mod(dft.ap)^2

dft.Yap = fft(resid3)/sqrt(n)
I.Yap = Mod(dft.Yap)^2

dft.Zap = fft(resid5)/sqrt(n)
I.Zap = Mod(dft.Zap)^2

op = par(mfrow=c(3,2))
plot(log.ap, ylab = "X", main="Log AP data")
plot(x=(1:(n/2))/n, y = I.ap[2:(n/2+1)], type="h", log="y",   xlab=expression(f[j]), ylab=expression(I(f[j])))
title(main="Raw periodogram")

plot(resid3, ylab="Y", main="Detrended log AP data")
plot(x=(1:(n/2))/n, y = I.Yap[2:(n/2+1)], type="h",   log="y",  xlab=expression(f[j]), ylab=expression(I(f[j])))
title(main="Raw periodogram")

plot(resid5, ylab="Z", main="Detrended and deseasonalised log AP data")
plot(x=(1:(n/2))/n, y = I.Zap[2:(n/2+1)], type="h",    log="y", xlab=expression(f[j]), ylab=expression(I(f[j])))
title(main="Raw periodogram")
par(op)

## Again, the annual cycle is clear in the first two rows but absent
## in the last.  The trend is shown as low-frequency activity in the
## first periodogram, but absent in the later two.
