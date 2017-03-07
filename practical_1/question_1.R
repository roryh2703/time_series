#Practical 1 code
#24nd February 2017
#Rory Hetherington

setwd("/usr/not-backed-up/time_series/practical_1")

load("data/pond.RData")
start(X)
end(X)
frequency(X)
class(X)    #check class of data: 'ts'

#    simple stats
summary(X); mean(X); var(X); sd(X)

#    plot data
jpeg('plots/plot_data.jpg')
plot(X, xlab="Year", ylab="Monthly water levels (ft)")
dev.off()

################################################
## Fit a linear trend

tt = 1:length(X)
fit1 = lm(X~tt)
trend1 = fitted(fit1) # trend as a vector; convert it to a "ts" object
trend1 = ts(trend1, start=start(X), end=end(X), frequency=frequency(X))

## Superimpose on time series plot
lines(trend1, col="blue")

## Find and plot residuals
resid1 = ts(residuals(fit1), start=start(X), end=end(X), 
	frequency=frequency(X))

jpeg('plots/plot_data_residuals.jpg')
plot(resid1, ylab="Residuals")
dev.off()


###############################################

## Finally, fit seasonal effect

## Create dummy variables for each month
n = length(X)
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
fit2 = lm(resid1 ~ 0 + jan + feb + mar + apr + may + jun + jul + aug +
            sep + aug + oct + nov + dec)
fit2

## So, for example, log sales are typically 0.08 below the trend in January;
## 0.02 above the trend in March etc.

## Now see what variation is left after removal of trend and seasonal effects.
seasonal = ts(fitted(fit2), start=start(X), end=end(X),
              frequency=frequency(X))
fv = trend1 + seasonal
resid5 = X - fv
par(mfrow=c(2,1))
plot(X, ylab="...")
lines(fv, col="blue")

############################################

plot(resid5, ylab="Residuals")









