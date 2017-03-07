lorry = scan("lorry.txt") # assumes lorry.txt is saved in your file space
# missing values are denoted NA in the above file
# setup dates for all years between 1965 and 1997
dates = c(1965:1997)
# analyse linear model lorry = A + B*dates
ll = lm(lorry~dates)
predict(ll, data.frame(dates=c(2000)), interval=c("confidence"),
        se.fit=TRUE, level=c(0.95))
