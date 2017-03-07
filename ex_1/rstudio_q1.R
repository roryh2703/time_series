data(lynx) # connect to lynx data set
lynx # print lynx data on screen
jpeg('ex1q1_rplot.jpg')
plot(lynx, xlab="Year", ylab="Number")
dev.off()
summary(lynx) # summary statistics
mean(lynx)
var(lynx)
sd(lynx)