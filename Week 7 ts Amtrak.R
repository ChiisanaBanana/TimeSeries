library(forecast)

setwd("C:/Users/halan/Desktop/FPU2025Spring/Time Series")

Amtrak <- read.csv("Amtrak.csv") #change to your filepath

# create ts object
ridership.ts <- ts(Amtrak$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)

# Figure 3-1
plot(ridership.ts, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25))
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1), digits = 2))
lines(c(2004.25 - 3 , 2004.25 - 3), c(0, 3500))
lines(c(2004.25, 2004.25), c(0, 3500))
text(1996.25, 2500, "Training")
text(2002.75, 2500, "Validation")
text(2005.25, 2500, "Future")
arrows(2004 - 3,2450,1991.25,2450,code=3,length=0.1,lwd=1,angle=30)
arrows(2004.5 - 3,2450,2004,2450,code=3,length=0.1,lwd=1,angle=30)
arrows(2004.5,2450,2006,2450,code=3,length=0.1,lwd=1,angle=30)

# create training and validation partition objects (note note process here)
nValid <- 36
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))

# fit quadratic trend model to training data
ridership.lm <-  tslm(train.ts ~ trend + I(trend^2))

# forecast validation period from trained model
ridership.lm.pred <- forecast(ridership.lm, h = nValid, level = 0)

# plot forecasts and original data (figure 3-2)
plot(ridership.lm.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1))) 
lines(ridership.lm$fitted, lwd = 2)
lines(valid.ts)
lines(c(2004.25 - 3, 2004.25 - 3), c(0, 3500)) 
lines(c(2004.25, 2004.25), c(0, 3500))
text(1996.25, 2500, "Training")
text(2002.75, 2500, "Validation")
text(2005.25, 2500, "Future")
arrows(2004 - 3, 2450, 1991.25, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5 - 3, 2450, 2004, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5, 2450, 2006, 2450, code = 3, length = 0.1, lwd = 1, angle = 30)

# plot forecast errors/residuals (Figure 3-3)
plot(ridership.lm.pred$residuals, ylim = c(-400, 500),  ylab = "Residuals", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(valid.ts - ridership.lm.pred$mean, lwd = 1)
lines(c(2004.25 - 3, 2004.25 - 3), c(-500, 3500))
lines(c(2004.25, 2004.25), c(-500, 3500))
text(1996.25, 500, "Training")
text(2002.75, 500, "Validation")
text(2005.25, 500, "Future")
arrows(2004 - 3, 450, 1991.25, 450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5 - 3, 450, 2004, 450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5, 450, 2006, 450, code = 3, length = 0.1, lwd = 1, angle = 30)

# plot histogram of residuals/forecast errors (Figure 3-4)
hist(ridership.lm.pred$residuals,  ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")

# use the 'checkresiduals' function from the forecast package
checkresiduals(ridership.lm)
