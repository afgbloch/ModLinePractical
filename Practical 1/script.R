load("../cars.RData")
library(car)
library(xtable)

#------------------------------
#         Introduction
#------------------------------

y = 100 / cars$CityMPG
x1 = cars$Weight
x2 = cars$Horsepower/cars$Weight

#------------------------------
#          The data
#------------------------------

# Boxplots
pdf("boxplots.pdf")
par(mfrow=c(1,3))
boxplot(y, main = "Fuel efficiency", ylab="Gallons per a hundred city miles")
boxplot(x1, main = "Weights", ylab="Weight in pounds")
boxplot(x2, main = "Horsepower / Weights", ylab="Horsepower per pounds")
dev.off()

# Scatter plots
pdf("scatter-plot-1.pdf")
scatterplot(y ~ x1, main = "", ylab = "Gallons per a hundred city miles", xlab = "Weight in pounds")
dev.off()
pdf("scatter-plot-2.pdf")
scatterplot(y ~ x2, main = "", ylab = "Gallons per a hundred city miles", xlab = "Horsepower per pounds")
dev.off()

#------------------------------
#          Analysis 1
#------------------------------

fit = lm(y ~ x1 + x2, cars)
n <- dim(model.matrix(fit))[1]
p <- 3

xtable(summary(fit), auto = TRUE)
summary(fit)

confint(fit)
xtable(confint(fit), auto = TRUE)

# Assumption validity check

# plot standardized residuals against explanatory variables
pdf("standardRes-explanatory.pdf")
par(pty="s", mfrow=c(1, 2))
plot(x1, rstandard(fit), xlab="Weight in pounds", ylab="Standard residuals")
plot(x2, rstandard(fit), xlab="Horsepower per pounds", ylab="Standard residuals")
dev.off()

# plot standardized residuals against fitted values
pdf("standardRes-fitted.pdf")
plot(fitted(fit), rstandard(fit), xlab="Fitted values", ylab="Standard residuals")
dev.off()

# QQ plot
pdf("qqplot.pdf")
qqnorm(rstandard(fit))
qqline(rstandard(fit))
dev.off()

# Cook's distanes
pdf("cook-distance.pdf")
par(pty="s")
plot(cooks.distance(fit), ylab="Cook's distance")
abline(h=8/(n-2*p), col="red")
identify(cooks.distance(fit))
dev.off()

# Leverage value for both outliers
pdf("leverage-point.pdf")
plot(hatvalues(fit))
abline(h=2*p/n, col="red")
identify(hatvalues(fit))
dev.off()

#------------------------------
#          Analysis 2
#------------------------------

# remove candidate outlier
carsC = cars[-80,]
carsC = carsC[-37,]

yC = 100 / carsC$CityMPG
x1C = carsC$Weight
x2C = carsC$Horsepower/carsC$Weight

fitC = lm(yC ~ x1C + x2C, carsC)
nC <- dim(model.matrix(fitC))[1]
pC <- 3

xtable(summary(fitC), auto = TRUE)
summary(fitC)

confint(fitC)
xtable(confint(fitC), auto = TRUE)

# Assumption validity check

# plot standardized residuals against explanatory variables
pdf("standardRes-explanatory-clean.pdf")
par(pty="s", mfrow=c(1, 2))
plot(x1C, rstandard(fitC), xlab="Weight in pounds", ylab="Standard residuals")
plot(x2C, rstandard(fitC), xlab="Horsepower per pounds", ylab="Standard residuals")
dev.off()

# plot standardized residuals against fitted values
pdf("standardRes-fitted-clean.pdf")
plot(fitted(fitC), rstandard(fitC), xlab="Fitted values", ylab="Standard residuals")
dev.off()

# QQ plot
pdf("qqplot-clean.pdf")
qqnorm(rstandard(fitC))
qqline(rstandard(fitC))
dev.off()

# Leverage value for both outliers
pdf("leverage-point-clean.pdf")
plot(hatvalues(fitC))
abline(h = 2 * pC / nC, col = "red")
identify(hatvalues(fitC))
dev.off()


