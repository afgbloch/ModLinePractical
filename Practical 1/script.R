load("../cars.RData")

y = 100 / cars$CityMPG
x1 = cars$Weight
x2 = cars$Horsepower/cars$Weight

fit = lm(y ~ x1 + x2,cars)

summary(fit)
confint(fit)

plot(cooks.distance(fit))
p <- dim(model.matrix(fit))[2]
n <- dim(model.matrix(fit))[1]
abline(8/(n-2*p),0)
