load("../../cars.RData")
library(car)
library(xtable)

#------------------------------
#         Introduction
#------------------------------

y = 100 / cars$CityMPG
x1 = cars$Weight
x2 = cars$Horsepower/cars$Weight
n = dim(cars)[1]
#------------------------------
#          The data 
#------------------------------

#TODO ??

#------------------------------
#          Analysis 1
#------------------------------

#Analyse of variance for model 1
fit = lm(y ~ x1 + x2, cars)
aov.model1 = aov(fit)
summary(aov.model1)
xtable(summary(aov.model1))

#Analyse of variance with inverted modelanova
fit2 = lm(y ~ x2 + x1, cars)
aov.model2 = aov(fit2)
summary(aov.model2)
xtable(summary(aov.model2))

#------------------------------
#          Analysis 2
#------------------------------

# Fit the full model
cars.sub = data.frame(cars[,11:26])
full.model = lm(y~., data = cars.sub)
summary(full.model)
xtable(summary(full.model))

# Compute the VIF for the full model
VIF = data.frame(vif(full.model))
xtable(VIF)

# backward deletion with AIC
bd.aic.model = step(full.model, direction = "backward")
summary(bd.aic.model)

# forward selection with AIC
base.model = lm(y~1, data = cars.sub)
extend.model = formula(data.frame(y, cars.sub))
fs.aic.model = step(base.model, direction = "forward", scope = extend.model)
summary(fs.aic.model)

# backward deletion with BIC
bd.bic.model = step(full.model, direction = "backward", k = log(n))
summary(bd.bic.model)

# forward selection with BIC
fs.bic.model = step(base.model, direction = "forward", scope = extend.model, k = log(n))
summary(fs.bic.model)

# Remove variable
cars.sub.clean = cars.sub[, -c(7, 9:12)]
full.model.clean = lm(y~., data = cars.sub.clean)

# backward deletion with AIC and clean model
bd.aic.model.clean = step(full.model.clean, direction = "backward")
summary(bd.aic.model.clean)

# forward selection with AIC and clean model
extend.model.clean = formula(data.frame(y, cars.sub.clean))
fs.aic.model.clean = step(base.model, direction = "forward", scope = extend.model.clean)
summary(fs.aic.model.clean)

# backward deletion with BIC and clean model
bd.bic.model.clean = step(full.model.clean, direction = "backward", k = log(n))
summary(bd.bic.model.clean)

# forward selection with BIC and clean model
fs.bic.model.clean = step(base.model, direction = "forward", scope = extend.model.clean, k = log(n))
summary(fs.bic.model.clean)

# justification of removal
removal = cars.sub[, c(7,9:12)]
pdf("scatter-plots.pdf")
pairs(removal)
dev.off()
cor = cor(removal)
xtable(cor)

# VIF of the two remaining model
VIF1 = data.frame(vif(fs.aic.model.clean))
VIF2 = data.frame(vif(fs.bic.model.clean))
xtable(VIF1)
xtable(VIF2)

#diagnostics for final model
final.model = fs.bic.model.clean
n <- dim(model.matrix(fit))[1]
p <- dim(model.matrix(fit))[2]

# plot standardized residuals against fitted values
pdf("standardRes-fitted.pdf")
plot(fitted(final.model), rstandard(final.model), xlab="Fitted values", ylab="Standard residuals")
dev.off()

# QQ plot
pdf("qqplot.pdf")
qqnorm(rstandard(final.model))
qqline(rstandard(final.model))
dev.off()

# Cook's distanes
pdf("cook-distance.pdf")
par(pty="s")
plot(cooks.distance(final.model), ylab="Cook's distance")
abline(h=8/(n-2*p), col="red")
identify(cooks.distance(final.model))
dev.off()

