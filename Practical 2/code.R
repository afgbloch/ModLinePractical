

aov.model = aov(model)
summary(aov.model)
model2 <- lm(y~x2+x1)
aov.model2 = aov(model2)
summary(aov.model2)



x.sub <- data.frame(cars[,11:26])
full.model <-lm(y~., data = x.sub)
summary(full.model)
library(cars)

vif(full.model)

bwd.model <- step(full.model, direction = "backward")
summary(bwd.model)

base.model <- lm(y~1, data= x.sub)
df <- data.frame(y, x.sub)
biggest <- formula(df)
fwd.model <- step(base.model, direction = "forward", scope=biggest)

BIC.bwd.model <- step(full.model, direction = "backward", k = log(82))
BIC.fwd.model <-step(base.model, direction = "forward", scope = biggest, k = log(82))

x.sub.clean <-x.sub[, -c(7,9:12)]

scatterplotMatrix(~FuelCapacity + Length+Wheelbase+Width+Uturn, data = x.sub)
remove <-x.sub[, c(7,9:12)]
cor(remove, y = NULL, method = "pearson")

df2 <- data.frame(y, x.sub.clean)
biggest2 <-formula(df2)
full.model2 <-lm(y~., data = x.sub.clean)
bwd.model2 <- step(full.model2, direction = "backward")
15BIC.bwd.model2 <- step(full.model2, direction = "backward", k = log(82))
fwd.model2 <- step(base.model, direction="forward", scope = biggest2)
BIC.fwd.model2 <-step(base.model, direction="forward", scope = biggest2, k=log(82))

finalmodel.stdr = rstandard(BIC.fwd.model2)
final.fitted = fitted(BIC.fwd.model2)
pdf("final_fitted.pdf", width = 6, height = 4)
plot(final.fitted, finalmodel.stdr, xlab="Fitted Values", ylab = "Standardised Residuals", m
box()
dev.off()
plot(cooks.distance(BIC.fwd.model2), ylab="Cook’s Distance", xlab="Observations", main="Cook
abline(h=8/(82-2*3), col="red", lty=2, lwd=2)
identify(cooks.distance(BIC.fwd.model2))