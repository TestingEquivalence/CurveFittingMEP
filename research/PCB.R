library(NRAIA)

str(PCB)
# compare to Figure 1.1 (p. xxx)
xyplot(conc ~ age, PCB, aspect = 'xy',
       type = c("g", "p"), xlab = "Age of fish (yr)",
       ylab = "PCB concentration")
# compare to Figure 1.2 (p. xxx)
xyplot(conc ~ age, PCB, scales = list(y = list(log = 2)),
       aspect = 'xy', type = c("g", "p", "smooth"),
       xlab = "Age of fish (yr)",
       ylab = "PCB concentration")
# linear model in cube root of age
summary(fm1 <- lm(log(conc) ~ I(age^(1/3)), data = PCB))
xyplot(log(conc) ~ I(age^(1/3)), data = PCB, aspect = 'xy',
       xlab = "Cube root of age (yr)", type = c("g", "p", "r"),
       ylab = "log(PCB concentration)",
       main = "Transformed PCB data and fitted line",
       sub = deparse(fm1$call$formula))
# diagnostic plots
opar <- par(mfrow = c(2, 2))
plot(fm1, which = 1:4, las = 1)
par(opar)
