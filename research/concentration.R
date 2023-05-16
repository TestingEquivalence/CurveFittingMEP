library(nlstools)
View(L.minor)

# conc is random
L.minor.m1 <- nls(rate ~ Vm*conc/(K+conc), data = L.minor, start = list(K=20, Vm=120))
confint2(L.minor.m1)
confint2(L.minor.m1, "K")

plot(L.minor$conc,L.minor$rate)
lines(L.minor$conc,predict(L.minor.m1))
