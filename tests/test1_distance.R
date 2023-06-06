library(nlstools)
View(L.minor)
source("distance.R")

# conc is random
L.minor.m1 <- nls(rate ~ Vm*conc/(K+conc), data = L.minor, start = list(K=20, Vm=120))
confint2(L.minor.m1)

plot(L.minor$conc,L.minor$rate)
lines(L.minor$conc,predict(L.minor.m1), col="blue")

df=list()
df$x=L.minor$conc
df$y=L.minor$rate
df=as.data.frame(df)
df$f=fitted(L.minor.m1)

ab=c(0,max(df$x))

dst_an=distance(df)