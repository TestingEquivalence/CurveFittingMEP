library(nlstools)
#View(L.minor)

# conc is random
data=list()
data$x=L.minor$conc
data$y=L.minor$rate
data=as.data.frame(data)

L.minor.m1 <- nls(rate ~ Vm*conc/(K+conc), data = L.minor, start = list(K=20, Vm=120))
confint2(L.minor.m1)
confint2(L.minor.m1, "K")

plot(L.minor$conc,L.minor$rate)
lines(L.minor$conc,predict(L.minor.m1), col="blue")

frm="y ~ Vm*x/(K+x)"
start= start = list(K=20, Vm=120)
model.nls=nls(frm, data,start)
coef(L.minor.m1)
coef(model.nls)

# residuals
data$f=predict(model.nls)
data$res.nls=data$f-data$y
plot(data$x,data$res.nls)
