library(nlstools)
#View(L.minor)

# conc is random
data=list()
data$x=L.minor$conc
data$y=L.minor$rate
data=as.data.frame(data)
data=data[order(data$x),]


frm="y ~ Vm*x/(K+x)"
start= list(K=20, Vm=120)
model.nls=nls(frm, data,start)
coef(model.nls)

plot(data$x,data$y,ylim=c(0,120))
lines(data$x,predict(model.nls), col="blue")

# residuals
data$f=predict(model.nls)
data$res.nls=data$f-data$y
plot(data$x,data$res.nls)
