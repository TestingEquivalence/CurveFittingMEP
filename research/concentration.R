library(nlstools)
source("distance.R")
source("curveFittingMEP.R")

# conc is random
data=list()
data$x=L.minor$conc
data$y=L.minor$rate
data=as.data.frame(data)
data=data[order(data$x),]

# model LSE

frm=y ~ Vm*x/(K+x)
start= list(K=20, Vm=120)
ab=c(0,205)
m=curveFittingMEP(frm,data,none, ab, start, method = LSE)

plot(m$data$x, m$prediction-m$data$y)
plot(m$data$x, m$prediction, col="blue",type="l")
points(m$data$x,m$data$y)

# model MDE

frm=y ~ Vm*x/(K+x)
start= list(K=20, Vm=120)
ab=c(0,205)
model.MDE=curveFittingMEP(frm,data,none, ab, start, method = MDE)

data$f.MDE=predict(model.MDE$model$)
data$res.LSE=data$f.LSE-data$y
plot(data$x,data$res.LSE)
plot(data$x,data$f.LSE, col="blue",type="l")
points(data$x,data$y)
