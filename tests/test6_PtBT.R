library(nlsMicrobio)
source("distance.R")
source("curveFittingMEP.R")

# conc is random
data=list()
data$x=L.minor$conc
data$y=L.minor$rate
data=as.data.frame(data)
data=data[order(data$x),]


frm=y ~ Vm*x/(K+x)
start= list(K=20, Vm=120)
ab=c(0,205)


model=curveFittingMEP(frm,data, tPercentileBootstrap, ab, start = start, method=LSE, nSimulation = 50,
                      nSimPercentileTBootstrap=200)
model$distance
model$min.epsilon

model2=curveFittingMEP(frm,data,asymptoticBV,ab,start, method=MDE)
model2$distance
model2$min.epsilon
