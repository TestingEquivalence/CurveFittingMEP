library(NISTnls)
source("distance.R")
source("curveFittingMEP.R")

data=Roszman1
data=data[order(data$x),]
frm=y ~ b1 - b2*x - atan(b3/(x-b4))/pi
start=c(b1 = 0.1, b2 = -0.00001, b3 = 1000, b4 = -100)
ab=c(-4869,-464)


model=curveFittingMEP(frm,data, tPercentileBootstrap, ab, start = start, method=NLS, nSimulation = 200,
                      nSimPercentileTBootstrap=200)
model$distance
model$min.epsilon

model2=curveFittingMEP(frm,data,tPercentileBootstrap,ab,start, method=LM, nSimulation = 100,
                       nSimPercentileTBootstrap = 200)
model2$distance
model2$min.epsilon
 