library(NISTnls)
source("distance.R")
source("curveFittingMEP.R")

data=Roszman1
data=data[order(data$x),]
frm=y ~ b1 - b2*x - atan(b3/(x-b4))/pi
start=c(b1 = 0.1, b2 = -0.00001, b3 = 1000, b4 = -100)
ab=c(-4869,-464)

model=curveFittingMEP(frm,data,asymptoticBV, ab, start, method=LSE, nSimulation = 1000)
model$distance
model$min.epsilon

model2=curveFittingMEP(frm,data,asymptoticBV,ab,start, method=MDE, nSimulation = 1000)
model2$distance
model2$min.epsilon
