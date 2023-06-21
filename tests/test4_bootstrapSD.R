library(nlsMicrobio)
source("distance.R")
source("curveFittingMEP.R")
source("asymptoticTestBootstrapVariance.R")

# conc is random
data=list()
data$x=L.minor$conc
data$y=L.minor$rate
data=as.data.frame(data)
data=data[order(data$x),]


frm=y ~ Vm*x/(K+x)
start= list(K=20, Vm=120)
ab=c(0,205)

model=curveFittingMEP(frm,data,none, ab, start, method=LSE, nSimulation = 10000)
model$distance
bootstrapSD(m=model)


model2=curveFittingMEP(frm,data,none,ab,start, method=MDE, nSimulation = 1000)
model2$distance
bootstrapSD(m=model2)
