library(nlsMicrobio)
source("distance.R")
source("curveFittingMEP.R")
source("size.R")

# conc is random
data=list()
data$x=L.minor$conc
data$y=L.minor$rate
data=as.data.frame(data)
data=data[order(data$x),]

frm=y ~ Vm*x/(K+x)
start= list(K=20, Vm=120)
ab=c(0,205)

# model LSE
mod=LSE
m=curveFittingMEP(frm,data,none, ab, start, method = mod)
m$distance
m$coef

plot(m$data$x, m$prediction-m$data$y)
plot(m$data$x, m$prediction, col="blue",type="l")
points(m$data$x,m$data$y)

set.seed(10071977)
m=curveFittingMEP(frm,data,asymptoticBV, ab, start, method = mod, nSimulation = 1000)
m$min.epsilon

set.seed(10071977)
m=curveFittingMEP(frm,data,tPercentileBootstrap, ab, start, method = mod, nSimulation = 200, 
                  nSimPercentileTBootstrap = 1000)
m$min.epsilon


# bootstrap coefficients

m=curveFittingMEP(frm,data,none, ab, start, method = LSE)
set.seed(10071977)
res=bootstrapCoef(m,1000)
write.csv2(res,"bst_coef_LSE.csv")

m=curveFittingMEP(frm,data,none, ab, start, method = MDE)
set.seed(10071977)
res=bootstrapCoef(m,1000)
write.csv2(res,"bst_coef_MDE.csv")

# simulate power at the model

m=curveFittingMEP(frm,data,asymptoticBV, ab, start, method = MDE)
res=powerAtModel(m,nSim=1000, xSamplerSmoothBootstrap, errSamplerNormal)
write.csv(res,"power.csv")
 
