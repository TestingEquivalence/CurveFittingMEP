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

m=curveFittingMEP(frm,data,asymptoticBV, ab, start, method=LSE, nSimulation = 200)

# xSamplerFix and errSamplerWildBootstrap work together only
res=powerAtModel(m,nSim=10, xSamplerFix, errSamplerWildBootstrap)
res

# combine xSamplerUniform with errSamplerBootstrap and errSamplerNormal

res=powerAtModel(m,nSim=10, xSamplerUniform, errSamplerBootstrap)
res
