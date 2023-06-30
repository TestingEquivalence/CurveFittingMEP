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

m=curveFittingMEP(frm,data,none, ab, start, method=LSE)



# xSamplerFix and errSamplerWildBootstrap work together only
res=powerAtModel(m,nSim=10, xSamplerFix, errSamplerWildBootstrap)
res

# combine xSamplerUniform with errSamplerBootstrap, errSamplerSmoothBootstrap and errSamplerNormal

res=powerAtModel(m,nSim=10, xSamplerUniform, errSamplerBootstrap)
res

res=powerAtModel(m,nSim=10, xSamplerUniform, errSamplerSmoothBootstrap)
res

res=powerAtModel(m,nSim=10, xSamplerUniform, errSamplerNormal)
res

# combine xSamplerBootstrap with errSamplerBootstrap, errSamplerSmoothBootstrap and errSamplerNormal

res=powerAtModel(m,nSim=10, xSamplerBootstrap, errSamplerBootstrap)
res

res=powerAtModel(m,nSim=10, xSamplerBootstrap, errSamplerSmoothBootstrap)
res

res=powerAtModel(m,nSim=10, xSamplerBootstrap, errSamplerNormal)
res

# combine xSamplerBootstrap with errSamplerBootstrap, errSamplerSmoothBootstrap and errSamplerNormal

res=powerAtModel(m,nSim=10, xSamplerSmoothBootstrap, errSamplerBootstrap)
res

res=powerAtModel(m,nSim=10, xSamplerSmoothBootstrap, errSamplerSmoothBootstrap)
res

res=powerAtModel(m,nSim=10, xSamplerSmoothBootstrap, errSamplerNormal)
res
