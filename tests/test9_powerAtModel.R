library(NISTnls)
source("distance.R")
source("curveFittingMEP.R")
source("size.R")

data=Roszman1
data=data[order(data$x),]
frm=y ~ b1 - b2*x - atan(b3/(x-b4))/pi
start=c(b1 = 0.1, b2 = -0.00001, b3 = 1000, b4 = -100)
ab=c(-4869,-464)

m=curveFittingMEP(frm,data,asymptoticBV, ab, start, method=NLS)



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
