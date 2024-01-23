library(NISTnls)  
source("distance.R")
source("curveFittingMEP.R")
source("size.R")

data=Roszman1
data=data[order(data$x),]
frm=y ~ b1 - b2*x - atan(b3/(x-b4))/pi
start=c(b1 = 0.1, b2 = -0.00001, b3 = 1000, b4 = -100)
ab=c(-4869,-464)

n=5
m=curveFittingMEP(frm,data,none, ab, start, method=NLS)
s=xSamplerUniform(m,n)
s

set.seed(10071977)
s=xSamplerBootstrap(m,n)
s

set.seed(10071977)
s=xSamplerSmoothBootstrap(m,n)
s

s=xSamplerFix(m,n)
s
