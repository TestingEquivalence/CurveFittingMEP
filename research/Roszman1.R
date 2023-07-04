library(NISTnls)
source("distance.R")
source("curveFittingMEP.R")
source("size.R")

data=Roszman1
data=data[order(data$x),]
frm=y ~ b1 - b2*x - atan(b3/(x-b4))/pi
start=c(b1 = 0.1, b2 = -0.00001, b3 = 1000, b4 = -100)
ab=c(-4869,-464)


# model
mod=MDE
m=curveFittingMEP(frm,data,none, ab, start, method = mod)
m$distance
write.csv(m$coef,"coef.csv")

plot(m$data$x, m$prediction-m$data$y)
plot(m$data$x, m$prediction, col="blue",type="l")
points(m$data$x,m$data$y)

set.seed(10071977)
m=curveFittingMEP(frm,data,asymptoticBV, ab, start, method = mod, nSimulation = 1000)
m$min.epsilon

set.seed(10071977)
m=curveFittingMEP(frm,data,tPercentileBootstrap, ab, start, method = mod, nSimulation = 200, 
                  nSimPercentileTBootstrap = 200)
m$min.epsilon

# bootstrap coefficients

m=curveFittingMEP(frm,data,none, ab, start, method = LSE)
set.seed(10071977)
res=bootstrapCoef(m,1000)
write.csv(res,"bst_coef_LSE.csv")

m=curveFittingMEP(frm,data,none, ab, start, method = MDE)
set.seed(10071977)
res=bootstrapCoef(m,1000)
write.csv(res,"bst_coef_MDE.csv")
