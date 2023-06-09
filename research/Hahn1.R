library(NISTnls)
source("distance.R")
source("curveFittingMEP.R")
source("size.R")

data=Hahn1
data=data[order(data$x),]
frm=y ~ (b1+b2*x+b3*x**2+b4*x**3) / (1+b5*x+b6*x**2+b7*x**3)
start=c(b1 = 10, b2 = -1, b3 = 0.05,
        b4 = -0.00001, b5 = -0.05, b6 = 0.001, b7 = -0.000001)
ab=c(14,852)


# model LSE
m=curveFittingMEP(frm,data,none, ab, start, method = LSE)
m$distance
m$coef

plot(m$data$x, m$prediction-m$data$y)
plot(m$data$x, m$prediction, col="blue",type="l")
points(m$data$x,m$data$y)

set.seed(10071977)
m=curveFittingMEP(frm,data,asymptoticBV, ab, start, method = LSE, nSimulation = 1000)
m$min.epsilon

set.seed(10071977)
m=curveFittingMEP(frm,data,tPercentileBootstrap, ab, start, method = LSE, nSimulation = 200, 
                  nSimPercentileTBootstrap = 1000)
m$min.epsilon


# model MDE
m=curveFittingMEP(frm,data,none, ab, start, method = MDE)
m$distance
write.csv(m$coef, "coef.csv")

plot(m$data$x, m$prediction-m$data$y)
plot(m$data$x, m$prediction, col="blue",type="l")
points(m$data$x,m$data$y)

set.seed(10071977)
m=curveFittingMEP(frm,data,asymptoticBV, ab, start, method = MDE, nSimulation = 1000)
m$min.epsilon
 
set.seed(10071977)
m=curveFittingMEP(frm,data,tPercentileBootstrap, ab, start, method = MDE, nSimulation = 200, 
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
