library(NISTnls)
source("distance.R")
source("curveFittingMEP.R")
source("size.R")

data=Kirby2
data=data[order(data$x),]
frm=y ~ (b1 + b2*x + b3*x**2) / (1 + b4*x + b5*x**2)
start=c(b1 = 2, b2 = -0.1, b3 = 0.003,
        b4 = -0.001, b5 = 0.00001)

ab=c(9,372)


# model LSE
m=curveFittingMEP(frm,data,none, ab, start, method = LSE)
m$distance
m$coef
write.csv(m$coef, "coef.csv")

plot(m$data$x, m$prediction-m$data$y)
plot(m$data$x, m$prediction, col="blue",type="l")
points(m$data$x,m$data$y)

set.seed(10071977)
m=curveFittingMEP(frm,data,asymptoticBV, ab, start, method = LSE, nSimulation = 1000)
m$min.epsilon

set.seed(10071977)
m=curveFittingMEP(frm,data,tPercentileBootstrap, ab, start, method = LSE, nSimulation = 50, 
                  nSimPercentileTBootstrap = 200)
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
m=curveFittingMEP(frm,data,tPercentileBootstrap, ab, start, method = MDE, nSimulation = 100, 
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
