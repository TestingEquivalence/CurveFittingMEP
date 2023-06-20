library(NISTnls)
source("distance.R")
source("curveFittingMEP.R")

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


# model MDE
m=curveFittingMEP(frm,data,none, ab, start, method = MDE)
m$distance
m$coef

plot(m$data$x, m$prediction-m$data$y)
plot(m$data$x, m$prediction, col="blue",type="l")
points(m$data$x,m$data$y)
