library(NISTnls)
source("distance.R")
source("curveFittingMEP.R")

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
