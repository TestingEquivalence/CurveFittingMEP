library(NISTnls)
source("distance.R")

# conc is random
data=Roszman1
data=data[order(data$x),]
frm=y ~ b1 - b2*x - atan(b3/(x-b4))/pi
start=c(b1 = 0.1, b2 = -0.00001, b3 = 1000, b4 = -100)
ab=c(-4869,-464)

m=nls(frm,data, start, nls.control(maxiter = 1000))
data$f=predict(m, data$x)

plot(data$x, data$f, col="blue",type="l")
points(data$x, data$y)

dst_an=distance(data,ab)
dst_an$dst


