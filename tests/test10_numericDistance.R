library(NISTnls)
source("distance.R")
source("curveFittingMEP.R")
source("size.R")
source("power.R")


data=Roszman1
data=data[order(data$x),]
frm=y ~ b1 - b2*x - atan(b3/(x-b4))/pi
start=c(b1 = 0.1, b2 = -0.00001, b3 = 1000, b4 = -100)
ab=c(-4869,-464)

m=curveFittingMEP(frm,data,none, ab, start, method=LSE)



f<-function(x){
  predict.m(m,x)+0.1*rnorm(length(x))
}




numericDistance(m,f,10)
