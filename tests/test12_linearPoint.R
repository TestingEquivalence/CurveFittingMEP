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
  omega=1
  res=sin(2*pi*omega*(x-ab[1])/(ab[2]-ab[1]))
  return(res)
}

x=seq(from=ab[1], to=ab[2], by=10)

w=0.97
y=linearPoint(m,f,w,x)
plot(x,y)
