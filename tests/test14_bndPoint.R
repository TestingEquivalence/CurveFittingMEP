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

fsin<-function(x){
  omega=2
  res=sin(2*pi*omega*(x-ab[1])/(ab[2]-ab[1]))
  return(res)
}

eps=1e-6
dx=10

bw=linearBoundaryPoint(m,fsin,dx,eps,0.92,0.99)

w=bw

f<-function(x){
  linearPoint(m,fsin,w,x)
}


numericDistance(m,f,dx)

