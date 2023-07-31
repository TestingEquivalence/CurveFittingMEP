library(nlsMicrobio)
source("distance.R")
source("curveFittingMEP.R")
source("size.R")
source("power.R")

# conc is random
data=list()
data$x=L.minor$conc
data$y=L.minor$rate
data=as.data.frame(data)
data=data[order(data$x),]


frm=y ~ Vm*x/(K+x)
start= list(K=20, Vm=120)
ab=c(0,205)

m=curveFittingMEP(frm,data,none, ab, start, method=LSE)

f<-function(x){
  omega=1
  res=sin(2*pi*omega*(x-ab[1])/(ab[2]-ab[1]))
  return(res)
}

x=seq(from=0, to=205, by=1)

w=0.1
y=linearPoint(m,f,w,x)
plot(x,y)
