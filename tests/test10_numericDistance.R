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



bf<-function(x){
  return(m$coef[2]*x/(m$coef[1]+x))
}

f<-function(x){
  return(1+bf(x))
}

f<-function(x){
  omega=1
  w=0.001
  w*sin(2*pi*omega*(x-ab[1])/(ab[2]-ab[1]))+(1-w)*bf(x)
}

numericDistance(m,f,0.01)
(205^3)/3
