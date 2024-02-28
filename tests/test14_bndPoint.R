library(NISTnls)
library(ggplot2)
source("distance.R")
source("curveFittingMEP.R")
source("size.R")
source("power.R")

data=Roszman1
data=data[order(data$x),]
frm=y ~ b1 - b2*x - atan(b3/(x-b4))/pi
start=c(b1 = 0.1, b2 = -0.00001, b3 = 1000, b4 = -100)
ab=c(-4869,-464)


m=curveFittingMEP(frm,data,none, ab, start, method=LM)

omega=7
fsin<-function(x){
  res=sin(2*pi*omega*(x-ab[1])/(ab[2]-ab[1]))
  return(res)
}

eps=0.0005
dx=(ab[2]-ab[1])/10000

w=linearBoundaryPoint(m,fsin,dx,eps,0.96,0.99)


f<-function(x){
  linearPoint(m,fsin,w,x)
}

dxnd=(ab[2]-ab[1])/1000
numericDistance(m,f,dxnd)

#show plots
df=list()
df$x=x=seq(from=ab[1], to=ab[2], by=dx)
df$m=linearPoint(m,fsin,1,df$x)
df$b=linearPoint(m,fsin,w,df$x)
df=as.data.frame(df)
ggplot(df, aes(x = x)) +
  geom_point(aes(y = m, color = "red"), size = 1) +
  geom_line(aes(y = b, color = "blue"), size = 1)


dfh=data.frame(x=df$x,y=df$b)
m$data=dfh
bm=updateModel(m)
bm$distance

df$nm=linearPoint(bm,fsin,1,df$x)
ggplot(df, aes(x = x)) +
  geom_line(aes(y = m, color = "red"), size = 1) +
  geom_line(aes(y = nm, color = "blue"), size = 1)+
  geom_line(aes(y = b, color = "green"), size = 1)
 
