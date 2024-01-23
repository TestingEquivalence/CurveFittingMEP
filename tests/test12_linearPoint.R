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

omega=1/2
f<-function(x){
  res=sin(2*pi*omega*(x-ab[1])/(ab[2]-ab[1]))
  return(res)
}

df=list()
# dx=(ab[2]-ab[1])/1000
dx=1
x=seq(from=ab[1], to=ab[2], by=dx)
df$x=x

w=1
df$m=linearPoint(m,f,w,x)
w=0.91
df$f=linearPoint(m,f,w,x)

df=as.data.frame(df)
ggplot(df, aes(x = x)) +
  geom_point(aes(y = m, color = "red"), size = 1) +
  geom_line(aes(y = f, color = "blue"), size = 1)

dfh=data.frame(x=df$x,y=df$f, f=df$m)
distance(dfh,ab)

m$data=dfh
mm=updateModel(m)
mm$distance

df$f1=predict.m(mm,df$x)

ggplot(df, aes(x = x)) +
  # geom_point(aes(y = m, color = "red"), size = 1) +
  geom_line(aes(y = f, color = "blue"), size = 1) +
  geom_line(aes(y = f1, color = "green"), size = 1)
