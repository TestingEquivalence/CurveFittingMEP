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

m=curveFittingMEP(frm,data,none, ab, start, method=NLS)
data$m=predict.m(m,data$x)
ggplot(data, aes(x = x)) +
  geom_point(aes(y = y, color = "red"), size = 1) +
  geom_line(aes(y = m, color = "blue"), size = 1) 


f<-function(x){
  predict.m(m,x)+0.1*rnorm(length(x))
}


df=list()
dx=10
df$x=seq(from=m$ab[1],to=m$ab[2],by=dx)
df$y=sapply(df$x,f)
df$m=predict.m(m,df$x)
df=as.data.frame(df)
ggplot(df, aes(x = x)) +
  geom_point(aes(y = y, color = "red"), size = 1) +
  geom_line(aes(y = m, color = "blue"), size = 1) 


m$data=df
mnew=updateModel(m)
df$mnew=predict.m(mnew, df$x)
ggplot(df, aes(x = x)) +
  geom_line(aes(y = m, color = "red"), size = 1) +
  geom_line(aes(y = mnew, color = "blue"), size = 1) 

numericDistance(m,f,10)

