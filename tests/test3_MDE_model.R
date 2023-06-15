library(nlstools)
library(formula.tools)
source("distance.R")
source("curveFittingMEP.R")

# conc is random
data=list()
data$x=L.minor$conc
data$y=L.minor$rate
data=as.data.frame(data)
data=data[order(data$x),]


frm=y~Vm*x/(K+x)
start= list(K=20, Vm=120)
ab=c(0,205)
model=curveFittingMEP(frm,data,none, ab, start)

rhs.frm=rhs(model$frm)

cf=coef(model$model)

for (key in names(cf)){
  data[[key]]=cf[[key]]
}

exp=expression(Vm*x/(K+x))

with(data,Vm*x/(K+x))
predict(model$model)
with(data,eval(exp))
with(data,eval(tfrm))

res=nls.lm(par=cf, fn=fn)
v=fn(res$par)
sum(v*v)
res$deviance
