source("distance.R")
source("asymptoticTestBootstrapVariance.R")
source("BootstrapTestTPercentile.R")
library(minpack.lm)
library(formula.tools)

none=""
asymptoticBV="asymptoticBootstrapVariance"
tPercentileBootstrap="tPercentileBootstrap"

MDE="MDE"
LSE="LSE"

curveFittingMEP<-function(frm,data, test, ab, start,  method, alpha=0.05,
                          nSimulation=200, nSimPercentileTBootstrap=0){
  
  #initial information
  m=list()
  data=data[order(data$x),]
  
  m$data=data
  m$frm=frm
  m$alpha=alpha
  m$test=test
  m$nSimulation=nSimulation
  m$start=start
  m$method=method
  m$ab=ab
  m$nSimPercentileTBootstrap=nSimPercentileTBootstrap
  
  #update model
  m=updateModel(m)
  m=updateTests(m)

  return(m)
}

updateModel<-function(m){
  if (m$method==LSE){
    m=updateModelLSE(m)
  }
  if (m$method==MDE){
    m=updateModelMDE(m)
  }
  
  return(m)
}

updateTests<-function(m){
  if (m$test==asymptoticBV){
    m$min.epsilon=asymptoticTestBootstrapVariance(m)
  }
  
  if (m$test==tPercentileBootstrap){
    m$min.epsilon=tPercentileBootstrapTest(m)
  }
  return(m)
}

updateModelLSE<-function(m){
  #LSE regression
  m$model <-  nls(m$frm,m$data, m$start, nls.control(maxiter = 1000))

  df=list()
  df$x=m$data$x
  df$y=m$data$y
  df$f=predict(m$model)
  df=as.data.frame(df)
  m$prediction=df$f
  m$distance=distance(df,m$ab)$dst
  m$coef=coef(m$model)
  
  return(m)
}

updateModelMDE<-function(m){
 
  model.nls=nls(m$frm,m$data, m$start, nls.control(maxiter = 1000))
  rhs.frm=rhs(m$frm)
  cf=coef(model.nls)
  
  data=m$data
  data=data[order(data$x),]
  
  fn<-function(x){
    for (key in names(x)){
      data[[key]]=x[[key]]
    }
    data$f=with(data,eval(rhs.frm))
    dst=distance(data,ab)
    return(dst$v)
  }
  
  res=nls.lm(par=cf, fn=fn)
  
  m$model=res
  m$distance=res$deviance
  m$coef=res$par
  for (key in names(res$par)){
    data[[key]]=res$par[[key]]
  }
  
  m$prediction=with(data,eval(rhs.frm))
  
  return(m)
}

predict.m<-function(m, x){
  data=data.frame(x)
  for (key in names(m$coef)){
    data[[key]]=m$coef[[key]]
  }
  
  rhs.frm=rhs(m$frm)
  y=with(data,eval(rhs.frm))
  return(y)
}