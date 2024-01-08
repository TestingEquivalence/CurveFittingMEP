source("distance.R")
source("asymptoticTestBootstrapVariance.R")
source("BootstrapTestTPercentile.R")
library(minpack.lm)
library(formula.tools)

none=""
asymptoticBV="asymptoticBootstrapVariance"
tPercentileBootstrap="tPercentileBootstrap"

LM="LM"
NLS="NLS"

curveFittingMEP<-function(frm,data, test, ab, start,  method=LSE, alpha=0.05,
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
  if (m$method==NLS){
    m=updateModelNLS(m)
  }
  if (m$method==LM){
    m=updateModelLM(m)
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

updateModelNLS<-function(m){
  #LSE regression
  m$model <-  nls(m$frm,m$data, m$start, nls.control(maxiter = 1000))

  df=list()
  df$x=m$data$x
  df$y=m$data$y
  df$f=predict(m$model)
  df=as.data.frame(df)
  m$prediction=df$f
  m$distance=distance(df,m$ab)
  m$coef=coef(m$model)
  
  return(m)
}

updateModelLM<-function(m){
  rhs.frm=rhs(m$frm)
  
  fn<-function(x){
    data=data.frame(x=m$data$x)
    
    #add model coefficients
    for (key in names(x)){
      data[[key]]=x[[key]]
    }
    v=with(data,eval(rhs.frm))
    v=v-m$data$y
    return(v)
  }
  
  res=nls.lm(par=m$start, fn=fn)
  
  m$model=res
  m$prediction=fn(res$par)+m$data$y
  m$coef=res$par
  
  data=data.frame(x=m$data$x)
  data$y=m$data$y
  data$f=m$prediction
  m$distance=distance(data,m$ab)
  
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