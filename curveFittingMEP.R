source("distance.R")
library(minpack.lm)
library(formula.tools)

none=""
asymptoticBV="asymptoticBootstrapVariance"
tPercentileBootstrap="tPercentileBootstrap"

MDE="MDE"
LSE="LSE"

curveFittingMEP<-function(frm,data, test, ab, start,alpha=0.05,
                          nSimulation=200, method){
  
  #initial information
  ls=list()
  data=data[order(data$x),]
  
  ls$data=data
  ls$frm=frm
  ls$alpha=alpha
  ls$test=test
  ls$nSimulation=nSimulation
  ls$start=start
  ls$method=method
  ls$ab=ab
  
  ls=updateModel(ls)
  
  #update model
 
  
  return(ls)
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

updateModelLSE<-function(m){
  
  #LSE regression
  m$model <-  nls(m$frm,m$data, m$start)

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
  model.nls=nls(m$frm,m$data, m$start)
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

