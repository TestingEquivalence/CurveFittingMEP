source("distance.R")
library(minpack.lm)

none=""
asymptoticBV="asymptoticBootstrapVariance"
tPercentileBootstrap="tPercentileBootstrap"

MDE="MDE"
LSE="LSE"

curveFittingMEP<-function(frm,data, test, ab, start,alpha=0.05,
                          nSimulation=200, method=LSE){
  
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
  m$distance=distance(df,m$ab)$dst
  
  return(m)
}

updateModelMDE<-function(m){
  model.nls=nls(m$formula,m$data, m$start)

  
  df=list()
  df$x=m$data$x
  df$y=m$data$y
  df$f=predict(m$model)
  df=as.data.frame(df)
  m$distance=distance(df,m$ab)$dst
  
  return(m)
}

fn<-function(x){
  for (key in names(x)){
    data[[key]]=x[[key]]
  }
  data$f=with(data,eval(rhs.frm))
  dst=distance(data,ab)
  return(dst$v)
}