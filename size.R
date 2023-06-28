bootstrapCoef<-function(m,nSim){
  res=list()
  res[["dst"]]=list()
  for (key in names(m$coef)){
    res[[key]]=list()
  }
  
  data=m$data
  
  for (i in c(1:nSim)){
    repeat{
      tryCatch({
        ind=sample.int(nrow(data), replace = TRUE)
        m$data=data[ind,]
        nm=updateModel(m)
        break
      }, error=function(e){
        
      })
    }
    
    res[["dst"]]=c(res[["dst"]],nm$distance)
    for (key in names(nm$coef)){
      res[[key]]=c(res[[key]], nm$coef[[key]])
    }
  }
  
  for (key in names(res)){
    res[[key]]=unlist(res[[key]])
  }
  res=as.data.frame(res)
  return(res)
}

powerAtModel<-function(m,nSim, xSampler,errSampler){
  res=rep(0,nSim)
  dfs=list()
  rhs.frm=rhs(m$frm)
  data=m$data
  
  #generate new data
  for (i in c(1:nSim)){
    data$x=xSampler(m)
    data$y=with(data,eval(rhs.frm))
    err=errSampler(m,data)
    data$y=data$y+err
    dfs[[i]]=data
  }
  
  for (i in c(1:nSim)){
    m$data=dfs[[i]]
    nm=updateModel(m)
    nm=updateTests(nm)
    res[i]=nm$min.epsilon
  }
  return(res)
}

xSamplerUniform<-function(m){
  s=runif(nrow(m$data),m$ab[1],m$ab[2])
  return(s)
}

xSamplerBootstrap<-function(m){
  size=nrow(m$data)
  s=sample(m$data$x,size, replace=TRUE)
  return(s)
}

xSamplerSmoothBootstrap<-function(m){
  f<-function(u){
    as.numeric(quantile(m$data$x, type = 4, probs = u))
  }
  size=nrow(m$data)
  s=runif(size)
  s=sapply(s, f)
  return(s)
}

xSamplerFix<-function(m){
  return(m$data$x)
}

errSamplerBootstrap<-function(m){
  err=m$data$y-m$prediction
  size=nrow(m$data)
  s=sample(err,size, replace=TRUE)
  return(s)
}

errSamplerNormal<-function(m){
  err=m$data$y-m$prediction
  sigma=sd(err)
  n=nrow(m$data)
  s=rnorm(n,mean=0,sd=sigma)
  return(s)
}

errSamplerWildBootstrap<-function(m){
  n=nrow(m$data)
  err=m$data$y-m$prediction
  s=rbinom(n, size = 1, prob = 0.5)
  s[s==0]= -1
  s=err*s
  return(s)
}