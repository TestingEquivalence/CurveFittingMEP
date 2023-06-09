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
  
  for (key in names(m$coef)){
    data[[key]]=m$coef[[key]]
  }
  
  #generate new data
  set.seed(10071977)
  for (i in c(1:(nSim+100))){
    n=nrow(data)
    data$x=xSampler(m, n)
    data$y=with(data,eval(rhs.frm))
    err=errSampler(m)
    data$y=data$y+err
    dfs[[i]]=data
  }
  
  j=1
  i=1
  while(i<=nSim){
    tryCatch({
      m$data=dfs[[j]]
      nm=updateModel(m)
      nm=updateTests(nm)
      res[i]=nm$min.epsilon
      print(i)
      i=i+1
      j=j+1
    }, error = function(e){
      j<<-j+1
      print("error")
      print(j)
    })
  }
  return(res)
}

xSamplerUniform<-function(m,n){
  s=runif(n,m$ab[1],m$ab[2])
  return(s)
}

xSamplerBootstrap<-function(m,n){
  s=sample(m$data$x,size=n, replace=TRUE)
  return(s)
}

xSamplerSmoothBootstrap<-function(m,n){
  f<-function(u){
    as.numeric(quantile(m$data$x, type = 4, probs = u))
  }
  s=runif(n)
  s=sapply(s, f)
  return(s)
}

errSamplerBootstrap<-function(m,n){
  err=m$data$y-m$prediction
  s=sample(err,size = n, replace=TRUE)
  return(s)
}

errSamplerSmoothBootstrap<-function(m){
  err=m$data$y-m$prediction
  f<-function(u){
    as.numeric(quantile(err, type = 4, probs = u))
  }
  size=nrow(m$data)
  s=runif(size)
  s=sapply(s, f)
  return(s)
}

errSamplerNormal<-function(m){
  err=m$data$y-m$prediction
  sigma=sd(err)
  n=nrow(m$data)
  s=rnorm(n,mean=0,sd=sigma)
  return(s)
}

#  xSamplerFix and errSamplerWildBootstrap should be used always together

errSamplerWildBootstrap<-function(m){
  n=nrow(m$data)
  err=m$data$y-m$prediction
  s=rbinom(n, size = 1, prob = 0.5)
  s[s==0]= -1
  s=err*s
  return(s)
}

xSamplerFix<-function(m){
  return(m$data$x)
}