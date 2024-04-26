numericDistance<-function(m,f,dx){
  x=seq(from=m$ab[1],to=m$ab[2],by=dx)
  y=f(x)
  df=data.frame(x=x,y=y)
  m$data=df
  m=updateModel(m)
  return(m$distance)
}

randomPiecewiseLinear<-function(m, xSampler, errSampler,n){
  x=xSampler(m,n-2)
  x=c(x,m$ab)
  
  y=errSampler(m,n)
  f=approxfun(x,y)
  
  return(f)
}

linearPoint<-function(m,f,w,x){
  my=predict.m(m,x)
  fy=f(x)
  y=w*my+(1-w)*fy
  return(y)
}

linearBoundaryPoint<-function(m,f,dx,eps,minW,maxW){
  
  aim<-function(w){
    wf<-function(x){
      linearPoint(m,f,w,x)
    }
    
    dst=numericDistance(m,wf,dx)
    return(dst-eps)
  }
  
  w=uniroot(aim, c(minW,maxW))$root
  return(w)
}

powerAtPoint<-function(m,f,nSim, xSampler,errSampler,eps, orderName="temp"){
  res=rep(0,nSim)
  dfs=list()
  
  #prepare
  if(!dir.exists(orderName)){
    dir.create(orderName)
  }
  
  #generate new data
  set.seed(10071977)
  for (i in c(1:(2*nSim))){
    n=nrow(m$data)
    x=xSampler(m, n)
    y= f(x)
    err=errSampler(m, n)
    y=y+err
    dfs[[i]]=data.frame(x=x, y=y )
  }
  
  j=1
  i=1
  while(i<=nSim){
    fname=paste0("r",i,".rds")
    fname=file.path(orderName,fname)
    
    if (file.exists(fname)){
      res[i]=readRDS(fname)
      print(i)
      i=i+1
      j=j+1
    }
    else{
      tryCatch({
        m$data=dfs[[j]]
        nm=updateModel(m)
        nm=updateTests(nm)
        res[i]=nm$min.epsilon
        saveRDS(res[i],fname)
        print(i)
        i=i+1
        j=j+1
      }, error = function(e){
        j<<-j+1
        print("error")
        print(j)
      })
    }
  }
  
  unlink(orderName,recursive = TRUE)
  
  return(sum(res<=eps)/nSim)
}
