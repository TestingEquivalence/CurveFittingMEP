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