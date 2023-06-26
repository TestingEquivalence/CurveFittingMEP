bootstrapCoef<-function(m,nSim){
  res=list()
  res[["dst"]]=list()
  for (key in names(m$coef)){
    res[[key]]=list()
  }
  
  data=m$data
  
  for (i in c(1:nSim)){
    ind=sample.int(nrow(data), replace = TRUE)
    m$data=data[ind,]
    nm=updateModel(m)
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