library(foreach)
library(doParallel)

powerAtModelP<-function(param){
  res=powerAtModel(m=param$m,nSim=param$nSim, xSampler =  param$xSampler,
                   errSampler = param$errSampler, orderName =  param$orderName)
  return(res)
}

simulatePowerAtModelP<-function(param, xKeys, errKeys){
  pList=list()
  i=1
  for (xKey in xKeys){
    for (errKey in errKeys){
      p=list()
      p$m=param$m
      p$nSim=param$nSim
      p$xSampler=xSampler[[xKey]]
      p$errSampler= errSampler[[errKey]]
      p$orderName=paste0(xKey,"_", errKey)
      pList[[i]]=p
      i=i+1
    }
  }
  
  
  cl = makeCluster(2)
  all_objects = ls(globalenv())
  clusterExport(cl, varlist = all_objects)
  clusterEvalQ(cl, {
    library(minpack.lm)
    library(formula.tools)
  })
  registerDoParallel(cl)
  
  results <- foreach(pList, .combine = c) %dopar% {
    powerAtModelP(p)
  }
  
  stopCluster(cl)
  
  return(res)
}

