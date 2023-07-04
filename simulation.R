library(parallel)

simulatePowerAtBoundary<-function(m, points, epsilon, orderName, nSimulation=1000){
  
  cl=getCluster()
  i=c(1:nPoints)
  power=parSapply(cl,i, persistentSimulatePowerAtPoint, test=test, nSimulation=nSimulation,
                  n=n, epsilon=epsilon, orderName=orderName)
  stopCluster(cl)
  
  # power=rep(0,nPoints)
  # for (i in c(1:nPoints)){
  #   power[i]=persistentSimulatePowerAtPoint(test,nSimulation,n,epsilon,i,orderName)
  # }
  
  for (i in c(1:nPoints)){
    fname=paste0("r",i,".csv")
    fname=file.path("power",fname)
    file.remove(fname)
  }
  return(power)
}

# Calculate the number of cores
getCluster<-function(){
  no_cores <- detectCores() - 1
  
  # Initiate cluster
  cl <- makeCluster(no_cores,'SOCK')
  clusterExport(cl,c())
  
  return(cl)
}
