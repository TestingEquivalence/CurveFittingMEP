library(boot)

bootstrapSD<-function(m){
  data=m$data
  #calculate bootstrap volatility
  vol.fun<-function(dat,ind){
    tryCatch({
      m$data=dat[ind,]
      nm=updateModel(m)
      return(nm$distance)
    }, error = function(e){
      return(NA)
    })
  }
  
  res=boot(data,vol.fun,R=m$nSimulation)
 
  return(sd(res$t, na.rm=TRUE))
}

asymptoticTestBootstrapVariance<-function(m){

  vol = bootstrapSD(m)
  qt=qnorm(1-m$alpha,0,1)

  min_eps = m$distance + qt*vol
  return(min_eps)
}
