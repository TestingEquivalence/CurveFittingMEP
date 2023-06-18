library(boot)

bootstrapSD<-function(m){
  data=m$data
  #calculate bootstrap volatility
  vol.fun<-function(dat,ind){
    m$data=dat[ind,]
    nm=updateModel(m)
    return(nm$distance)
  }
  
  res=boot(data,vol.fun,R=m$nSimulation)
 
  return(sd(res$t))
}

# asymptoticTestBootstrapVariance<-function(parameter){
#   
#   dst=distance(parameter$x)
#   
#   n=length(parameter$x)
#   vol = bootstrapVolatility(parameter$x,parameter$nSimulation)
#   qt=qnorm(1-parameter$alpha,0,1)
#   
#   min_eps = dst + qt*vol
#   # res=list()
#   # res$distance=dst
#   # res$min_eps=min_eps
#   return(min_eps)
# }
