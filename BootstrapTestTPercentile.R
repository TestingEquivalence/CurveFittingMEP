tPercentileBootstrapTest<-function(m){
  dst=m$distance
  stDev=bootstrapSD(m)
  data=m$data

  #calculate bootstrap distribution
  t.fun<-function(dat,ind){
    m$data=dat[ind,]
    nm=updateModel(m)
    dstBst=nm$distance
    stDevBst=bootstrapSD(nm)
    return((dstBst-dst)/stDevBst)
  }

  res=boot(m$data,t.fun,R=m$nSimPercentileTBootstrap)

  #calculate quantile of bootstrap distribution
  qt=quantile(res$t,m$alpha,type=1)
  min_eps=dst-stDev*qt
  return(min_eps)
}