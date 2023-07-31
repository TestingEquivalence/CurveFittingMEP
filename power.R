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

linearBoundaryPoint<-function(m,f,dx,eps){
  
  aim<-function(w){
    #print(a)
    lc=a*interiorPoint+(1-a)*exteriorPoint
    nmdr=updateMinDistanceModel(lc,mdr)
    return(nmdr$min.distance-eps)
  }
  
  a=uniroot(aim, c(0,1))$root
  lc=a*interiorPoint+(1-a)*exteriorPoint
  nmdr=updateMinDistanceModel(lc,mdr)
  return(lc)
}