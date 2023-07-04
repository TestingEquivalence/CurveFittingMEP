randomExteriorPoint<-function(m,xSampler, errSampler,eps){
  repeat{
    x=xSampler(m)
    y=errSampler(m)
    m$data=data.frame(x,y)
    nm=updateModel(m)
    if (m$distance>eps) return(m$data)
  }
}
boundaryPoint<-function(m,df,eps){
  
}