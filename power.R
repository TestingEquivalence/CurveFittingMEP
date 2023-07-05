numericDistance<-function(m,f,dx){
  x=seq(from=m$ab[1], to=m$ab[2], by=dx)
  y1=predict.m(m,x)
  y2=f(x)
  
  y1=cumsum(y1)
  y2=cumsum(y2)
  
  dy=y1-y2
  dy=dy*dx
  dy=dy*dy
  
  res=sum(dy)*dx
  return(res)
}
