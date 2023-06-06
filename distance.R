distance<-function(df,ab){
  df=df[order(df$x),]
  sf=0
  sy=0
  dst=0
  n=nrow(df)
  
  for(i in c(1:(n-1))){
    sf=sf+df$f[i]
    sy=sy+df$y[i]
    dst=dst+((sf-sy)^2)*(df$x[i+1]-df$x[i])
  }
  
  #special case i=n
  sf=sf+df$f[n]
  sy=sy+df$y[n]
  dst=dst+((sf-sy)^2)*(ab[2]-df$x[n])
  
  return(dst)
}

