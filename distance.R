distance<-function(df,ab){
  df=df[order(df$x),]
  sf=0
  sy=0
  dst=0
  n=nrow(df)
  v=rep(0,n)
  
  for(i in c(1:n)){
    if (i==n){
      dx=ab[2]-df$x[i]
    }
    else{
      dx=df$x[i+1]-df$x[i]
    }
    sf=sf+df$f[i]
    sy=sy+df$y[i]
    dst=dst+((sf-sy)^2)*dx
    v[i]=(sf-sy)*sqrt(dx)
  }
  
  ls=list()
  ls$v=v
  ls$dst=dst/(n*n*(ab[2]-ab[1]))
  
  return(ls)
}

