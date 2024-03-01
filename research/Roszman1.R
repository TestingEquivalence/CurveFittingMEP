library(NISTnls)
source("distance.R")
source("curveFittingMEP.R")
source("size.R")
source("power.R")

data=Roszman1
data=data[order(data$x),]
frm=y ~ b1 - b2*x - atan(b3/(x-b4))/pi
start=c(b1 = 0.1, b2 = -0.00001, b3 = 1000, b4 = -100)
ab=c(-4869,-464)


# fitting model and test
method=LM
m=curveFittingMEP(frm,data,none, ab, start, method)
m$distance
write.csv(m$coef,"coef.csv")

plot(m$data$x, m$prediction-m$data$y)
plot(m$data$x, m$prediction, col="blue",type="l")
points(m$data$x,m$data$y)

set.seed(10071977)
m=curveFittingMEP(frm,data,asymptoticBV, ab, start, method, nSimulation = 1000)
m$min.epsilon

set.seed(10071977)
m=curveFittingMEP(frm,data,tPercentileBootstrap, ab, start, method, nSimulation = 1000, 
                  nSimPercentileTBootstrap = 1000)
m$min.epsilon

# bootstrap coefficients

m=curveFittingMEP(frm,data,none, ab, start, method = NLS)
set.seed(10071977)
res=bootstrapCoef(m,1000)
write.csv(res,"bst_coef_NLS.csv")

m=curveFittingMEP(frm,data,none, ab, start, method = LM)
set.seed(10071977)
res=bootstrapCoef(m,1000)
write.csv(res,"bst_coef_LM.csv")
 
# power at the model LM LSE only
m=curveFittingMEP(frm,data, asymptoticBV, ab, start, method = LM, nSimulation = 1000, nSimPercentileTBootstrap = 200)
resPower=list()
for (xKey in names(xSampler)){
  for (errKey in names(errSampler)){
    resPower[[paste(xKey,errKey,sep="_")]]=powerAtModel(m,nSim=1000, xSampler[[xKey]], errSampler[[errKey]])  
  }
}

write.csv(resPower,"resSizeATBT1000.csv")

# power at the boundary points based on sin(omega*x)
vOmega=c(0.5, c(1:10))
vXSampler=c()
vErrSampler=c()
vPower=c()
vDst=c()
vW=c()
vOm=c()

for (omega in vOmega){
  fsin<-function(x){
    res=sin(2*pi*omega*(x-ab[1])/(ab[2]-ab[1]))
    return(res)
  }
  
  eps=0.0010
  dx=(ab[2]-ab[1])/1000
  
  m=curveFittingMEP(frm,data,asymptoticBV, ab, start, method = LM, nSimulation = 200)
  w=linearBoundaryPoint(m,fsin,dx,eps,0.92,0.99999)
  
  
  f<-function(x){
    linearPoint(m,fsin,w,x)
  } 
  
  dst=numericDistance(m,f,dx)
  
  for (xKey in c("xSamplerBootstrap","xSamplerUniform")){
    for (errKey in c("errSamplerBootstrap","errSamplerNormal")){
      pw=powerAtPoint(m,f,nSim=10, xSampler[[xKey]], errSampler[[errKey]],eps)
      vW=c(vW,w)
      vPower=c(vPower,pw)
      vXSampler=c(vXSampler,xKey)
      vErrSampler=c(vErrSampler,errKey)
      vOm=c(vOm, omega)
      vDst=c(vDst,dst)
    }
  }
}

res=data.frame(omega=vOm, xSampler=vXSampler, errSampler=vErrSampler,
               power=vPower, distance=vDst, w=vW)
write.csv(res,"resPower_AT200_eps001.csv")


