source("distance.R")
library(minpack.lm)

none=""
asymptoticBV="asymptoticBootstrapVariance"
tPercentileBootstrap="tPercentileBootstrap"

curveFittingMEP<-function(formula,data, test, alpha=0.05,
                        nSimulation=200){
  
  #initial information
  ls=list()
  ls$data=data
  ls$formula=as.formula(formula)
  ls$frm=formula
  ls$alpha=alpha
  ls$test=test
  ls$nSimulation=nSimulation
 
  # #logit regression for initial values
  # lr <- glm(mdr$formula,mdr$data, family = quasibinomial("logit"), weights =mdr$weights)
  # 
  # # dummy model for technical reasons
  # md= lm(mdr$frm, mdr$data)
  # y=all.vars(as.formula(mdr$frm))[1]
  # 
  # 
  # # logistic model for given parameters
  # w=mdr$weights/mdr$n
  # distance<-function(coef){
  #   md$coefficients=coef
  #   l=predict.lm(md,mdr$data)
  #   (logistic(l)-mdr$data[[y]])*w
  # }
  # 
  # # calculate minimum distance estimator
  # 
  # res=nls.lm(par=lr$coefficients, fn=distance)
  # mdr$result.nls.lm=res
  # 
  # # calculate min distance
  # mdr$min.distance=sqrt(deviance(res))
  # mdr$coefficients=coef(res)
  # 
  # # calculate fitted
  # md$coefficients=coef(res)
  # l=predict.lm(md,mdr$data)
  # mdr$fitted=logistic(l)
  #  
  # # easy access to other data 
  # mdr$y=mdr$data[[y]]
  # mdr$w=mdr$weights/mdr$n
  # 
  # # test results
  # mdr$min.epsilon=NA
  # 
  # if (asymptotic==test) {
  #   mdr$min.epsilon=asymptoticTest(mdr=mdr)
  # }
  # 
  # if (asymptoticBootstrapVariance==test){
  #   mdr$min.epsilon=asymptoticTestBootstrapVariance(mdr,nSimulation)
  # }
  # 
  # if (empiricalBootstrap==test){
  #   mdr$min.epsilon=empiricalBootstrapTest(mdr,nSimulation)
  # }
  # 
  # if (tPercentileBootstrap==test){
  #   mdr$min.epsilon=tPercentileBootstrapTest(mdr,nSimulation)
  # }
  # 
  # return(ls)
}

# updateMinDistanceModel<-function(p,weights,mdr){
#   df=mdr$data
#   y=all.vars(as.formula(mdr$frm))[1]
#   df[[y]]=p
#   
#   nlr=min_dst_logit(mdr$frm,data=df,weights=weights,test = mdr$test, alpha = mdr$alpha,
#                     nSimulation = mdr$nSimulation)
#   return(nlr)
# }
