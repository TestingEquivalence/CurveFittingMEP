library(nlsMicrobio)
#View(L.minor)
source("distance.R")

# conc is random
L.minor.m1 <- nls(rate ~ Vm*conc/(K+conc), data = L.minor, start = list(K=20, Vm=120))
confint2(L.minor.m1)

plot(L.minor$conc,L.minor$rate)
lines(L.minor$conc,predict(L.minor.m1), col="blue")

df=list()
df$x=L.minor$conc
df$y=L.minor$rate
df=as.data.frame(df)
df$f=fitted(L.minor.m1)

#df=df[1:3,]
ab=c(0,205)
dst_an=distance(df,ab)
dst_an$dst
sum(dst_an$v^2)

df=df[order(df$x),]
df$cy=cumsum(df$y)
df$cf=cumsum(df$f)
df$dcf=(df$cy-df$cf)^2

ff=stepfun(df$x,c(0,df$dcf))
dst_num=integrate(ff,ab[1],ab[2])$value
dst_num

# look at residuals

residuals(L.minor.m1)
df$res=df$y-df$f

plot(df$x,df$res)
hist(df$res)

