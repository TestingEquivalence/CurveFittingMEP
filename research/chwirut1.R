library(NISTnls)

Try <- function(expr) if (!inherits(val <- try(expr), "try-error")) val

Try(fm <- nls(y ~ exp(-b1*x)/(b2+b3*x), data = Chwirut1, trace = TRUE,
               start = c(b1 = 0.1, b2 = 0.01, b3 = 0.02)))
y=predict(fm)

df=data.frame(x=Chwirut1$x, y)
!duplicated(v)
df=df[!duplicated(df$x),]
df=df[order(df$x),]

plot(y ~ x, data = Chwirut1)
lines(df$x,df$y, col="blue")
