library(NISTnls)
Try <- function(expr) if (!inherits(val <- try(expr), "try-error")) val
plot(y ~ x, data = Roszman1)
Try(fm <- nls(y ~ b1 - b2*x - atan(b3/(x-b4))/pi, data = Roszman1,
               start = c(b1 = 0.1, b2 = -0.00001, b3 = 1000, b4 = -100),
               trace = TRUE))

y=predict(fm)

df=data.frame(x=Roszman1$x, y)
df=df[!duplicated(df$x),]
df=df[order(df$x),]

plot(y ~ x, data = Roszman1)
lines(df$x,df$y, col="blue")
