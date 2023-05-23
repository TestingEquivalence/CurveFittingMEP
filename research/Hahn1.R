library(NISTnls)
Try <- function(expr) if (!inherits(val <- try(expr), "try-error")) val

Try(fm <- nls(y ~ (b1+b2*x+b3*x**2+b4*x**3) / (1+b5*x+b6*x**2+b7*x**3),
               data = Hahn1, trace = TRUE,
               start = c(b1 = 10, b2 = -1, b3 = 0.05,
                         b4 = -0.00001, b5 = -0.05, b6 = 0.001, b7 = -0.000001)))

y=predict(fm)

df=data.frame(x=Hahn1$x, y)
df=df[!duplicated(df$x),]
df=df[order(df$x),]

plot(y ~ x, data = Hahn1)
lines(df$x,df$y, col="blue")
