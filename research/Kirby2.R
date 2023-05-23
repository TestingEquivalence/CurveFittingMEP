library(NISTnls)

Try <- function(expr) if (!inherits(val <- try(expr), "try-error")) val

Try(fm <- nls(y ~ (b1 + b2*x + b3*x**2) / (1 + b4*x + b5*x**2),
               data = Kirby2, trace = TRUE,
               start = c(b1 = 2, b2 = -0.1, b3 = 0.003,
                         b4 = -0.001, b5 = 0.00001)))

y=predict(fm)

df=data.frame(x=Kirby2$x, y)
df=df[!duplicated(df$x),]
df=df[order(df$x),]

plot(y ~ x, data = Kirby2)
lines(df$x,df$y, col="blue")
