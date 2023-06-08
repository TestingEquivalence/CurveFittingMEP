library(NISTnls)

data=Hahn1
data=data[order(data$x),]
frm="y ~ (b1+b2*x+b3*x**2+b4*x**3) / (1+b5*x+b6*x**2+b7*x**3)"
start=c(b1 = 10, b2 = -1, b3 = 0.05,
        b4 = -0.00001, b5 = -0.05, b6 = 0.001, b7 = -0.000001)

model.nls <- nls(frm,data, start)

plot(data$x,data$y)
lines(data$x,predict(model.nls), col="blue")

# residuals
data$f=predict(model.nls)
data$res.nls=data$f-data$y
plot(data$x,data$res.nls)
hist(data$res.nls)
