library(NRAIA)

str(BOD2)
# simplest form of fitting a first-order model to these data
(fm1 <- nls(demand ~ A*(1-exp(-exp(lrc)*Time)), data = BOD2,
            start = c(A = 2.2, lrc = log(0.25))))
coef(fm1)
# using the plinear algorithm
(fm2 <- nls(demand ~ (1-exp(-exp(lrc)*Time)), data = BOD2,
            start = c(lrc = log(0.25)), algorithm = "plinear", trace = TRUE))
# using a self-starting model
(fm3 <- nls(demand ~ SSasympOrig(Time, A, lrc), data = BOD2))
plotfit(fm3, xlab = "Time (days)",
        ylab = "Biochemical Oxygen Demand")
