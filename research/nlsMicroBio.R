library(nlsMicrobio)

data(growthcurve4)
def.par <- par(no.readonly = TRUE)
par(mfrow = c(2,2))
plot(growthcurve4)
par(def.par)

