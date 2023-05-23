plot(conc ~ time, data = Lipo,
     xlab = "Time since drug administration (hr)",
     ylab = "Lipoprotein concentration",
     main = "Lipo data and fitted curve", las = 1)
fm1 <- nls(conc ~ SSbiexp(time, A1, lrc1, A2, lrc2), data = Lipo)
summary(fm1)
usr <- par("usr")
xx <- seq(usr[1], usr[2], len = 51)
lines(xx, predict(fm1, list(time = xx)))
title(sub = deparse(fm1$call$formula))
diff(Lipo$time)
