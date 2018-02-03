# rm(list=ls())
# .rs.restartR()
# source("C:/Users/T460p/Desktop/Rpackage/rtadf.R")

# library(Quandl)
# Quandl.api_key("sMcvTx8cd-zzewgSdERq")
# a1 <- Quandl("WIKI/AAPL.11", type = "zoo", collapse = "monthly")

library(quantmod)
getSymbols("AAPL")
a1 <- AAPL[,"AAPL.Close"]
a2 <- sim_ar(200)
a3 <- sim_rw(200)
dfrm <- data.frame(a1[1:200], a2, a3)
colnames(dfrm) <- c("AAPL","ar1","rw")
plot.ts(dfrm)

ts <- radf(dfrm, lag = 1)
mc <- mc_cv(NROW(dfrm), 10, parallel = T)

summary(ts, mc)
diagnostics(ts, mc)
date.stamp(ts, mc)
plot(ts, mc, breaks_x = "1 month")
plot(ts, mc, plot.type = "multiple", breaks_x = "1 month") + ggtitle("Hello There")

# Testing variables
x <- radf(dfrm, lag = 1)
y <- mc_cv(NROW(dfrm), 200, parallel = T)
y <- wb_cv(dfrm, 100, parallel = T)


summary(x, y)
diagnostics(x, y)
date.stamp(x, y)
plot(x, y)
p[[2]] + scale_x_date(date_breaks = "2 weeks", date_labels = "%m-%Y") + scale_y_continuous(breaks = seq(-3,5)) +
  ggtitle("RADF TEST MPLA MPLA")
do.call(grid.arrange,p[[2]])


summary(ts, wb)
diagnostics(ts, wb)
date.stamp(ts, wb)
plot(ts, wb)
