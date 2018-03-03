# rm(list=ls())
# .rs.restartR()
# source("C:/Users/T460p/Desktop/Rpackage/rtadf.R")

set.seed(123)
a1 <- sim_dgp1(200) # one bubble
a2 <- sim_dgp2(200) # two bubbles
a3 <- sim_ar(200)   # explosive ar(1)
a4 <- sim_blan(200) # blanchard model
a5 <- sim_evans(200) # evans model
a6 <- sim_div(200) # dividends

dfrm <- data.frame(a1, a2, a3, a4, a5, a6)
colnames(dfrm) <- c("oneb", "twob", "ar", "blan", "evans", "div")
plot.ts(dfrm)

system.time(ts <- radf(dfrm, lag = 1))
system.time(mc <- mc_cv(NROW(dfrm), 1000, parallel = T))
system.time(wb <- wb_cv(dfrm, 1000, parallel = T))

summary(ts, mc)
diagnostics(ts, mc)
date.stamp(ts, mc)

library(ggplot2)
plot(ts, mc, plot.type = "single", breaks_x = 20) + ggtitle("Hello There")

library(grid);library(gridExtra)
p1 <- plot(ts, mc, breaks_x = 20)
do.call(grid.arrange, c(p1, ncol = 3))
