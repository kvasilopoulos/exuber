## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load_libraries, message=FALSE, warning=FALSE------------------------
library(exuber)
library(tidyverse)

## ----generate simulated data, eval = TRUE--------------------------------
# need to add explanation for what each of these is
# n <- the sample size
n <- 400
mu <- 0.0024
# this is the value of sigma squared in the paper need to check if code takes sigma (i.e. do i need to take sqroot)
sigma <- 0.0010
rho <- 0.985
# check this is the correct understanding of rho to r transformation
r <- 1/rho
# the value which the bubble returns to when it collapses
delta <- 1
b1 <- 0.50
pi <- 0.85
xi <- 0.5
# the scaling factor for the bubble
kap <- 20
set.seed(123) # find a nice seed for illu purposes
# The fundamental value from the Lucas pricing model
pf <- sim_div(100)
# the evans bubble term
b <- sim_evans(100)
# the simulated time series
p <- pf + kap*b
  

## ----plot simulation, eval = TRUE, out.width = '80%', fig.align = "center"----
plot(p, xlab = "", ylab = "")
# need to add actual plot below it to show it looks similar to PSY(2015)

## ----test simulated data, eval = FALSE-----------------------------------
#  test <- radf(p)

## ----load data, eval = FALSE---------------------------------------------
#  dta <- sp500 %>% select(Date, Ratio)

## ----plot data, eval = TRUE----------------------------------------------
ggplot(sp500, aes(y = Ratio, x = Date)) + geom_line()

## ----radf, eval = FALSE--------------------------------------------------
#  testSP500 <- radf(sp500 %>% select(PVratio))

## ----crit values, eval = FALSE-------------------------------------------
#  critSP500 <- mc_cv(NROW(sp500), nrep = 2000, parallel = T)

## ----report--------------------------------------------------------------
report(testSP500, critSP500)

## ----plot,  eval = TRUE, out.width = '80%', fig.align = "center"---------
plot(testSP500, critSP500, breaks_x = "10 years", format_date = "%Y")

