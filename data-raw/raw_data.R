require("readxl")

raw.dat <- "data-raw/ie_data.xls"

dat <- read_excel(raw.dat, sheet = 3)

real.dividend <- as.numeric(as.matrix(dat[7:1686, 9]))

real.price <- as.numeric(as.matrix(dat[7:1686, 8]))

sp.500.ratio <- real.price / real.dividend

sp.500.ratio <- ts(sp.500.ratio, start = c(1871, 1), frequency = 12)

sp500ratio <- as.data.frame(sp.500.ratio)

#save(sp.500.ratio, file = "data/sp500ratio.rdata")

devtools::use_data(sp500ratio, overwrite = T)
