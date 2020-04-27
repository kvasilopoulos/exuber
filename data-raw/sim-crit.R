
options("exuber.parallel" = TRUE)
options("ncores" = parallel::detectCores())
options("exuber.show_progress" = FALSE)

# Simulation -------------------------------------------------------------

nn <- 600
radf_crit <- vector(list, nn)
for (i in 6:nn) {
  radf_crit[[i]] <-  radf_mc_cv(i, seed = 123, nrep = 2000)
  print(i)
}
names(radf_crit) <- c(paste0("_nan", 1:5), paste0("n", 6:nn))
class(radf_crit) <- c("crit", "list")


# test and store ----------------------------------------------------------

is_equal <- all.equal(radf_crit[[100]], radf_mc_cv(100, nrep = 2000L, seed = 123))

if (isTRUE(is_equal)) {
  usethis::use_data(radf_crit, overwrite = TRUE , compress = "xz")
}

for (i in 6:600) {
  store_attrs <- attributes(radf_crit[[i]])
  attributes(radf_crit[[i]]) <- NULL
  attr(radf_crit[[i]], "index") <- 1:i
  attributes(radf_crit[[i]]) <- c(attributes(radf_crit[[i]]), store_attrs)
  class(radf_crit[[i]]) <- c("radf_cv", "mc_cv", "cv", "list")
}
