
options("exuber.parallel" = TRUE)
options("ncores" = parallel::detectCores())
options("exuber.show_progress" = FALSE)

# Simulation -------------------------------------------------------------

nn <- 599
# pkg_radf_crit <- list()
# for (i in 6:nn) {
#   pkg_radf_crit[[i]] <-  mc_cv(i, seed = 123)
#   print(i)
# }
names(radf_crit) <- c(paste0("_nan", 1:5), paste0("n", 6:nn))
# class(pkg_radf_crit) <- c("list", "crit")


radf_crit <- exuberdata::radf_crit2[1:700]
class(radf_crit) <- c("list", "crit")

# ... ---------------------------------------------------------------------

usethis::use_data(radf_crit, overwrite = TRUE , compress = "xz")

radf_crit <- crit
for (i in 1:599) {
  pcnt <- c(0.9, 0.95, 0.99)
  asy_adf_crit <- rep(
    c(-0.44, -0.08, 0.6),
    each = i - psy_minw(i)
  )
  badf_crit <- matrix(
    asy_adf_crit, ncol = 3,
    dimnames = list(NULL, paste0(pcnt*100, "%"))
  )
  radf_crit[[i]]$badf_cv <- badf_crit
  attr(radf_crit[[i]], "opt_badf") <- NULL
  attr(radf_crit[[i]], "opt_bsadf") <- NULL
}
