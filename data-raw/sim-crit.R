
options("exuber.parallel" = TRUE)
options("ncores" = parallel::detectCores())
options("exuber.show_progress" = FALSE)

# Simulation -------------------------------------------------------------

# nn <- 700
# pkg_radf_crit <- list()
# for (i in 6:nn) {
#   pkg_radf_crit[[i]] <-  mc_cv(i, seed = 123)
#   print(i)
# }
# names(pkg_radf_crit) <- c(paste0("_nan", 1:5), paste0("n", 6:nn))
# class(pkg_radf_crit) <- c("list", "crit")


pkg_radf_crit <- exuberdata::radf_crit[1:700]
class(pkg_radf_crit) <- c("list", "crit")

# ... ---------------------------------------------------------------------

usethis::use_data(pkg_radf_crit, overwrite = TRUE , compress = "xz")
