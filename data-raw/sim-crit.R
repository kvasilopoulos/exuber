
options("exuber.parallel" = TRUE)
options("exuber.show_progress" = FALSE)
options("exuber.global_seed" = NULL)

# Simulation -------------------------------------------------------------

nn <- 700
crit <- list()
for (i in 6:nn) {
  crit[[i]] <- mc_cv(i, seed = 123)
  print(i)
}
names(crit) <- c(paste0("_nan", 1:5), c(paste0("n", 6:nn)))
class(crit) <- c("list", "crit")

# ... ---------------------------------------------------------------------

usethis::use_data(crit, overwrite = TRUE , compress = "xz")

