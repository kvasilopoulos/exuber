
options("exuber.parallel" = TRUE)
options("exuber.show_progress" = FALSE)
options("exuber.global_seed" = NULL)


# Simulation -------------------------------------------------------------

nn <- 20
crit <- list()
for (i in 6:nn) {
  crit[[i]] <-  mc_cv(i, seed = 123)
  print(i)
}
names(crit) <- c(paste0("_nan", 1:5), c(paste0("n", 6:nn)))
class(crit) <- c("list", "crit")

# ... ---------------------------------------------------------------------

usethis::use_data(crit, overwrite = TRUE , compress = "xz")

all.equal(mc_cv(20, nrep = 20, seed = 123), mc_cv(20, nrep = 20, seed = 123))
all.equal(mc_cv(20, seed = 123), crit[[20]])
