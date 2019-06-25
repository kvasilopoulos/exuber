
options("exuber.parallel" = TRUE)
options("exuber.show_progress" = FALSE)

# Simulation -------------------------------------------------------------

nn <- 400
crit <- crit
for (i in 301:nn) {
  crit[[i]] <- mc_cv(i, seed = 123, opt_bsadf = "conservative")
  print(i)
}
names(crit) <- c(paste0("_nan", 1:5), c(paste0("n", 6:nn)))
class(crit) <- c("list", "crit")

# ... ---------------------------------------------------------------------

usethis::use_data(crit, overwrite = TRUE, compress = "xz")
