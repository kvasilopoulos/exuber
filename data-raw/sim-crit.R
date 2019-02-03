
# options("exuber.parallel" = TRUE)
# options("exuber.show_progress" = FALSE)

# Simulation -------------------------------------------------------------

nn <- 2000
crit <- list()
for (i in 6:nn) {
  crit[[i]] <- mc_cv(i)
}
names(crit) <- c(paste0("_nan", 1:5), c(paste0("n", 6:nn)))
class(crit) <- c("list", "crit")


# change critical values badf_cv from simulated to fixed ------------------

for (n in 6:nn) { #length(crit)
  minw <- floor((0.01 + 1.8 / sqrt(n)) * n)
  temp <- log(log(n*seq(minw + 1, n)))/100
  new_cv <- matrix(rep(temp, 3), ncol = 3,
                   dimnames = list(NULL, c("90%", "95%", "99%")))

  crit[[n]]$"badf_cv" <- new_cv
  attr(crit[[n]], "opt_badf") <- "fixed"
}

# ... ---------------------------------------------------------------------

# usethis::use_data(crit, overwrite = TRUE)
