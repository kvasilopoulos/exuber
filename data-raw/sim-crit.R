
nn <- 500
crit <- list()
for (i in 6:nn) {
  invisible(capture.output(crit[[i]] <- mc_cv(i, parallel = TRUE)))
}
names(crit) <- c(paste0("_nan", 1:5), c(paste0("n", 6:nn)))
class(crit) <- c("list", "crit")


# update ------------------------------------------------------------------

update_by <- 1200
# we cant get dim of list hence
n_start <- length(names(crit)) + 1
n_end <- n_start + update_by

for (i in n_start:n_end) {
  invisible(capture.output(crit[[i]] <- mc_cv(i, parallel = TRUE)))
  names(crit)[i] <- paste0("n", i)
  print(i)
}

# ... ---------------------------------------------------------------------

usethis::use_data(crit, overwrite = TRUE)

