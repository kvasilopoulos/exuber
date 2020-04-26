# filter(ds_tbl, id == nm) %>%
#   pull(tstat) -> y
#
# filter(ds_tbl, id == nm) %>%
#   pull(ds_lgl) %>%
#   which() -> x
#
# start <- x[c(TRUE, diff(x) != 1)] # diff reduces length by 1
# end <- x[c(diff(x) != 1, TRUE)]
# end[end - start == 0] <- end[end - start == 0] + 1
#
# peak <- numeric()
# for(i in length(start)) {
#   interval <- y[start[i]:end[i]]
#   peak <- start[i] + which.max(interval)
#   print(peak)
# }
