# Speed comparison: exuber::radf() vs. the two R alternatives used in
# Vasilopoulos, Pavlidis & Martinez-Garcia (2022, JSS) Section 4 --
# MultipleBubbles (a pure-R port of Phillips-Shi-Yu's GSADF) and
# psymonitor::PSY().
#
# The MultipleBubbles/psymonitor columns are NOT re-simulated here: they
# are read straight from the paper's own saved benchmark run
# (exuber-paper/bench.Rds, microbenchmark(times = 100) per sample size,
# read-only -- that repo is the published paper and is not modified by
# this script). Both are O(T^2) pure-R double loops with a regression
# call per window; at n = 1000 a *single* run already takes minutes on
# current hardware, so re-running microbenchmark(times = 100) for them
# here would take hours for no benefit -- their numbers don't depend on
# exuber's implementation.
#
# Only the exuber column is freshly simulated, against the current
# package version (2.0.0), and reported as "exuber 2.0.0". Medians are
# in milliseconds throughout, matching the paper's table.
#
# Run from the exuber/ package root:
#   Rscript tools/benchmark-comparison.R

suppressMessages(devtools::load_all(quiet = TRUE))
library(microbenchmark)

options(exuber.show_progress = FALSE)

minw <- 30L
sample_size <- seq(100, 1000, 100)

# --- historical columns, read verbatim from the paper's own run ------------

paper_bench <- readRDS("../exuber-paper/bench.Rds")
stopifnot(length(paper_bench) == length(sample_size))

paper_medians <- do.call(rbind, lapply(seq_along(paper_bench), function(i) {
  d <- as.data.frame(paper_bench[[i]])
  med <- tapply(d$time, d$expr, median) / 1e6 # ns -> ms
  data.frame(
    n = sample_size[i],
    MultipleBubbles_ms = unname(med[grepl("mb_mod", names(med))]),
    psymonitor_ms = unname(med[grepl("psymonitor", names(med))])
  )
}))

# --- current column: exuber 2.0.0, freshly simulated -----------------------

exuber_medians <- do.call(rbind, lapply(sample_size, function(n) {
  set.seed(123)
  rw <- cumsum(rnorm(n))
  t <- median(microbenchmark(
    exuber::radf(rw, minw = minw, lag = 1),
    unit = "ms", times = 50L
  )$time) / 1e6
  message(sprintf("n = %4d   exuber 2.0.0 = %7.2f ms", n, t))
  data.frame(n = n, `exuber 2.0.0_ms` = t, check.names = FALSE)
}))

bench_tbl <- merge(paper_medians, exuber_medians, by = "n")
bench_tbl$speedup_vs_MultipleBubbles <- bench_tbl$MultipleBubbles_ms / bench_tbl[["exuber 2.0.0_ms"]]
bench_tbl$speedup_vs_psymonitor <- bench_tbl$psymonitor_ms / bench_tbl[["exuber 2.0.0_ms"]]

saveRDS(bench_tbl, "tools/benchmark-comparison.rds")
print(bench_tbl, digits = 6)
