# Speed comparison: exuber::radf() vs. every alternative benchmarked in
# Section 4 of Vasilopoulos, Pavlidis & Martinez-Garcia (2022, JSS) --
# MultipleBubbles (a pure-R GSADF port), psymonitor::PSY(), EViews'
# rtadf, MATLAB's PSY.m, and Stata's simulation.do -- plus exuber itself
# as it was benchmarked in the paper.
#
# Every "exuber (JSS paper)" and competitor number is read straight from
# the paper's own archived benchmark runs (exuber-paper/bench.Rds and
# exuber-paper/other-software/, read-only -- that repo is the published
# paper and is not modified by this script). bench.Rds was committed
# 2020-07-15, when exuber's DESCRIPTION read Version: 0.4.1, hence the
# "exuber 0.4.1 (JSS paper)" label. MultipleBubbles/psymonitor are
# O(T^2) pure-R double loops that already take minutes per run at
# n = 1000 on current hardware, and none of these archived numbers
# depend on the current exuber implementation, so none are re-simulated.
#
# Only the "exuber 2.0.0" series is freshly simulated, against the
# current package version. Output is a long-format table (one row per
# software x sample size) in milliseconds, saved to
# tools/benchmark-comparison.rds and plotted by README.Rmd.
#
# Run from the exuber/ package root:
#   Rscript tools/benchmark-comparison.R

suppressMessages(devtools::load_all(quiet = TRUE))
suppressMessages({
  library(microbenchmark)
  library(readxl)
  library(readr)
  library(purrr)
})

options(exuber.show_progress = FALSE)

minw <- 30L
sample_size <- seq(100, 1000, 100)
paper_dir <- "../exuber-paper"

# --- R packages, read verbatim from the paper's own microbenchmark run ----

paper_bench <- readRDS(file.path(paper_dir, "bench.Rds"))
stopifnot(length(paper_bench) == length(sample_size))

r_pkg_medians <- do.call(rbind, lapply(seq_along(paper_bench), function(i) {
  d <- as.data.frame(paper_bench[[i]])
  med <- tapply(d$time, d$expr, median) / 1e6 # ns -> ms
  data.frame(
    n = sample_size[i],
    software = c("MultipleBubbles", "psymonitor", "exuber 0.4.1 (JSS paper)"),
    time_ms = c(
      unname(med[grepl("mb_mod", names(med))]),
      unname(med[grepl("psymonitor", names(med))]),
      unname(med[grepl("^exuber", names(med))])
    )
  )
}))

# --- EViews / MATLAB / Stata, read verbatim from other-software/ ----------
# Each file is 100 replications (rows) x 10 sample sizes (columns,
# X1 = n=100 ... X10 = n=1000), elapsed seconds; median -> ms.

read_other_software_ms <- function(path, name, reader) {
  d <- reader(path)
  med_s <- map_dbl(d, median, na.rm = TRUE)
  data.frame(n = sample_size, software = name, time_ms = unname(med_s) * 1000)
}

eviews <- read_other_software_ms(
  file.path(paper_dir, "other-software/eviews/elapsed-eviews.xlsx"), "EViews (rtadf)",
  function(p) read_excel(p)
)
matlab <- read_other_software_ms(
  file.path(paper_dir, "other-software/matlab/elapsed-matlab3.txt"), "MATLAB (PSY)",
  function(p) read_csv(p, col_names = FALSE, show_col_types = FALSE)
)
stata <- read_other_software_ms(
  file.path(paper_dir, "other-software/stata/elapsed-stata.txt"), "Stata",
  function(p) read_csv(p, col_names = FALSE, show_col_types = FALSE)
)

# --- current column: exuber 2.0.0, freshly simulated -----------------------

exuber_medians <- do.call(rbind, lapply(sample_size, function(n) {
  set.seed(123)
  rw <- cumsum(rnorm(n))
  t <- median(microbenchmark(
    exuber::radf(rw, minw = minw, lag = 1),
    unit = "ms", times = 50L
  )$time) / 1e6
  message(sprintf("n = %4d   exuber 2.0.0 = %7.2f ms", n, t))
  data.frame(n = n, software = "exuber 2.0.0", time_ms = t)
}))

bench_tbl <- rbind(r_pkg_medians, eviews, matlab, stata, exuber_medians)
rownames(bench_tbl) <- NULL

saveRDS(bench_tbl, "tools/benchmark-comparison.rds")
print(bench_tbl, digits = 6)
