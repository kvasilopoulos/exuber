# Simulates critical values and saves each one to a local folder as soon
# as it's done -- one small file per (lag, n) combination, written
# immediately (never held in memory / batched), so nothing is lost if this
# is stopped. Uploading to the bucket is a separate, later step (plain
# `aws s3 sync OUT_DIR s3://<bucket>/crit/`) -- this script has no network
# dependency at all, which also sidesteps needing bucket credentials just
# to generate data.
#
# Resumable & idempotent: checks the local OUT_DIR on startup and skips
# any (lag, n) whose file already exists there, so this is safe to stop
# (Ctrl+C / kill) and re-run any time -- never redoes the same critical
# values twice. n/lag bounds are CLI args (see below); default is
# lag 0:4, n 6:2000.
#
# Run from this directory, in small bounded increments -- cost grows
# ~O(n^2.9), so keep n ranges modest per invocation and widen once you've
# seen a chunk actually complete:
#   Rscript simulate-crit.R <lag_start> <lag_end> <n_start> <n_end>
#   Rscript simulate-crit.R 1 1 6 300      # lag 1, n = 6:300 only
#   Rscript simulate-crit.R 1 4 6 2000     # lag 1:4, n = 6:2000
# Any trailing args can be omitted; defaults are lag 0:4, n 6:2000.

library(exuber)
options(exuber.parallel = TRUE)
options(exuber.show_progress = FALSE)
# .onLoad caps this at 2 for non-interactive/CRAN sessions -- this is a
# dedicated batch job, so override it directly instead.
options(exuber.ncores = max(1, parallel::detectCores() - 2))

# Each combo runs in its own throwaway subprocess (callr), not inline:
# the parallel backend creates/tears down a fresh worker pool on every
# radf_mc_cv() call, and repeating that hundreds of times in one long-lived
# process has been observed to eventually hang on worker spawn (silently --
# no error, no CPU, just stuck) with zero indication beyond the log going
# quiet. A subprocess with a hard timeout turns that into a logged,
# skippable failure instead of an undetected multi-hour stall, and as a
# side effect never accumulates hundreds of plan()/teardown cycles in one
# session in the first place.
run_one <- function(n, lag, nrep, ncores) {
  library(exuber)
  options(exuber.parallel = TRUE)
  options(exuber.show_progress = FALSE)
  options(exuber.ncores = ncores)
  radf_mc_cv(n, nrep = nrep, seed = 123L, lag = lag)
}

OUT_DIR <- Sys.getenv("EXUBER_CRIT_DIR", "../../../exuber-crit")
NREP <- 2000L
LOG <- "simulate-crit.log"

args <- commandArgs(trailingOnly = TRUE)
lag_start <- if (length(args) >= 1) as.integer(args[1]) else 0L
lag_end   <- if (length(args) >= 2) as.integer(args[2]) else 4L
N_MIN     <- if (length(args) >= 3) as.integer(args[3]) else 6L
N_MAX     <- if (length(args) >= 4) as.integer(args[4]) else 2000L

log_msg <- function(fmt, ...) {
  cat(sprintf("[%s] %s\n", format(Sys.time()), sprintf(fmt, ...)), file = LOG, append = TRUE)
}

# Rough ETA only (for log messages) -- power-law fit to measured
# single-threaded per-replicate cost (n=500 ~0.26s, n=1000 ~1.93s,
# n=2000 ~15.2s -> exponent ~2.9), divided by an assumed ~8x effective
# parallel speedup (observed ~5.5x at n=1000 incl. per-call cluster
# startup overhead, rising towards the true core count as n grows and
# that fixed overhead matters less).
est_seconds <- function(n, nrep, ncores) {
  b <- log(15.2 / 0.26) / log(2000 / 500)
  a <- 0.26 / 500^b
  per_rep <- a * n^b
  per_rep * nrep / min(8, ncores)
}

fmt_hms <- function(s) {
  s <- round(s)
  sprintf("%02d:%02d:%02d", s %/% 3600, (s %% 3600) %/% 60, s %% 60)
}

# Fixed little-endian binary layout, parseable with readBin()/np.frombuffer
# in R/Python without pickle or RDS overhead. badf_cv is NOT stored: it's
# always the constant PWY asymptotic tiling (see radf_mc_cv()), trivially
# reconstructed client-side, so storing it would just double the payload
# for zero information.
#   int32 x4:  n, minw, lag, nrows
#   float64 x3: adf_cv (90/95/99%)
#   float64 x3: sadf_cv
#   float64 x3: gsadf_cv
#   float64 x(nrows*3): bsadf_cv, row-major (row = time index, col = pcnt)
write_crit_bin_xz <- function(cv, lag, path) {
  nrows <- nrow(cv$bsadf_cv)
  tmp <- paste0(path, ".part")  # write under a temp name, rename on success
  con <- xzfile(tmp, "wb", compression = 9)
  writeBin(as.integer(c(attr(cv, "n"), attr(cv, "minw"), lag, nrows)), con, size = 4L)
  writeBin(as.double(cv$adf_cv), con)
  writeBin(as.double(cv$sadf_cv), con)
  writeBin(as.double(cv$gsadf_cv), con)
  writeBin(as.double(t(cv$bsadf_cv)), con)
  close(con)
  file.rename(tmp, path)  # atomic-ish: a half-written file never looks "done"
}

ncores <- getOption("exuber.ncores") %||% 1
log_msg("=== starting: lag %d:%d, n %d:%d, nrep=%d, cores=%s, pid=%d, out=%s ===",
        lag_start, lag_end, N_MIN, N_MAX, NREP, ncores, Sys.getpid(), normalizePath(OUT_DIR, mustWork = FALSE))

bundled_max <- length(exuber::radf_crit)  # n <= this, lag 0, is already bundled in-package

todo <- character(0)
for (lag in lag_start:lag_end) {
  dir.create(file.path(OUT_DIR, sprintf("lag%d", lag)), recursive = TRUE, showWarnings = FALSE)
  n_start <- if (lag == 0) max(N_MIN, bundled_max + 1L) else N_MIN
  if (n_start > N_MAX) next  # e.g. lag 0 with N_MAX below the bundled range
  ns <- n_start:N_MAX
  paths <- file.path(OUT_DIR, sprintf("lag%d", lag), sprintf("n%d.bin.xz", ns))
  keep <- !file.exists(paths)
  if (any(keep)) todo <- c(todo, sprintf("%d:%d", lag, ns[keep]))
}
log_msg("%d combinations left to simulate this run (already have the rest locally)", length(todo))

n_total <- length(todo)
n_done_session <- 0L
session_start <- Sys.time()

for (item in todo) {
  parts <- strsplit(item, ":")[[1]]
  lag <- as.integer(parts[1]); n <- as.integer(parts[2])
  out_path <- file.path(OUT_DIR, sprintf("lag%d", lag), sprintf("n%d.bin.xz", n))

  eta_this <- est_seconds(n, NREP, ncores)
  timeout_this <- max(120, eta_this * 5)  # generous margin over the estimate
  log_msg("start n=%d lag=%d (%d/%d this run, ~%s est., %s timeout)",
          n, lag, n_done_session + 1L, n_total, fmt_hms(eta_this), fmt_hms(timeout_this))
  t0 <- Sys.time()

  cv <- tryCatch(
    callr::r(run_one, args = list(n = n, lag = lag, nrep = NREP, ncores = ncores),
             timeout = timeout_this),
    error = function(e) { log_msg("SIM FAILED/TIMEOUT n=%d lag=%d: %s", n, lag, conditionMessage(e)); NULL }
  )
  if (is.null(cv)) next

  write_crit_bin_xz(cv, lag, out_path)
  size_kb <- file.info(out_path)$size / 1024

  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  n_done_session <- n_done_session + 1L
  session_elapsed <- as.numeric(difftime(Sys.time(), session_start, units = "secs"))
  avg_pace <- session_elapsed / n_done_session
  remaining <- n_total - n_done_session
  log_msg("saved n=%d lag=%d (%.1fs, %.1f KB) -- %d/%d done, ~%s left in queue",
          n, lag, elapsed, size_kb, n_done_session, n_total,
          fmt_hms(avg_pace * remaining))
}

log_msg("=== finished: lag %d:%d (%d combinations simulated this run) ===",
        lag_start, lag_end, n_done_session)
