# Precomputed Monte Carlo critical values (lag 0-4, n up to 4000) live in a
# small object-storage bucket, one object per (lag, n) combination -- not
# one big blob, since any single analysis only ever needs exactly one
# (n, lag) table. Served read-only through a proxy so no credentials are
# needed on the client side. The producer side (simulation, upload, and
# the proxy source) lives in the sibling `exubercrit` repo.
crit_bucket_base_url <- "https://exuber.up.railway.app/crit2"

#' Local cache directory for downloaded critical-value tables
#' @keywords internal
crit_cache_dir <- function() {
  dir <- tools::R_user_dir("exuber", "cache")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

crit_cache_path <- function(n, lag) {
  file.path(crit_cache_dir(), sprintf("lag%d-n%d.bin.xz", lag, n))
}

#' Parse the fixed little-endian binary layout written by
#' exubercrit's simulate-crit.R:
#'   int32 x4: n, minw, lag, nrows
#'   float64 x3: adf_cv, x3: sadf_cv, x3: gsadf_cv
#'   float64 x(nrows*3): bsadf_cv (row-major)
#' badf_cv isn't stored -- it's always the constant PWY asymptotic tiling,
#' reconstructed here instead of transferred.
#' @keywords internal
parse_crit_bin <- function(path) {
  pcnt_names <- c("90%", "95%", "99%")
  con <- xzfile(path, "rb")
  on.exit(close(con))
  hdr <- readBin(con, "integer", n = 4L, size = 4L)
  n <- hdr[1]; minw <- hdr[2]; lag <- hdr[3]; nrows <- hdr[4]
  adf_cv <- stats::setNames(readBin(con, "double", n = 3L), pcnt_names)
  sadf_cv <- stats::setNames(readBin(con, "double", n = 3L), pcnt_names)
  gsadf_cv <- stats::setNames(readBin(con, "double", n = 3L), pcnt_names)
  bsadf_cv <- matrix(readBin(con, "double", n = nrows * 3L), ncol = 3L, byrow = TRUE,
                      dimnames = list(NULL, pcnt_names))
  badf_cv <- matrix(rep(c(-0.44, -0.08, 0.6), each = nrows), ncol = 3L,
                     dimnames = list(NULL, pcnt_names))

  list(adf_cv = adf_cv, sadf_cv = sadf_cv, gsadf_cv = gsadf_cv,
       badf_cv = badf_cv, bsadf_cv = bsadf_cv) %>%
    add_attr(method = "Monte Carlo", n = n, minw = minw, lag = lag, iter = 2000L) %>%
    add_class("radf_cv", "mc_cv")
}

#' Fetch simulated critical values for a given (n, lag), disk-cached
#'
#' Checks a persistent local cache first (survives across sessions), then
#' the bucket proxy. Returns `NULL` when the store answers 404 (that
#' combination hasn't been simulated yet) so callers can fail gracefully;
#' any other failure (no network, proxy down) is an error, since retrying
#' later may well succeed.
#' @keywords internal
fetch_crit_bucket <- function(n, lag = 0, base_url = crit_bucket_base_url) {
  # Parsed once per session: summary()/datestamp()/autoplot() each call
  # retrieve_crit() and would otherwise re-read and xz-decompress the file.
  key <- sprintf("lag%d-n%d", lag, n)
  if (!is.null(.pkgenv$crit[[key]])) return(.pkgenv$crit[[key]])
  cache_path <- crit_cache_path(n, lag)
  if (file.exists(cache_path)) {
    cv <- suppressWarnings(tryCatch(parse_crit_bin(cache_path), error = function(e) NULL))
    if (!is.null(cv)) {
      .pkgenv$crit[[key]] <- cv
      return(cv)
    }
    unlink(cache_path) # corrupt/truncated download: refetch instead of "not simulated"
  }
  # download.file() surfaces an HTTP 404 as a warning before its error, so
  # the same handler serves both conditions.
  on_fail <- function(cnd) {
    if (grepl("404", conditionMessage(cnd), fixed = TRUE)) return(NULL)
    stop_glue(
      "Cannot reach the critical-value store ({conditionMessage(cnd)}). ",
      "Check your network connection and try again."
    )
  }
  tryCatch(
    {
      dest <- tempfile(fileext = ".bin.xz")
      on.exit(unlink(dest), add = TRUE)
      url_ <- sprintf("%s/%d/%d", base_url, lag, n)
      utils::download.file(url_, dest, mode = "wb", quiet = TRUE)
      cv <- parse_crit_bin(dest)
      file.copy(dest, cache_path, overwrite = TRUE)
      .pkgenv$crit[[key]] <- cv
      cv
    },
    error = on_fail,
    warning = on_fail
  )
}
