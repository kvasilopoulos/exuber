# Extended simulated critical values (n = 601:2000) used to live in the
# separate `exuberdata` package, installed from a drat repo. They now live
# in a small Railway object-storage bucket, served read-only through the
# proxy in exuber-fn.ts (deployed as the Railway function "exuber-fn",
# domain exuber.up.railway.app) -- see R/crit-bucket.R for the runtime
# client and pyexuber/src/exuber/critical_values.py for the Python side.
#
# Re-run this whenever the source simulation (data-raw/sim-crit.R and its
# exuberdata equivalent) changes. Steps:
#   1. This script: export radf_crit2.rds (R runtime object) + a JSON
#      bridge file for 2-export-python.py to consume.
#   2. `python 2-export-python.py` (from this same directory): builds
#      radf_crit2.pkl.xz (extended, for the bucket) and radf_crit.pkl.xz
#      (bundled n=6:600, ships inside pyexuber -- copy it to
#      pyexuber/src/exuber/data/radf_crit.pkl.xz).
#   3. Upload to the bucket (credentials via `railway bucket credentials
#      --bucket exuber-storage`):
#        aws s3 cp radf_crit2.rds     s3://<bucket>/radf_crit2.rds     --endpoint-url https://t3.storageapi.dev --region auto
#        aws s3 cp radf_crit2.pkl.xz  s3://<bucket>/radf_crit2.pkl.xz  --endpoint-url https://t3.storageapi.dev --region auto
#   4. If exuber-fn.ts itself changed: `railway functions push -p exuber-fn.ts`.

# source: the exuberdata package's dataset, e.g. downloaded from
# https://kvasilopoulos.github.io/drat/src/contrib/exuberdata_0.2.0.tar.gz
load("exuberdata/data/radf_crit2.rda")
load("../../data/radf_crit.rda")

# -- extended (n = 601:2000): bucket-served, R runtime object -------------
crit_r <- radf_crit2
crit_r[1:600] <- list(NULL)
saveRDS(crit_r, "radf_crit2.rds", compress = "xz")

# -- JSON bridge for the Python build step ---------------------------------
to_plain <- function(cv) {
  list(
    n = attr(cv, "n"),
    minw = attr(cv, "minw"),
    adf_cv = as.list(cv$adf_cv),
    sadf_cv = as.list(cv$sadf_cv),
    gsadf_cv = as.list(cv$gsadf_cv),
    badf_cv = cv$badf_cv,
    bsadf_cv = cv$bsadf_cv
  )
}

jsonlite::write_json(
  lapply(radf_crit2[601:2000], to_plain),
  path = "radf_crit2_601_2000.json", auto_unbox = TRUE, digits = NA
)
jsonlite::write_json(
  lapply(radf_crit[6:600], to_plain),
  path = "radf_crit_6_600.json", auto_unbox = TRUE, digits = NA
)

cat("wrote radf_crit2.rds, radf_crit2_601_2000.json, radf_crit_6_600.json\n")
