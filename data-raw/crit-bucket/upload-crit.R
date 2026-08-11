# One-time bulk upload of the locally-generated exuber-crit/ folder (see
# simulate-crit.R) to the bucket -- run this later, once you're happy with
# what's been generated locally. A plain recursive sync, not per-file: aws
# diffs against what's already remote and only transfers what's new.
#
# Requires AWS_ACCESS_KEY_ID / AWS_SECRET_ACCESS_KEY (from `railway bucket
# credentials --bucket exuber-storage`) and the aws CLI on PATH.
#   Rscript upload-crit.R

OUT_DIR <- Sys.getenv("EXUBER_CRIT_DIR", "../../../exuber-crit")
BUCKET <- Sys.getenv("EXUBER_BUCKET_NAME", "critical-values-kwz4n3ykp")
ENDPOINT <- Sys.getenv("EXUBER_BUCKET_ENDPOINT", "https://t3.storageapi.dev")

status <- system2("aws", c(
  "s3", "sync", OUT_DIR, sprintf("s3://%s/crit/", BUCKET),
  "--endpoint-url", ENDPOINT, "--region", "auto"
))
if (status != 0L) stop("aws s3 sync failed, see output above")
cat("done\n")
