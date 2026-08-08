#!/usr/bin/env bash
# Vendors a pinned exubercore tag into src/vendor/exubercore/.
# Run before `R CMD build` -- CRAN tarballs must be self-contained, so this
# is a release-time step, not something the build itself does.
set -euo pipefail

TAG="${1:-v0.1.0}"
REPO="https://github.com/kvasilopoulos/exubercore.git"
PKG_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DEST="$PKG_ROOT/src/vendor/exubercore"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

echo "Fetching exubercore@$TAG..."
git clone --quiet --depth 1 --branch "$TAG" "$REPO" "$TMP"

rm -rf "$DEST"
mkdir -p "$DEST"
cp -r "$TMP/include" "$DEST/include"
cp -r "$TMP/src" "$DEST/src"
echo "$TAG" > "$DEST/VERSION"

echo "Vendored exubercore $TAG into src/vendor/exubercore/"
