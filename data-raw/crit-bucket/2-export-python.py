"""Consumes the JSON bridge from 1-export-r-and-json.R and produces the
Python-side critical-value blobs: radf_crit2.pkl.xz (extended, upload to
the bucket) and radf_crit.pkl.xz (bundled n=6:600, copy into
pyexuber/src/exuber/data/radf_crit.pkl.xz). xz beats gzip by ~30x here --
the data has long-range repetition that gzip's 32KB window can't see.
"""

import json
import lzma
import os
import pickle


def build(json_path: str, out_path: str) -> None:
    with open(json_path) as f:
        raw = json.load(f)

    crit = {}
    for key, cv in raw.items():
        import numpy as np

        n = int(key[1:])
        crit[n] = {
            "n": cv["n"],
            "minw": cv["minw"],
            "adf_cv": np.array(list(cv["adf_cv"].values()), dtype=np.float64),
            "sadf_cv": np.array(list(cv["sadf_cv"].values()), dtype=np.float64),
            "gsadf_cv": np.array(list(cv["gsadf_cv"].values()), dtype=np.float64),
            "badf_cv": np.array(cv["badf_cv"], dtype=np.float64),
            "bsadf_cv": np.array(cv["bsadf_cv"], dtype=np.float64),
        }

    with lzma.open(out_path, "wb", preset=9 | lzma.PRESET_EXTREME) as f:
        pickle.dump(crit, f, protocol=4)
    print(f"wrote {out_path}: {os.path.getsize(out_path)} bytes, n={min(crit)}..{max(crit)}")


if __name__ == "__main__":
    build("radf_crit2_601_2000.json", "radf_crit2.pkl.xz")
    build("radf_crit_6_600.json", "radf_crit.pkl.xz")
