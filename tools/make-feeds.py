#!/usr/bin/env python3
"""Build the .npz of inputs that mill's dump-fixtures.py feeds to the voice.

    make-feeds.py en_US-lessac-medium.onnx feeds.npz ["phonemes"]

mill knows how to run a graph and keep every intermediate; it does not know what
a VITS voice wants fed to it, which is this file's whole job.  Three inputs:

  input          Piper's id sequence — BOS, then every phoneme followed by a
                 pad, then EOS.  The interleaved pads are not decoration; the
                 model was trained with them and the alignment it predicts is
                 over this doubled sequence.
  input_lengths  how long that is
  scales         [noise_scale, length_scale, noise_w]

The two noise scales are ZEROED.  VITS samples noise in two places — the
stochastic duration predictor and the flow — so back-to-back runs of the same
input differ, and fixtures have to be reproducible.  Both draws are multiplied by
a scale passed in here, so zeroing them makes the graph a pure function of its
input without touching a weight.  length_scale stays at the voice's own default,
so the duration arithmetic is exercised rather than bypassed.
"""
import json
import sys
from pathlib import Path

import numpy as np


def phoneme_ids(config, phonemes):
    m = config["phoneme_id_map"]
    missing = [p for p in phonemes if p not in m]
    if missing:
        raise SystemExit(f"phonemes not in this voice's map: {missing!r}")
    ids = list(m["^"])
    for p in phonemes:
        ids.extend(m[p])
        ids.extend(m["_"])
    ids.extend(m["$"])
    return np.array([ids], dtype=np.int64)


def main():
    if len(sys.argv) < 3:
        raise SystemExit(__doc__)
    src, out = Path(sys.argv[1]), Path(sys.argv[2])
    # "Hello world." in the voice's own IPA — what chord's G2P produces for it.
    # Short on purpose: the fixtures are a per-node dump of a 2755-node graph,
    # and every extra phoneme costs across all of them.
    phonemes = list(sys.argv[3]) if len(sys.argv) > 3 else list("həlˈoʊ wˈɜːld.")

    config = json.loads(Path(str(src) + ".json").read_text())
    ids = phoneme_ids(config, phonemes)
    np.savez(out,
             input=ids,
             input_lengths=np.array([ids.shape[1]], dtype=np.int64),
             scales=np.array([0.0, config["inference"]["length_scale"], 0.0],
                             dtype=np.float32))
    print(f"{out}: {ids.shape[1]} ids for {''.join(phonemes)!r}, noise scales at 0")


if __name__ == "__main__":
    main()
