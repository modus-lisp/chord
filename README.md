# chord

Text to speech in pure Common Lisp. No FFI, no C, no foreign libraries — the
neural network runs in Lisp, and so does everything in front of it.

chord takes English text and produces a 22.05 kHz waveform. In between it runs a
[Piper](https://github.com/rhasspy/piper) VITS voice, which is an ONNX graph of
2755 nodes and 15.65M parameters.

    (asdf:load-system :chord)
    (let ((v (chord:load-voice "export/en_US-lessac-medium.graph")))
      (chord:say v (first (chord:text-to-ipa "Hello, world."))
                 "hello.wav"))

MIT licensed, including the front end — see [NOTICE](NOTICE) for why that took
some doing.

## State

**Text → IPA → audio, both halves measured.**

*The engine.* All 50 ONNX op types the graph uses are implemented, and all 2761
comparable intermediate values match onnxruntime within `1e-4 + 1e-4*|ref|`. The
three that cannot be compared are the two Gaussian draws and the one multiply
between a draw and its noise-scale gate; the gate names them rather than skipping
them quietly. The finished waveform differs from onnxruntime's by at most 5.3e-5
— below the 3.05e-5 step of the 16-bit WAV it gets written to. It started at
0.05x realtime and now runs **faster than realtime**: 0.63x on one thread, 2.36x
on the default 16, 2.76x on 64 (`CHORD_THREADS` sets it). Convolution is about
half of what is left, and it is bandwidth-bound rather than arithmetic-bound —
which is why it gets 6.2x out of 16 threads and only 8.6x out of 64.

The per-node gate is what made it safe to go fast, and
`inspect/conv-gate.lisp` is what makes it safe to have gone fast in the
convolution kernel specifically: 11342 cases of Conv and ConvTranspose against
the definition, over strides, dilations, padding and kernel widths this voice
never uses, compared **exactly** rather than within a tolerance. Every
optimization in that kernel claims to leave the summation order alone, so a
tolerance would hide the only kind of mistake worth catching.

*The front end.* English in, IPA out, in four stages: normalize the text, look
the word up in [CMUdict](https://github.com/cmusphinx/cmudict), fall back to
rules learned from CMUdict, then map ARPAbet to the IPA the voice was trained on.
CMUdict answers about 94% of the words in ordinary prose directly; the learned
rules get **86.6% of phones and 53.7% of whole words** exactly right on a
held-out slice of the dictionary, and of the words they miss, half miss by a
single phone.

What that costs you is diction, not intelligibility. Words are rendered one at a
time, so there is no cross-word context — no linking r in `far away`. Homographs
always take the dictionary's first listing, so half of every `read`, `live` and
`record` is wrong and nothing here can notice; fixing that needs a
part-of-speech tag, which is a different problem.

## Layout

    src/text.lisp       text -> clauses of sayable words (numbers spelled out,
                        abbreviations expanded, punctuation turned into breaks)
    src/lexicon.lisp    word -> ARPAbet, by dictionary then morphology
    src/lts.lisp        word -> ARPAbet when the dictionary has never heard of it
    src/lts-data.lisp   the learned tables (generated; do not edit)
    src/arpabet.lisp    ARPAbet -> the IPA the voice was trained on
    src/g2p.lisp        the four stages, in order

    src/tensor.lisp     flat typed vector + shape; broadcasting via zero strides
    src/model.lisp      reads the exported graph and weight blob
    src/graph.lisp      the interpreter: run the nodes in order, drop dead values
    src/ops-*.lisp      the 50 ops, grouped by what they do
    src/parallel.lisp   the convolutions, across cores
    src/speech.lisp     phoneme ids, the three scale knobs, WAV output

    data/cmudict.dict   the CMU Pronouncing Dictionary (BSD)

    tools/learn-lts.lisp    CMUdict -> src/lts-data.lisp (EM alignment, ~2 min)
    tools/export-onnx.py    ONNX -> .graph (s-expressions) + .bin (weights)
    tools/dump-fixtures.py  onnxruntime -> a golden value for every node

    inspect/text-gate.lisp  the normalizer, by table and by invariant
    inspect/lts-gate.lisp   the learned rules, on the held-out slice only
    inspect/g2p-gate.lisp   text to IPA, end to end
    inspect/node-gate.lisp  every node against onnxruntime, in order

Protobuf parsing lives in Python because it runs once per model, at build time,
on a workstation — never on the Pi this is aimed at. What ships is a text graph
a person can read and a blob of little-endian floats.

## Running the gates

    sbcl --non-interactive --load inspect/text-gate.lisp   ; 154 pass, 8 known wrong
    sbcl --non-interactive --load inspect/lts-gate.lisp    ; 17 pass
    sbcl --non-interactive --load inspect/g2p-gate.lisp    ; 31 pass
    sbcl --dynamic-space-size 8192 --load inspect/node-gate.lisp

The first three need only what is in the repo. The last one needs an exported
voice and onnxruntime fixtures:

    python3 tools/export-onnx.py en_US-lessac-medium.onnx export/
    python3 tools/dump-fixtures.py en_US-lessac-medium.onnx fixtures/

## Why a graph interpreter

The alternative was to reimplement VITS from the paper. That would have been
smaller and faster, and it would have had exactly one test: does the audio sound
right. An arithmetic mistake in the middle of a 2755-node graph does not sound
wrong, it sounds slightly off, and there is no way to bisect it.

Running the graph as exported means every node has a reference answer. When
something is wrong, the gate names the node.

## Why no reference for the front end

There is no equivalent oracle for text-to-IPA, and pretending otherwise would
have meant copying one. The dictionary is the authority for the words it lists;
held-out entries from that same dictionary are the authority for the rules; and
for how it actually sounds, the authority is a person listening. Each gate says
which of those it is using, and the eight cases the normalizer gets wrong are in
the gate by name, with the reason.
