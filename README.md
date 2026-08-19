# chord

Text to speech in pure Common Lisp. No FFI, no C, no foreign libraries — the
neural network runs in Lisp, and so does everything in front of it.

chord takes English text and produces a 22.05 kHz waveform. In between it runs a
[Piper](https://github.com/rhasspy/piper) VITS voice, which is an ONNX graph of
2755 nodes and 15.65M parameters.

    (asdf:load-system :chord)
    (let ((v (chord:load-voice "export/en_US-ljspeech-medium.graph")))
      (chord:say v (first (chord:text-to-ipa "Hello, world."))
                 "hello.wav"))

MIT licensed, including the front end — see [NOTICE](NOTICE) for why that took
some doing.

## State

**Text → IPA → audio, both halves measured.**

*The engine.* The graph is run by [mill](https://github.com/modus-lisp/mill),
which chord is one consumer of — all 50 ONNX op types this voice uses are
implemented there, and everything about how fast they are is documented there.
All 2761 comparable intermediate values match onnxruntime within
`1e-4 + 1e-4*|ref|`. The three that cannot be compared are the two Gaussian draws
and the one multiply between a draw and its noise-scale gate; the gate names them
rather than skipping them quietly. The finished waveform differs from
onnxruntime's by at most 5.3e-5 — below the 3.05e-5 step of the 16-bit WAV it
gets written to.

It started at 0.05x realtime and now runs **faster than realtime on a single
core**: warm, on a busy 116-core box, the median of 15 sentences is 2.70x on one
thread, 6.57x on the default 16 and 6.91x on 64 (`CHORD_THREADS` sets it).
`inspect/profile.lisp` reports lower, because it times one cold run including the
first lexicon lookup. Convolution is over half of that, and mill vectorizes it
through a seven-operation vocabulary that a NEON port can implement in one file —
which is what makes the Raspberry Pi this is ultimately aimed at reachable.

Two gates here are about the voice rather than the arithmetic.
`inspect/node-gate.lisp` is the per-node comparison against onnxruntime, and it
is the reason any of the optimization in mill could be attempted at all: it runs
this graph against a golden value for every one of its 2755 nodes, so a wrong
answer names the node that first diverged. `inspect/pool-gate.lisp` covers the
one thing the node gate cannot see: the interpreter recycles a value's data
vector once its last reader has run, which the node gate never exercises because
it keeps every value alive. It synthesizes with the recycling off and on and
compares the waveforms bit for bit.

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

    src/speech.lisp     phoneme ids, the three scale knobs, WAV output

    data/cmudict.dict   the CMU Pronouncing Dictionary (BSD)

    tools/learn-lts.lisp    CMUdict -> src/lts-data.lisp (EM alignment, ~2 min)
    tools/make-feeds.py     the three inputs a VITS voice wants, as an .npz

    inspect/text-gate.lisp  the normalizer, by table and by invariant
    inspect/lts-gate.lisp   the learned rules, on the held-out slice only
    inspect/g2p-gate.lisp   text to IPA, end to end
    inspect/node-gate.lisp  every node against onnxruntime, in order
    inspect/pool-gate.lisp  recycled data vectors change no sample
    inspect/profile.lisp    where a synthesis spends its time

The tensors, the graph interpreter and the ops are mill's. So are the two Python
tools that turn a `.onnx` into something either of them can read — they run once
per model, at build time, on a workstation, never on the Pi. What ships is a text
graph a person can read and a blob of little-endian floats.

## The voice

chord does not ship weights, but it is written against a voice you can freely use and
redistribute:

    en_US-ljspeech-medium        Piper VITS, 22.05 kHz, 63 MB
    LJSpeech (keithito.com)      PUBLIC DOMAIN — no attribution required

Fetch it and convert it once, on a workstation:

    B=https://huggingface.co/rhasspy/piper-voices/resolve/main/en/en_US/ljspeech/medium
    curl -sLO $B/en_US-ljspeech-medium.onnx
    curl -sLO $B/en_US-ljspeech-medium.onnx.json

    python3 ../mill/tools/export-onnx.py en_US-ljspeech-medium.onnx export/
    cp en_US-ljspeech-medium.onnx.json export/en_US-ljspeech-medium.config.json

    (let ((v (chord:load-voice "export/en_US-ljspeech-medium.graph")))
      (chord:say v (first (chord:text-to-ipa "Hello, world.")) "hello.wav"))

The config travels with the graph because the phoneme inventory is per voice — ljspeech
maps 157 symbols where lessac maps 154, so a graph without its config will mis-speak
rather than fail.

### Check the dataset licence, not the model licence

The two are routinely different, and it is the dataset that binds. Piper's card for
`en_US-lessac-medium` reads `License: mit` — which covers Piper's repository, not the
speech. It links to the Blizzard 2013 Lessac licence, which grants use *for research
purposes only*, forbids redistribution ("lend, hire, sell, distribute or otherwise part
with the Materials"), and excludes "the development, marketing, commercialisation, sale
or licencing of voice synthesis ... products". Weights trained on that data are a derived
work of it, so that voice cannot be shipped with anything.

Of the en_US Piper voices, these are the ones whose data is actually free:

    ljspeech            public domain      the recommendation above
    kristin, norman     public domain      LibriVox
    libritts, libritts_r  CC BY 4.0        attribution required
    ryan, hfc_female    CC BY-NC-SA 4.0    non-commercial only — not usable here
    lessac              research only      not usable at all

What IS in this repo is `data/cmudict.dict` (the CMU Pronouncing Dictionary, BSD) and
`src/lts-data.lisp`, the letter-to-sound rules learned from it — both freely
redistributable, which is why they ship.

## Running the gates

    sbcl --non-interactive --load inspect/text-gate.lisp   ; 154 pass, 8 known wrong
    sbcl --non-interactive --load inspect/lts-gate.lisp    ; 17 pass
    sbcl --non-interactive --load inspect/g2p-gate.lisp    ; 31 pass
    sbcl --dynamic-space-size 8192 --load inspect/pool-gate.lisp
    sbcl --dynamic-space-size 8192 --load inspect/node-gate.lisp

The first three need only what is in the repo. The last two need an exported
voice, and the node gate also needs onnxruntime fixtures — both of which are
made with mill's tools, since neither is about speech:

    python3 ../mill/tools/export-onnx.py en_US-ljspeech-medium.onnx export/
    python3 tools/make-feeds.py en_US-ljspeech-medium.onnx feeds.npz
    python3 ../mill/tools/dump-fixtures.py en_US-ljspeech-medium.onnx feeds.npz fixtures/

The gates that cover the arithmetic itself — Conv and ConvTranspose against the
definition, and the strided copy behind Slice and Transpose — are in mill, and
need nothing but that repository.

## Why a graph interpreter

The alternative was to reimplement VITS from the paper. That would have been
smaller and faster, and it would have had exactly one test: does the audio sound
right. An arithmetic mistake in the middle of a 2755-node graph does not sound
wrong, it sounds slightly off, and there is no way to bisect it.

Running the graph as exported means every node has a reference answer. When
something is wrong, the gate names the node. That argument is the whole reason
mill exists as a separate thing, and it is made at more length there.

## Why no reference for the front end

There is no equivalent oracle for text-to-IPA, and pretending otherwise would
have meant copying one. The dictionary is the authority for the words it lists;
held-out entries from that same dictionary are the authority for the rules; and
for how it actually sounds, the authority is a person listening. Each gate says
which of those it is using, and the eight cases the normalizer gets wrong are in
the gate by name, with the reason.
