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
0.05x realtime and now runs **faster than realtime on a single core**: warm, on
a busy 116-core box, the median of 15 sentences is 2.70x on one thread, 6.57x on
the default 16 and 6.91x on 64 (`CHORD_THREADS` sets it). `inspect/profile.lisp`
reports lower, because it times one cold run including the first lexicon lookup.

Convolution is over half of that, and it is vectorized — `src/simd.lisp` is
seven operations and a lane count wide, and every kernel is written in those, so
the ARM machine this is ultimately aimed at needs one new arm in one file rather
than a second copy of the convolution. Lanes go across the output, never across
the kernel taps, and it does not use fused multiply-add even though the CPU has
it: both would change the order the terms are added in, and the point of
`inspect/conv-gate.lisp` comparing exactly is that nothing does. The elementwise
Add, Sub, Mul and Div go the same way, where the lanes are independent and there
is nothing to reassociate at all.

ConvTranspose is the odd one, because its output taps are strided and a strided
store is not one store. It de-interleaves the output row by residue class first:
a given kernel position only ever reaches outputs sharing one residue mod the
stride, so writing those contiguously turns each pass into the same stride-1
loop the Convs use — the same terms, added in the same order, at permuted
addresses. That was worth 6x on the op.

Measured against the same build with the vector unit turned off, one thread went
0.78x → 2.70x, sixteen went 3.91x → 6.57x and sixty-four went 4.87x → 6.91x.
That "turned off" build is not a separate implementation that could drift: the
fallback arm of `src/simd.lisp` sets the lane count to 1 and every operation to
its scalar self, so it is the same source with the width turned down — and it
produces the same 205568 samples, byte for byte, that the AVX2 build does.

The other half of going fast is not arithmetic at all. Slice, Transpose, Expand
and Split are one strided copy loop between them, and it was a fifth of a
single-threaded synthesis — which turned out to be mostly a misreading of the
work: a slice that only cuts an outer axis leaves the whole tail contiguous, so
the copy is a block move, and it was being done one float at a time by an
odometer. Rewriting the walk with the fewest axes that describe it takes that
loop from 0.158 s to 0.018 s. It is worth more at sixteen threads (+32%) than at
one (+7%), because it was serial: work that does not spread out is a bigger
share of the total the more everything around it does.

Two gates make going fast safe. The per-node one is the reason any of it could
be attempted at all. `inspect/conv-gate.lisp` covers the convolution kernel
specifically: 11342 cases of Conv and ConvTranspose against the definition, over
strides, dilations, padding and kernel widths this voice never uses, compared
**exactly** rather than within a tolerance — every optimization in that kernel
claims to leave the summation order alone, so a tolerance would hide the only
kind of mistake worth catching. `inspect/gather-gate.lisp` does the same
for the strided copy, and for the same reason — one voice only ever asks for
seven transposes and a dozen slices, so nothing there exercises a negative
stride, a broadcast stride of 0, or rank 5. It sweeps 20000 random walks against
the definition, also exactly, since a copy does no arithmetic and one differing
element is a bug. `inspect/pool-gate.lisp` covers the other thing the node gate
cannot see: the interpreter recycles a value's data vector once its last reader
has run, which the node gate never exercises because it keeps every value alive.
It synthesizes with the recycling off and on and compares the waveforms bit for
bit.

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
    src/simd.lisp       one vector width, named once (AVX2 today, NEON next;
                        lanes = 1 is the fallback, and the scalar reference)
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
    inspect/conv-gate.lisp  Conv and ConvTranspose against the definition
    inspect/gather-gate.lisp  the strided copy behind Slice/Transpose/Expand
    inspect/pool-gate.lisp  recycled data vectors change no sample
    inspect/profile.lisp    where a synthesis spends its time

Protobuf parsing lives in Python because it runs once per model, at build time,
on a workstation — never on the Pi this is aimed at. What ships is a text graph
a person can read and a blob of little-endian floats.

## Running the gates

    sbcl --non-interactive --load inspect/text-gate.lisp   ; 154 pass, 8 known wrong
    sbcl --non-interactive --load inspect/lts-gate.lisp    ; 17 pass
    sbcl --non-interactive --load inspect/g2p-gate.lisp    ; 31 pass
    sbcl --dynamic-space-size 4096 --load inspect/conv-gate.lisp  ; 11342 cases
    sbcl --dynamic-space-size 4096 --load inspect/gather-gate.lisp ; 20000 walks
    sbcl --dynamic-space-size 8192 --load inspect/pool-gate.lisp
    sbcl --dynamic-space-size 8192 --load inspect/node-gate.lisp

The first five need only what is in the repo. The last two need an exported
voice, and the node gate also needs onnxruntime fixtures:

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
