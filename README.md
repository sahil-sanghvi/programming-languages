[![OCaml](https://img.shields.io/badge/OCaml-EC6813?logo=ocaml&logoColor=white)](https://ocaml.org/)
[![Racket](https://img.shields.io/badge/Racket-9F1D20?logo=racket&logoColor=white)](https://racket-lang.org/)
[![Ruby](https://img.shields.io/badge/Ruby-CC342D?logo=ruby&logoColor=white)](https://www.ruby-lang.org/)
[![APL](https://img.shields.io/badge/APL-array--oriented-informational)](https://en.wikipedia.org/wiki/APL_(programming_language))

# Programming Languages

Five assignments spanning four languages and as many paradigms — functional
(OCaml), lazy/metaprogramming (Racket), object-oriented + functional (Ruby),
and array-oriented (APL) — the throughline of a "programming languages"
course: the same ideas (sets, streams, interpreters) implemented in
languages that make you think about them differently.

| # | Language(s) | What it builds |
|---|---|---|
| [`assignment-1-ocaml/`](assignment-1-ocaml/) | OCaml | A leap-year checker, and a parser that reads a file of country/currency-rate data into typed records, tolerating malformed rows via `option` types. |
| [`assignment-2-ocaml/`](assignment-2-ocaml/) | OCaml | A functional set ADT (`FSET` module signature — insert/remove/union/intersect, no mutation) and **OToml**, a small TOML-like config format with its own type (`ovalue`), parser, and serializer. |
| [`assignment-3-racket/`](assignment-3-racket/) | Racket | Lazy infinite streams built from thunks (`stream-for-n-steps`, `fibo-stream`, `filter-stream`, a `create-stream` macro), plus an interpreter for a tiny functional language (variables, arithmetic, conditionals, closures, recursive functions) — problem 2 is a from-scratch `eval`. |
| [`assignment-4-ocaml-ruby/`](assignment-4-ocaml-ruby/) | OCaml + Ruby | The *same* interpreter — a 2D geometry expression language (points, lines, line segments, intersection, `let`/variable binding, coordinate shifts) — implemented twice: once as an OCaml variant type + pattern matching, once as a Ruby class hierarchy with polymorphic dispatch. Same semantics, opposite paradigm. |
| [`assignment-5-ruby-apl/`](assignment-5-ruby-apl/) | Ruby + APL | `MySet` — a polymorphic functional set in Ruby backed by a sorted array, built via `Enumerable` + a mixin for set algebra (`\|`, `&`). Then **Deoxyribonucleic Arrays**: DNA-sequence analysis in APL — nucleotide counting, GC-content, and magic-square detection, written both as APL functions and their tacit ("point-free") equivalents. |

## Running

- **OCaml**: `ocaml prob1.ml` (or load in `utop`) — each file is self-contained.
- **Racket**: open in DrRacket, or `racket prob1.rkt`; problem 3's tests live in `prob2_test.rkt`.
- **Ruby**: `ruby prob1.rb`; tests run with `ruby test_prob1.rb`.
- **APL**: load `prob2.apl` in GNU APL or Dyalog APL; `test_prob2.apl` exercises it.

## 🎓 Project Context

Built as part of **CSC 330: Programming Languages** at the University of
Victoria (Spring 2026).

## ⚠️ Academic Integrity Notice

This repository is maintained for portfolio and educational purposes only. If you are
currently enrolled in CSC 330 at the University of Victoria or a similar programming
languages course, please note that using this code in your own assignments may
constitute a violation of Academic Integrity policies.
