# Changelog

## 1.0.0

Complete rewrite.

- New interpreter: static scope resolution, mutable environments, real closures.
- `tulosta` prints things, at last.
- Classes (`luakka`, `niinku`, `itse`, `kanta`, `alota`).
- Lists, indexing, string interpolation, string concatenation.
- `else if` (`mut jos ... ni`), `ny riittää` (break), `jatketaa` (continue), for-each (`jokaselle`).
- Exceptions: `paiskaa`, `yritä ... jos käy kalpaten (e) ni ...`.
- Lambdas, `eli` short function bodies, the `sitte` pipeline, `x.f()` call syntax.
- Built-ins with signatures derived from their Haskell types, plus a prelude written in Manse.
- Error messages with source locations, a caret, and a stack trace, in the dialect.
- REPL.
- GHC 9.12, golden tests.
