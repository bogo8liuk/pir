# Pir
Pir is an implementation of [pi-calculus](https://en.wikipedia.org/wiki/%CE%A0-calculus).
The goal of this project is purely experimental and for self-learning. Anyway,
there are many features that I would like to add. The actual implementation is
made with Rust along with Tokio runtime.

## TODO
This is a (probably non-complete) list of todos:
- [ ] massive unit tests for evaluation
- [ ] better errors, especially on parsing (if possible write unit tests on them)
- [ ] adding more data types: characters, strings, floating point numbers, etc.
- [ ] adding expressions with identifiers
- [ ] adding concepts of lambda-calculus, in particular functions, but here I
need some extra theory
- [ ] benchmarks: need a way to track tasks and stop them when they are all done
without influencing performance
- [ ] rust ffi: very cool, but not the priority
