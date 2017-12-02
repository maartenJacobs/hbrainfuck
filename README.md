# hBrainfuck

hBrainfuck is a [Brainfuck](https://esolangs.org/wiki/Brainfuck) parser and interpreter written in Haskell.

## Versions

### 0.0.1

Simple implementation of the parser using Parsec, and straightforward implementation of an interpreter. The tests are written in HSpec, but only the parser is covered so far. The project is built using `stack`.

## TODO

- Test the interpreter. IO actions are used for reading and writing bytes.
- Rename "brainfuck" to "hbrainfuck" for consistency.
