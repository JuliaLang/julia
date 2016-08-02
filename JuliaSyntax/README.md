# Tokenize

`Tokenize` is a Julia package that serves a similar purpose and API as the [tokenize module](https://docs.python.org/3/library/tokenize.html) in Python but for Julia. This is to take a string or buffer containing Julia code, perform lexical analysis and return a stream of tokens.

The goals of this package is to be

* Fast
* Round trippable, that is, from a stream of tokens the original string should be recoverable exactly.
* Non error throwing. Instead of throwing errors a certain error token is returned.

### API

### Tokenization

The function `tokenize` is the main entrypoint for generating `Token`s.
It takes a string or a buffer and creates an iterator that will sequentially return the next `Token` until the end of string or buffer.

```jl

e

### `Token`s

Each `Token` is represented by where it starts and ends, what string it contains and what type it is.

The types are

```julia
startpos(t)::Tuple{Int, Int} # row and column where the token start
endpos(t)::Tuple{Int, Int} # row and column where the token ends
startbyte(T)::Int64 # byte offset where the token start
endbyte(t)::Int64 # byte offset where the token ends
untokenize(t)::String # the string representation of the token
kind(t)::Token.Kind # A
exactkind(t)::
```


