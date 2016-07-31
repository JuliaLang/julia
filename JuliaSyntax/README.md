# Tokenize

`Tokenize` is a Julia package that serves a similar purpose and API as the [tokenize module](https://docs.python.org/3/library/tokenize.html) in Python but for Julia code. This is to take a string or buffer containing Julia code, perform lexical analysis and return a stream of tokens. Whitespace and comments are also returned as tokens making a collection of tokens completely roundtrippable back to the original string. This is useful for making syntax highlighter or formatters for example.


### Tokenization

The function `tokenize` is the main entrypoint for generating `Token`s.
It takes a string or a buffer and creates an iterator that will sequentially return the next `Token` until the end of string or buffer.


### `Token`s

Each `Token` is represented by where it starts and ends, what string it contains and what type it is.

```julia

startpos(t)::Tuple{Int, Int} # row and column where the token start
endpos(t)::Tuple{Int, Int} # row and column where the token ends
startbyte(T)::Int64 # byte offset where the token start
endbyte(t)::Int64 # byte offset where the token ends
string(t)::String # the string representation of the token

```

