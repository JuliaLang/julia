# Tokenize

[![Build Status](https://travis-ci.org/JuliaLang/Tokenize.jl.svg?branch=master)](https://travis-ci.org/JuliaLang/Tokenize.jl) [![Build status](https://ci.appveyor.com/api/projects/status/h9d9webkxyhpx790?svg=true)](https://ci.appveyor.com/project/KristofferC/tokenize-jl)  [![codecov.io](https://codecov.io/github/JuliaLang/Tokenize.jl/coverage.svg?branch=master)](https://codecov.io/github/JuliaLang/Tokenize.jl?branch=master)


`Tokenize` is a Julia package that serves a similar purpose and API as the [tokenize module](https://docs.python.org/3/library/tokenize.html) in Python but for Julia. This is to take a string or buffer containing Julia code, perform lexical analysis and return a stream of tokens.

The goals of this package is to be

* Fast, it currently lexes all of Julia source files in ~0.25 seconds (580 files, 2 million Tokens)
* Round trippable, that is, from a stream of tokens the original string should be recoverable exactly.
* Non error throwing. Instead of throwing errors a certain error token is returned.

### API

#### Tokenization

The function `tokenize` is the main entrypoint for generating `Token`s.
It takes a string or a buffer and creates an iterator that will sequentially return the next `Token` until the end of string or buffer. The argument to `tokenize` can either be a `String`, `IOBuffer` or an `IOStream`.

```jl
julia> collect(tokenize("function f(x) end"))
 1,1-1,8          KEYWORD        "function"
 1,9-1,9          WHITESPACE     " "
 1,10-1,10        IDENTIFIER     "f"
 1,11-1,11        LPAREN         "("
 1,12-1,12        IDENTIFIER     "x"
 1,13-1,13        RPAREN         ")"
 1,14-1,14        WHITESPACE     " "
 1,15-1,17        KEYWORD        "end"
 1,18-1,17        ENDMARKER      ""
```

#### `Token`s

Each `Token` is represented by where it starts and ends, what string it contains and what type it is.

The API for a `Token` (non exported from the `Tokenize.Tokens` module) is.

```julia
startpos(t)::Tuple{Int, Int} # row and column where the token start
endpos(t)::Tuple{Int, Int}   # row and column where the token ends
startbyte(T)::Int            # byte offset where the token start
endbyte(t)::Int              # byte offset where the token ends
untokenize(t)::String        # string representation of the token
kind(t)::Token.Kind          # kind of the token
exactkind(t)::Token.Kind     # exact kind of the token
```

The difference between `kind` and `exactkind` is that `kind` returns `OP` for all operators and `KEYWORD` for all keywords while `exactkind` returns a unique kind for all different operators and keywords, ex;

```jl
julia> tok = collect(tokenize("⇒"))[1];

julia> Tokens.kind(tok)
OP::Tokenize.Tokens.Kind = 90

julia> Tokens.exactkind(tok)
RIGHTWARDS_DOUBLE_ARROW::Tokenize.Tokens.Kind = 128
```

All the different `Token.Kind` can be seen in the [`token_kinds.jl` file](https://github.com/JuliaLang/Tokenize.jl/blob/master/src/token_kinds.jl)
