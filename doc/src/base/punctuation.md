# Punctuation

Extended documentation for mathematical symbols & functions is [here](@ref math-ops).

| symbol      | meaning                                                                                                                                         |
|:----------- |:----------------------------------------------------------------------------------------------------------------------------------------------- |
| `@m`        | invoke macro `m`; followed by space-separated expressions                                   |
| `!`         | prefix "not" (logical negation) operator                                                    |
| `a!( )`     | at the end of a function name, `!` is used as a convention to indicate that a function modifies its argument(s) |
| `#`         | begin single line comment                                                                   |
| `#=`        | begin multi-line comment (these are nestable)                                               |
| `=#`        | end multi-line comment                                                                      |
| `$`         | string and expression interpolation                                                         |
| `%`         | remainder operator                                                                          |
| `^`         | exponent operator                                                                           |
| `&`         | bitwise and                                                                                 |
| `&&`        | short-circuiting boolean and                                                                |
| `\|`        | bitwise or                                                                                  |
| `\|\|`      | short-circuiting boolean or                                                                 |
| `⊻`         | bitwise xor operator                                                                        |
| `*`         | multiply, or matrix multiply                                                                |
| `()`        | the empty tuple                                                                             |
| `~`         | bitwise not operator                                                                        |
| `\`         | backslash operator                                                                          |
| `'`         | complex transpose operator Aᴴ                                                               |
| `a[]`       | array indexing (calling [`getindex`](@ref) or [`setindex!`](@ref))                          |
| `[,]`       | vector literal constructor (calling [`vect`](@ref Base.vect))                               |
| `[;]`       | vertical concatenation (calling [`vcat`](@ref) or [`hvcat`](@ref))                          |
| `[    ]`    | with space-separated expressions, horizontal concatenation (calling [`hcat`](@ref) or [`hvcat`](@ref)) |
| `T{ }`      | parametric type instantiation                                                               |
| `;`         | statement separator                                                                         |
| `,`         | separate function arguments or tuple components                                             |
| `?`         | 3-argument conditional operator (used like: `conditional ? if_true : if_false`)             |
| `""`        | delimit string literals                                                                     |
| `''`        | delimit character literals                                                                  |
| ``` ` ` ``` | delimit external process (command) specifications                                           |
| `...`       | splice arguments into a function call or declare a varargs function                         |
| `.`         | access named fields in objects/modules (calling [`getproperty`](@ref Base.getproperty) or [`setproperty!`](@ref Base.setproperty!)), also prefixes elementwise function calls (calling [`broadcast`](@ref)) |
| `a:b`       | range a, a+1, a+2, ..., b                                                                   |
| `a:s:b`     | range a, a+s, a+2s, ..., b                                                                  |
| `:`         | index an entire dimension (firstindex:lastindex), see [`Colon`](@ref))                      |
| `::`        | type annotation or [`typeassert`](@ref), depending on context                               |
| `:( )`      | quoted expression                                                                           |
| `:a`        | symbol a                                                                                    |
| `<:`        | [`subtype operator`](@ref <:)                                                               |
| `>:`        | [`supertype operator`](@ref >:) (reverse of subtype operator)                               |
| `===`       | [`egal comparison operator`](@ref ===)                                                      |
