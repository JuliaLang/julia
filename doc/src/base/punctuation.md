# Punctuation

Extended documentation for mathematical symbols & functions is [here](@ref math-ops).

| symbol      | meaning                                                                                                                                         |
|:----------- |:----------------------------------------------------------------------------------------------------------------------------------------------- |
| `@m`        | the at-symbol invokes [macro](@ref man-macros) `m`; followed by space-separated expressions or a function-call-like argument list |
| `!`         | an exclamation mark is a prefix operator for [logical negation](@ref !) ("not")                       |
| `a!`        | function names that end with an exclamation mark modify one or more of their arguments by convention |
| `#`         | the number sign (or hash or pound) character begins single line comments                    |
| `#=`        | when followed by an equals sign, it begins multi-line comment (these are nestable)          |
| `=#`        | end a multi-line comment by immediately preceding the number sign with an equals sign       |
| `$`         | the dollar sign is used for [string](@ref string-interpolation) and [expression](@ref man-expression-interpolation) interpolation |
| `%`         | the percent symbol is the [remainder operator](@ref %)                                      |
| `^`         | the carat is the [exponentiation operator](@ref ^).                                         |
| `&`         | single ampersand is [bitwise and](@ref &)                                                   |
| `&&`        | double ampersands is [short-circuiting boolean and](@ref &&)                                |
| `\|`        | single pipe character is [bitwise or](@ref |)                                               |
| `\|\|`      | double pipe characters is [short-circuiting boolean or](@ref ||)                            |
| `⊻`         | the unicode xor character is [bitwise exclusive or](@ref xor)                               |
| `~`         | the tilde is an operator for [bitwise not](@ref ~)                                          |
| `'`         | a trailing apostrophe is the [complex transpose](@ref adjoint) (or adjoint) operator Aᴴ     |
| `*`         | the asterisk is used for [multiplication](@ref *), including matrix multiplication and [string concatenation](@ref man-concatenation) |
| '/'         | forward slash [divides](@ref /) the argument on its left by the one on its right            |
| `\`         | backslash operator divides the argument on its right by the one on its left, commonly used to solve matrix equations |
| `()`        | parentheses with no arguments constructs an empty [tuple](@ref Tuple)                       |
| `(a,...)`   | parentheses with comma-separated arguments constructs a tuple containing its arguments      |
| `(a=1,...)` | parentheses with comma-separated assignments constructs a [named tuple](@ref NamedTuple)    |
| `(;)`       | parentheses can also be used to group one or more semicolon separated expressions           |
| `a[]`       | [array indexing](@ref man-indexing) (calling [`getindex`](@ref) or [`setindex!`](@ref))     |
| `[,]`       | [vector literal constructor](@ref man-array-literals) (calling [`vect`](@ref Base.vect))    |
| `[;]`       | [vertical concatenation](@ref man-array-concatenation) (calling [`vcat`](@ref) or [`hvcat`](@ref)) |
| `[    ]`    | with space-separated expressions, [horizontal concatenation](@ref man-concatenation) (calling [`hcat`](@ref) or [`hvcat`](@ref)) |
| `T{ }`      | curly braces following a type list that type's [parameters](@ref man-parametric-types)      |
| `{}`        | curly braces can also be used to group multiple [`where`](@ref) expressions in function declarations |
| `;`         | semicolons separate statements, begin a list of keyword arguments in function declarations or calls, or are used to separate array literals for vertical concatenation |
| `,`         | commas separate function arguments or tuple or array components                             |
| `?`         | the question mark delimits the ternary conditional operator (used like: `conditional ? if_true : if_false`) |
| `" "`       | the single double-quote character delimits [string](@ref String) literals                   |
| `""" """`   | three double-quote characters delimits string literals that may contain `"` and ignore leading indentation |
| `' '`       | the single-quote character delimits [character](@ref Char) literals                         |
| ``` ` ` ``` | the backtick character delimits [external process (command) literals](@ref Cmd)             |
| `A...`      | triple periods are a postfix operator that "splat" their arguments' contents into many arguments of a function call or declare a varargs function that "slurps" up many arguments into a single tuple |
| `a.b`       | single periods access named fields in objects/modules (calling [`getproperty`](@ref Base.getproperty) or [`setproperty!`](@ref Base.setproperty!)) |
| `f.()`      | periods may also prefix parentheses (like `f.(...)`) or infix operators (like `.+`) to perform the function element-wise (calling [`broadcast`](@ref)) |
| `a:b`       | colons used as a binary infix operator construct a [range](@ref :) from `a` to `b` (inclusive) with step size 1 |
| `a:s:b`     | colons used as a ternary infix operator construct a [range](@ref :) from `a` to `b` with step size `s` |
| `:`         | when used by themselves, [colons](@ref Colon) represent all indices within a dimension, frequently combined with [indexing](@ref man-indexing) |
| `::`        | double-colons represent a type annotation or [`typeassert`](@ref), depending on context, frequently used when declaring function arguments |
| `:( )`      | quoted expression                                                                           |
| `:a`        | [symbol](@ref Symbol) a                                                                     |
| `<:`        | [`subtype operator`](@ref <:)                                                               |
| `>:`        | [`supertype operator`](@ref >:) (reverse of subtype operator)                               |
| `=`         | single equals sign is [assignment](@ref man-variables)                                      |
| `==`        | double equals sign is [value equality comparison](@ref ==)                                  |
| `===`       | triple equals sign is [programmatically identical equality comparison](@ref ===)            |
