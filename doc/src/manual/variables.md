# [Variables](@id man-variables)

A variable, in Julia, is a name associated (or bound) to a value. It's useful when you want to
store a value (that you obtained after some math, for example) for later use. For example:

```julia-repl
# Assign the value 10 to the variable x
julia> x = 10
10

# Doing math with x's value
julia> x + 1
11

# Reassign x's value
julia> x = 1 + 1
2

# You can assign values of other types, like strings of text
julia> x = "Hello World!"
"Hello World!"
```

Julia provides an extremely flexible system for naming variables. Variable names are case-sensitive,
and have no semantic meaning (that is, the language will not treat variables differently based
on their names).

```jldoctest
julia> x = 1.0
1.0

julia> y = -3
-3

julia> Z = "My string"
"My string"

julia> customary_phrase = "Hello world!"
"Hello world!"

julia> UniversalDeclarationOfHumanRightsStart = "人人生而自由，在尊严和权利上一律平等。"
"人人生而自由，在尊严和权利上一律平等。"
```

Unicode names (in UTF-8 encoding) are allowed:

```jldoctest
julia> δ = 0.00001
1.0e-5

julia> 안녕하세요 = "Hello"
"Hello"
```

In the Julia REPL and several other Julia editing environments, you can type many Unicode math
symbols by typing the backslashed LaTeX symbol name followed by tab. For example, the variable
name `δ` can be entered by typing `\delta`-*tab*, or even `α̂⁽²⁾` by `\alpha`-*tab*-`\hat`-
*tab*-`\^(2)`-*tab*. (If you find a symbol somewhere, e.g. in someone else's code,
that you don't know how to type, the REPL help will tell you: just type `?` and
then paste the symbol.)

Julia will even let you redefine built-in constants and functions if needed (although
this is not recommended to avoid potential confusions):

```jldoctest
julia> pi = 3
3

julia> pi
3

julia> sqrt = 4
4
```

However, if you try to redefine a built-in constant or function already in use, Julia will give
you an error:

```jldoctest
julia> pi
π = 3.1415926535897...

julia> pi = 3
ERROR: cannot assign a value to imported variable MathConstants.pi from module Main

julia> sqrt(100)
10.0

julia> sqrt = 4
ERROR: cannot assign a value to imported variable Base.sqrt from module Main
```

## [Allowed Variable Names](@id man-allowed-variable-names)

Variable names must begin with a letter (A-Z or a-z), underscore, or a subset of Unicode code
points greater than 00A0; in particular, [Unicode character categories](https://www.fileformat.info/info/unicode/category/index.htm)
Lu/Ll/Lt/Lm/Lo/Nl (letters), Sc/So (currency and other symbols), and a few other letter-like characters
(e.g. a subset of the Sm math symbols) are allowed. Subsequent characters may also include ! and
digits (0-9 and other characters in categories Nd/No), as well as other Unicode code points: diacritics
and other modifying marks (categories Mn/Mc/Me/Sk), some punctuation connectors (category Pc),
primes, and a few other characters.

Operators like `+` are also valid identifiers, but are parsed specially. In some contexts, operators
can be used just like variables; for example `(+)` refers to the addition function, and `(+) = f`
will reassign it. Most of the Unicode infix operators (in category Sm), such as `⊕`, are parsed
as infix operators and are available for user-defined methods (e.g. you can use `const ⊗ = kron`
to define `⊗` as an infix Kronecker product).  Operators can also be suffixed with modifying marks,
primes, and sub/superscripts, e.g. `+̂ₐ″` is parsed as an infix operator with the same precedence as `+`.
A space is required between an operator that ends with a subscript/superscript letter and a subsequent
variable name. For example, if `+ᵃ` is an operator, then `+ᵃx` must be written as `+ᵃ x` to distinguish
it from `+ ᵃx` where `ᵃx` is the variable name.


A particular class of variable names is one that contains only underscores. These identifiers can only be assigned values, which are immediately discarded, and cannot therefore be used to assign values to other variables (i.e., they cannot be used as [`rvalues`](https://en.wikipedia.org/wiki/Value_(computer_science)#Assignment:_l-values_and_r-values)) or use the last value
assigned to them in any way.

```julia-repl
julia> x, ___ = size([2 2; 1 1])
(2, 2)

julia> y = ___
ERROR: syntax: all-underscore identifier used as rvalue

julia> println(___)
ERROR: syntax: all-underscore identifier used as rvalue
```

The only explicitly disallowed names for variables are the names of the built-in [Keywords](@ref Keywords):

```julia-repl
julia> else = false
ERROR: syntax: unexpected "else"

julia> try = "No"
ERROR: syntax: unexpected "="
```

Some Unicode characters are considered to be equivalent in identifiers.
Different ways of entering Unicode combining characters (e.g., accents)
are treated as equivalent (specifically, Julia identifiers are [NFC](https://www.macchiato.com/unicode-intl-sw/nfc-faq)-normalized).
Julia also includes a few non-standard equivalences for characters that are
visually similar and are easily entered by some input methods. The Unicode
characters `ɛ` (U+025B: Latin small letter open e) and `µ` (U+00B5: micro sign)
are treated as equivalent to the corresponding Greek letters. The middle dot
`·` (U+00B7) and the Greek
[interpunct](https://en.wikipedia.org/wiki/Interpunct) `·` (U+0387) are both
treated as the mathematical dot operator `⋅` (U+22C5).
The minus sign `−` (U+2212) is treated as equivalent to the hyphen-minus sign `-` (U+002D).

## [Assignment expressions and assignment versus mutation](@id man-assignment-expressions)

An assignment `variable = value` "binds" the name `variable` to the `value` computed
on the right-hand side, and the whole assignment is treated by Julia as an expression
equal to the right-hand-side `value`.  This means that assignments can be *chained*
(the same `value` assigned to multiple variables with `variable1 = variable2 = value`)
or used in other expressions, and is also why their result is shown in the REPL as
the value of the right-hand side.  (In general, the REPL displays the value of whatever
expression you evaluate.)  For example, here the value `4` of `b = 2+2` is
used in another arithmetic operation and assignment:

```jldoctest
julia> a = (b = 2+2) + 3
7

julia> a
7

julia> b
4
```

A common confusion is the distinction between *assignment* (giving a new "name" to a value)
and *mutation* (changing a value).  If you run `a = 2` followed by `a = 3`, you have changed
the "name" `a` to refer to a new value `3` … you haven't changed the number `2`, so `2+2`
will still give `4` and not `6`!   This distinction becomes more clear when dealing with
*mutable* types like [arrays](@ref lib-arrays), whose contents *can* be changed:

```jldoctest mutation_vs_rebind
julia> a = [1,2,3] # an array of 3 integers
3-element Vector{Int64}:
 1
 2
 3

julia> b = a   # both b and a are names for the same array!
3-element Vector{Int64}:
 1
 2
 3
```

Here, the line `b = a` does *not* make a copy of the array `a`, it simply binds the name
`b` to the *same* array `a`: both `b` and `a` "point" to one array `[1,2,3]` in memory.
In contrast, an assignment `a[i] = value` *changes* the *contents* of the array, and the
modified array will be visible through both the names `a` and `b`:

```jldoctest mutation_vs_rebind
julia> a[1] = 42     # change the first element
42

julia> a = 3.14159   # a is now the name of a different object
3.14159

julia> b   # b refers to the original array object, which has been mutated
3-element Vector{Int64}:
 42
  2
  3
```
That is, `a[i] = value` (an alias for [`setindex!`](@ref)) *mutates* an existing array object
in memory, accessible via either `a` or `b`.  Subsequently setting `a = 3.14159`
does not change this array, it simply binds `a` to a different object; the array is still
accessible via `b`. The other common syntax to mutate an existing object is
`a.field = value` (an alias for [`setproperty!`](@ref)), which can be used to change
a [`mutable struct`](@ref).

When you call a [function](@ref man-functions) in Julia, it behaves as if you *assigned*
the argument values to new variable names corresponding to the function arguments, as discussed
in [Argument-Passing Behavior](@ref man-functions).  (By [convention](@ref man-punctuation),
functions that mutate one or more of their arguments have names ending with `!`.)


## Stylistic Conventions

While Julia imposes few restrictions on valid names, it has become useful to adopt the following
conventions:

  * Names of variables are in lower case.
  * Word separation can be indicated by underscores (`'_'`), but use of underscores is discouraged
    unless the name would be hard to read otherwise.
  * Names of `Type`s and `Module`s begin with a capital letter and word separation is shown with upper
    camel case instead of underscores.
  * Names of `function`s and `macro`s are in lower case, without underscores.
  * Functions that write to their arguments have names that end in `!`. These are sometimes called
    "mutating" or "in-place" functions because they are intended to produce changes in their arguments
    after the function is called, not just return a value.

For more information about stylistic conventions, see the [Style Guide](@ref).
