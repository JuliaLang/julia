# This file is a part of Julia. License is MIT: https://julialang.org/license

module BaseDocs

@nospecialize # don't specialize on any arguments of the methods declared herein

struct Keyword
    name::Symbol
end
macro kw_str(text)
    return Keyword(Symbol(text))
end

"""
**Welcome to Julia $(string(VERSION)).** The full manual is available at

    https://docs.julialang.org

as well as many great tutorials and learning resources:

    https://julialang.org/learning/

For help on a specific function or macro, type `?` followed
by its name, e.g. `?cos`, or `?@time`, and press enter.
Type `;` to enter shell mode, `]` to enter package mode.
"""
kw"help", kw"Julia", kw"julia", kw""

"""
    using

`using Foo` will load the module or package `Foo` and make its [`export`](@ref)ed names
available for direct use. Names can also be used via dot syntax (e.g. `Foo.foo` to access
the name `foo`), whether they are `export`ed or not.
See the [manual section about modules](@ref modules) for details.
"""
kw"using"

"""
    import

`import Foo` will load the module or package `Foo`.
Names from the imported `Foo` module can be accessed with dot syntax
(e.g. `Foo.foo` to access the name `foo`).
See the [manual section about modules](@ref modules) for details.
"""
kw"import"

"""
    export

`export` is used within modules to tell Julia which functions should be
made available to the user. For example: `export foo` makes the name
`foo` available when [`using`](@ref) the module.
See the [manual section about modules](@ref modules) for details.
"""
kw"export"

"""
    abstract type

`abstract type` declares a type that cannot be instantiated, and serves only as a node in the
type graph, thereby describing sets of related concrete types: those concrete types
which are their descendants. Abstract types form the conceptual hierarchy which makes
Julia’s type system more than just a collection of object implementations. For example:

```julia
abstract type Number end
abstract type Real <: Number end
```
[`Number`](@ref) has no supertype, whereas [`Real`](@ref) is an abstract subtype of `Number`.
"""
kw"abstract type"

"""
    module

`module` declares a [`Module`](@ref), which is a separate global variable workspace. Within a
module, you can control which names from other modules are visible (via importing), and
specify which of your names are intended to be public (via exporting).
Modules allow you to create top-level definitions without worrying about name conflicts
when your code is used together with somebody else’s.
See the [manual section about modules](@ref modules) for more details.

# Examples
```julia
module Foo
import Base.show
export MyType, foo

struct MyType
    x
end

bar(x) = 2x
foo(a::MyType) = bar(a.x) + 1
show(io::IO, a::MyType) = print(io, "MyType \$(a.x)")
end
```
"""
kw"module"

"""
    __init__

`__init__()` function in your module would executes immediately *after* the module is loaded at
runtime for the first time (i.e., it is only called once and only after all statements in the
module have been executed). Because it is called *after* fully importing the module, `__init__`
functions of submodules will be executed *first*. Two typical uses of __init__ are calling
runtime initialization functions of external C libraries and initializing global constants
that involve pointers returned by external libraries.
See the [manual section about modules](@ref modules) for more details.

# Examples
```julia
const foo_data_ptr = Ref{Ptr{Cvoid}}(0)
function __init__()
    ccall((:foo_init, :libfoo), Cvoid, ())
    foo_data_ptr[] = ccall((:foo_data, :libfoo), Ptr{Cvoid}, ())
    nothing
end
```
"""
kw"__init__"

"""
    baremodule

`baremodule` declares a module that does not contain `using Base`
or a definition of [`eval`](@ref Base.eval). It does still import `Core`.
"""
kw"baremodule"

"""
    primitive type

`primitive type` declares a concrete type whose data consists only of a series of bits. Classic
examples of primitive types are integers and floating-point values. Some example built-in
primitive type declarations:

```julia
primitive type Char 32 end
primitive type Bool <: Integer 8 end
```
The number after the name indicates how many bits of storage the type requires. Currently,
only sizes that are multiples of 8 bits are supported.
The [`Bool`](@ref) declaration shows how a primitive type can be optionally
declared to be a subtype of some supertype.
"""
kw"primitive type"

"""
    macro

`macro` defines a method for inserting generated code into a program.
A macro maps a sequence of argument expressions to a returned expression, and the
resulting expression is substituted directly into the program at the point where
the macro is invoked.
Macros are a way to run generated code without calling [`eval`](@ref Base.eval), since the generated
code instead simply becomes part of the surrounding program.
Macro arguments may include expressions, literal values, and symbols. Macros can be defined for
variable number of arguments (varargs), but do not accept keyword arguments.

# Examples
```jldoctest
julia> macro sayhello(name)
           return :( println("Hello, ", \$name, "!") )
       end
@sayhello (macro with 1 method)

julia> @sayhello "Charlie"
Hello, Charlie!

julia> macro saylots(x...)
           return :( println("Say: ", \$(x...)) )
       end
@saylots (macro with 1 method)

julia> @saylots "hey " "there " "friend"
Say: hey there friend
```
"""
kw"macro"

"""
    local

`local` introduces a new local variable.
See the [manual section on variable scoping](@ref scope-of-variables) for more information.

# Examples
```jldoctest
julia> function foo(n)
           x = 0
           for i = 1:n
               local x # introduce a loop-local x
               x = i
           end
           x
       end
foo (generic function with 1 method)

julia> foo(10)
0
```
"""
kw"local"

"""
    global

`global x` makes `x` in the current scope and its inner scopes refer to the global
variable of that name.
See the [manual section on variable scoping](@ref scope-of-variables) for more information.

# Examples
```jldoctest
julia> z = 3
3

julia> function foo()
           global z = 6 # use the z variable defined outside foo
       end
foo (generic function with 1 method)

julia> foo()
6

julia> z
6
```
"""
kw"global"

"""
    =

`=` is the assignment operator.
* For variable `a` and expression `b`, `a = b` makes `a` refer to the value of `b`.
* For functions `f(x)`, `f(x) = x` defines a new function constant `f`, or adds a new method to `f` if `f` is already defined; this usage is equivalent to `function f(x); x; end`.
* `a[i] = v` calls [`setindex!`](@ref)`(a,v,i)`.
* `a.b = c` calls [`setproperty!`](@ref)`(a,:b,c)`.
* Inside a function call, `f(a=b)` passes `b` as the value of keyword argument `a`.
* Inside parentheses with commas, `(a=1,)` constructs a [`NamedTuple`](@ref).

# Examples
Assigning `a` to `b` does not create a copy of `b`; instead use [`copy`](@ref) or [`deepcopy`](@ref).

```jldoctest
julia> b = [1]; a = b; b[1] = 2; a
1-element Array{Int64,1}:
 2

julia> b = [1]; a = copy(b); b[1] = 2; a
1-element Array{Int64,1}:
 1

```
Collections passed to functions are also not copied. Functions can modify (mutate) the contents of the objects their arguments refer to. (The names of functions which do this are conventionally suffixed with '!'.)
```jldoctest
julia> function f!(x); x[:] .+= 1; end
f! (generic function with 1 method)

julia> a = [1]; f!(a); a
1-element Array{Int64,1}:
 2

```
Assignment can operate on multiple variables in parallel, taking values from an iterable:
```jldoctest
julia> a, b = 4, 5
(4, 5)

julia> a, b = 1:3
1:3

julia> a, b
(1, 2)

```
Assignment can operate on multiple variables in series, and will return the value of the right-hand-most expression:
```jldoctest
julia> a = [1]; b = [2]; c = [3]; a = b = c
1-element Array{Int64,1}:
 3

julia> b[1] = 2; a, b, c
([2], [2], [2])

```
Assignment at out-of-bounds indices does not grow a collection. If the collection is a [`Vector`](@ref) it can instead be grown with [`push!`](@ref) or [`append!`](@ref).
```jldoctest
julia> a = [1, 1]; a[3] = 2
ERROR: BoundsError: attempt to access 2-element Array{Int64,1} at index [3]
[...]

julia> push!(a, 2, 3)
4-element Array{Int64,1}:
 1
 1
 2
 3

```
Assigning `[]` does not eliminate elements from a collection; instead use [`filter!`](@ref).
```jldoctest
julia> a = collect(1:3); a[a .<= 1] = []
ERROR: DimensionMismatch("tried to assign 0 elements to 1 destinations")
[...]

julia> filter!(x -> x > 1, a) # in-place & thus more efficient than a = a[a .> 1]
2-element Array{Int64,1}:
 2
 3

```
"""
kw"="

"""
    .=

Perform broadcasted assignment. The right-side argument is expanded as in
[`broadcast`](@ref) and then assigned into the left-side argument in-place.
Fuses with other dotted operators in the same expression; i.e. the whole
assignment expression is converted into a single loop.

`A .= B` is similar to `broadcast!(identity, A, B)`.

# Examples
```jldoctest
julia> A = zeros(4, 4); B = [1, 2, 3, 4];

julia> A .= B
4×4 Array{Float64,2}:
 1.0  1.0  1.0  1.0
 2.0  2.0  2.0  2.0
 3.0  3.0  3.0  3.0
 4.0  4.0  4.0  4.0

julia> A
4×4 Array{Float64,2}:
 1.0  1.0  1.0  1.0
 2.0  2.0  2.0  2.0
 3.0  3.0  3.0  3.0
 4.0  4.0  4.0  4.0
```
"""
kw".="

"""
    .

The dot operator is used to access fields or properties of objects and access
variables defined inside modules.

In general, `a.b` calls `getproperty(a, :b)` (see [`getproperty`](@ref Base.getproperty)).

# Examples
```jldoctest
julia> z = 1 + 2im; z.im
2

julia> Iterators.product
product (generic function with 1 method)
```
"""
kw"."

"""
    let

`let` statements allocate new variable bindings each time they run. Whereas an
assignment modifies an existing value location, `let` creates new locations. This
difference is only detectable in the case of variables that outlive their scope via
closures. The `let` syntax accepts a comma-separated series of assignments and variable
names:

```julia
let var1 = value1, var2, var3 = value3
    code
end
```
The assignments are evaluated in order, with each right-hand side evaluated in the scope
before the new variable on the left-hand side has been introduced. Therefore it makes
sense to write something like `let x = x`, since the two `x` variables are distinct and
have separate storage.
"""
kw"let"

"""
    quote

`quote` creates multiple expression objects in a block without using the explicit
[`Expr`](@ref) constructor. For example:

```julia
ex = quote
    x = 1
    y = 2
    x + y
end
```
Unlike the other means of quoting, `:( ... )`, this form introduces `QuoteNode` elements
to the expression tree, which must be considered when directly manipulating the tree.
For other purposes, `:( ... )` and `quote .. end` blocks are treated identically.
"""
kw"quote"

"""
    {}

Curly braces are used to specify [type parameters](@ref man-parametric-types).

Type parameters allow a single type declaration to introduce a whole family of
new types — one for each possible combination of parameter values. For example,
the [`Set`](@ref) type describes many possible types of sets; it uses one type
parameter to describe the type of the elements it contains. The specific _parameterized_
types `Set{Float64}` and `Set{Int64}` describe two _concrete_ types: both are
subtypes ([`<:`](@ref)) of `Set`, but the former has `Float64` elements and the latter
has `Int64` elements.
"""
kw"{", kw"{}", kw"}"

"""
    []

Square braces are used for [indexing](@ref man-array-indexing), [indexed assignment](@ref man-indexed-assignment),
[array literals](@ref man-array-literals), and [array comprehensions](@ref man-comprehensions).
"""
kw"[", kw"[]", kw"]"

"""
    ()

Parentheses are used to group expressions, call functions, and construct [tuples](@ref Tuple) and [named tuples](@ref NamedTuple).
"""
kw"(", kw"()", kw")"

"""
    #

The number sign (or hash) character is used to begin a single-line comment.
"""
kw"#"

"""
    #= =#

A multi-line comment begins with `#=` and ends with `=#`, and may be nested.
"""
kw"#=", kw"=#"

"""
    ;

Semicolons are used as statement separators and mark the beginning of keyword arguments in function declarations or calls.
"""
kw";"

"""
    Expr(head::Symbol, args...)

A type representing compound expressions in parsed julia code (ASTs).
Each expression consists of a `head` `Symbol` identifying which kind of
expression it is (e.g. a call, for loop, conditional statement, etc.),
and subexpressions (e.g. the arguments of a call).
The subexpressions are stored in a `Vector{Any}` field called `args`.

See the manual chapter on [Metaprogramming](@ref) and the developer
documentation [Julia ASTs](@ref).

# Examples
```jldoctest
julia> Expr(:call, :+, 1, 2)
:(1 + 2)

julia> dump(:(a ? b : c))
Expr
  head: Symbol if
  args: Array{Any}((3,))
    1: Symbol a
    2: Symbol b
    3: Symbol c
```
"""
Expr

"""
    '

The conjugate transposition operator, see [`adjoint`](@ref).

# Examples
```jldoctest
julia> A = [1.0 -2.0im; 4.0im 2.0]
2×2 Array{Complex{Float64},2}:
 1.0+0.0im  -0.0-2.0im
 0.0+4.0im   2.0+0.0im

julia> A'
2×2 Array{Complex{Float64},2}:
  1.0-0.0im  0.0-4.0im
 -0.0+2.0im  2.0-0.0im
```
"""
kw"'"

"""
    \$

Interpolation operator for interpolating into e.g. [strings](@ref string-interpolation)
and [expressions](@ref man-expression-interpolation).

# Examples
```jldoctest
julia> name = "Joe"
"Joe"

julia> "My name is \$name."
"My name is Joe."
```
"""
kw"$"

"""
    const

`const` is used to declare global variables whose values will not change. In almost all code
(and particularly performance sensitive code) global variables should be declared
constant in this way.

```julia
const x = 5
```

Multiple variables can be declared within a single `const`:
```julia
const y, z = 7, 11
```

Note that `const` only applies to one `=` operation, therefore `const x = y = 1`
declares `x` to be constant but not `y`. On the other hand, `const x = const y = 1`
declares both `x` and `y` constant.

Note that "constant-ness" does not extend into mutable containers; only the
association between a variable and its value is constant.
If `x` is an array or dictionary (for example) you can still modify, add, or remove elements.

In some cases changing the value of a `const` variable gives a warning instead of
an error.
However, this can produce unpredictable behavior or corrupt the state of your program,
and so should be avoided.
This feature is intended only for convenience during interactive use.
"""
kw"const"

"""
    function

Functions are defined with the `function` keyword:

```julia
function add(a, b)
    return a + b
end
```
Or the short form notation:

```julia
add(a, b) = a + b
```

The use of the [`return`](@ref) keyword is exactly the same as in other languages,
but is often optional. A function without an explicit `return` statement will return
the last expression in the function body.
"""
kw"function"

"""
    return

`return x` causes the enclosing function to exit early, passing the given value `x`
back to its caller. `return` by itself with no value is equivalent to `return nothing`
(see [`nothing`](@ref)).

```julia
function compare(a, b)
    a == b && return "equal to"
    a < b ? "less than" : "greater than"
end
```
In general you can place a `return` statement anywhere within a function body, including
within deeply nested loops or conditionals, but be careful with `do` blocks. For
example:

```julia
function test1(xs)
    for x in xs
        iseven(x) && return 2x
    end
end

function test2(xs)
    map(xs) do x
        iseven(x) && return 2x
        x
    end
end
```
In the first example, the return breaks out of `test1` as soon as it hits
an even number, so `test1([5,6,7])` returns `12`.

You might expect the second example to behave the same way, but in fact the `return`
there only breaks out of the *inner* function (inside the `do` block) and gives a value
back to `map`. `test2([5,6,7])` then returns `[5,12,7]`.

When used in a top-level expression (i.e. outside any function), `return` causes
the entire current top-level expression to terminate early.
"""
kw"return"

"""
    if/elseif/else

`if`/`elseif`/`else` performs conditional evaluation, which allows portions of code to
be evaluated or not evaluated depending on the value of a boolean expression. Here is
the anatomy of the `if`/`elseif`/`else` conditional syntax:

```julia
if x < y
    println("x is less than y")
elseif x > y
    println("x is greater than y")
else
    println("x is equal to y")
end
```
If the condition expression `x < y` is true, then the corresponding block is evaluated;
otherwise the condition expression `x > y` is evaluated, and if it is true, the
corresponding block is evaluated; if neither expression is true, the `else` block is
evaluated. The `elseif` and `else` blocks are optional, and as many `elseif` blocks as
desired can be used.
"""
kw"if", kw"elseif", kw"else"

"""
    a ? b : c

Short form for conditionals; read "if `a`, evaluate `b` otherwise evaluate `c`".
Also known as the [ternary operator](https://en.wikipedia.org/wiki/%3F:).

This syntax is equivalent to `if a; b else c end`, but is often used to
emphasize the value `b`-or-`c` which is being used as part of a larger
expression, rather than the side effects that evaluating `b` or `c` may have.

See the manual section on [control flow](@ref man-conditional-evaluation) for more details.

# Examples
```
julia> x = 1; y = 2;

julia> println(x > y ? "x is larger" : "y is larger")
y is larger
```
"""
kw"?", kw"?:"

"""
    for

`for` loops repeatedly evaluate a block of statements while
iterating over a sequence of values.

# Examples
```jldoctest
julia> for i in [1, 4, 0]
           println(i)
       end
1
4
0
```
"""
kw"for"

"""
    while

`while` loops repeatedly evaluate a conditional expression, and continue evaluating the
body of the while loop as long as the expression remains true. If the condition
expression is false when the while loop is first reached, the body is never evaluated.

# Examples
```jldoctest
julia> i = 1
1

julia> while i < 5
           println(i)
           global i += 1
       end
1
2
3
4
```
"""
kw"while"

"""
    end

`end` marks the conclusion of a block of expressions, for example
[`module`](@ref), [`struct`](@ref), [`mutable struct`](@ref),
[`begin`](@ref), [`let`](@ref), [`for`](@ref) etc.
`end` may also be used when indexing into an array to represent
the last index of a dimension.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> A[end, :]
2-element Array{Int64,1}:
 3
 4
```
"""
kw"end"

"""
    try/catch

A `try`/`catch` statement allows intercepting errors (exceptions) thrown
by [`throw`](@ref) so that program execution can continue.
For example, the following code attempts to write a file, but warns the user
and proceeds instead of terminating execution if the file cannot be written:

```julia
try
    open("/danger", "w") do f
        println(f, "Hello")
    end
catch
    @warn "Could not write file."
end
```

or, when the file cannot be read into a variable:

```julia
lines = try
    open("/danger", "r") do f
        readlines(f)
    end
catch
    @warn "File not found."
end
```

The syntax `catch e` (where `e` is any variable) assigns the thrown
exception object to the given variable within the `catch` block.

The power of the `try`/`catch` construct lies in the ability to unwind a deeply
nested computation immediately to a much higher level in the stack of calling functions.
"""
kw"try", kw"catch"

"""
    finally

Run some code when a given block of code exits, regardless
of how it exits. For example, here is how we can guarantee that an opened file is
closed:

```julia
f = open("file")
try
    operate_on_file(f)
finally
    close(f)
end
```

When control leaves the [`try`](@ref) block (for example, due to a [`return`](@ref), or just finishing
normally), [`close(f)`](@ref) will be executed. If the `try` block exits due to an exception,
the exception will continue propagating. A `catch` block may be combined with `try` and
`finally` as well. In this case the `finally` block will run after `catch` has handled
the error.
"""
kw"finally"

"""
    break

Break out of a loop immediately.

# Examples
```jldoctest
julia> i = 0
0

julia> while true
           global i += 1
           i > 5 && break
           println(i)
       end
1
2
3
4
5
```
"""
kw"break"

"""
    continue

Skip the rest of the current loop iteration.

# Examples
```jldoctest
julia> for i = 1:6
           iseven(i) && continue
           println(i)
       end
1
3
5
```
"""
kw"continue"

"""
    do

Create an anonymous function and pass it as the first argument to
a function call.
For example:

```julia
map(1:10) do x
    2x
end
```

is equivalent to `map(x->2x, 1:10)`.

Use multiple arguments like so:

```julia
map(1:10, 11:20) do x, y
    x + y
end
```
"""
kw"do"

"""
    ...

The "splat" operator, `...`, represents a sequence of arguments.
`...` can be used in function definitions, to indicate that the function
accepts an arbitrary number of arguments.
`...` can also be used to apply a function to a sequence of arguments.

# Examples
```jldoctest
julia> add(xs...) = reduce(+, xs)
add (generic function with 1 method)

julia> add(1, 2, 3, 4, 5)
15

julia> add([1, 2, 3]...)
6

julia> add(7, 1:100..., 1000:1100...)
111107
```
"""
kw"..."

"""
    ;

`;` has a similar role in Julia as in many C-like languages, and is used to delimit the
end of the previous statement. `;` is not necessary after new lines, but can be used to
separate statements on a single line or to join statements into a single expression.
`;` is also used to suppress output printing in the REPL and similar interfaces.

# Examples
```julia
julia> function foo()
           x = "Hello, "; x *= "World!"
           return x
       end
foo (generic function with 1 method)

julia> bar() = (x = "Hello, Mars!"; return x)
bar (generic function with 1 method)

julia> foo();

julia> bar()
"Hello, Mars!"
```
"""
kw";"

"""
    x && y

Short-circuiting boolean AND.
"""
kw"&&"

"""
    x || y

Short-circuiting boolean OR.
"""
kw"||"

"""
    ccall((function_name, library), returntype, (argtype1, ...), argvalue1, ...)
    ccall(function_name, returntype, (argtype1, ...), argvalue1, ...)
    ccall(function_pointer, returntype, (argtype1, ...), argvalue1, ...)

Call a function in a C-exported shared library, specified by the tuple `(function_name, library)`,
where each component is either a string or symbol. Instead of specifying a library,
one can also use a `function_name` symbol or string, which is resolved in the current process.
Alternatively, `ccall` may also be used to call a function pointer `function_pointer`, such as one returned by `dlsym`.

Note that the argument type tuple must be a literal tuple, and not a tuple-valued
variable or expression.

Each `argvalue` to the `ccall` will be converted to the corresponding
`argtype`, by automatic insertion of calls to `unsafe_convert(argtype,
cconvert(argtype, argvalue))`. (See also the documentation for
[`unsafe_convert`](@ref Base.unsafe_convert) and [`cconvert`](@ref Base.cconvert) for further details.)
In most cases, this simply results in a call to `convert(argtype, argvalue)`.
"""
kw"ccall"

"""
    llvmcall(IR::String, ReturnType, (ArgumentType1, ...), ArgumentValue1, ...)
    llvmcall((declarations::String, IR::String), ReturnType, (ArgumentType1, ...), ArgumentValue1, ...)

Call LLVM IR string in the first argument. Similar to an LLVM function `define` block,
arguments are available as consecutive unnamed SSA variables (%0, %1, etc.).

The optional declarations string contains external functions declarations that are
necessary for llvm to compile the IR string. Multiple declarations can be passed in by
separating them with line breaks.

Note that the argument type tuple must be a literal tuple, and not a tuple-valued
variable or expression.

Each `ArgumentValue` to `llvmcall` will be converted to the corresponding
`ArgumentType`, by automatic insertion of calls to `unsafe_convert(ArgumentType,
cconvert(ArgumentType, ArgumentValue))`. (See also the documentation for
[`unsafe_convert`](@ref Base.unsafe_convert) and [`cconvert`](@ref Base.cconvert) for further details.)
In most cases, this simply results in a call to `convert(ArgumentType, ArgumentValue)`.

See `test/llvmcall.jl` for usage examples.
"""
Core.Intrinsics.llvmcall

"""
    begin

`begin...end` denotes a block of code.

```julia
begin
    println("Hello, ")
    println("World!")
end
```

Usually `begin` will not be necessary, since keywords such as [`function`](@ref) and [`let`](@ref)
implicitly begin blocks of code. See also [`;`](@ref).
"""
kw"begin"

"""
    struct

The most commonly used kind of type in Julia is a struct, specified as a name and a
set of fields.

```julia
struct Point
    x
    y
end
```

Fields can have type restrictions, which may be parameterized:

```julia
struct Point{X}
    x::X
    y::Float64
end
```

A struct can also declare an abstract super type via `<:` syntax:

```julia
struct Point <: AbstractPoint
    x
    y
end
```

`struct`s are immutable by default; an instance of one of these types cannot
be modified after construction. Use [`mutable struct`](@ref) instead to declare a
type whose instances can be modified.

See the manual section on [Composite Types](@ref) for more details,
such as how to define constructors.
"""
kw"struct"

"""
    mutable struct

`mutable struct` is similar to [`struct`](@ref), but additionally allows the
fields of the type to be set after construction. See the manual section on
[Composite Types](@ref) for more information.
"""
kw"mutable struct"

"""
    new

Special function available to inner constructors which created a new object
of the type.
See the manual section on [Inner Constructor Methods](@ref man-inner-constructor-methods)
for more information.
"""
kw"new"

"""
    where

The `where` keyword creates a type that is an iterated union of other types, over all
values of some variable. For example `Vector{T} where T<:Real` includes all [`Vector`](@ref)s
where the element type is some kind of `Real` number.

The variable bound defaults to [`Any`](@ref) if it is omitted:

```julia
Vector{T} where T    # short for `where T<:Any`
```
Variables can also have lower bounds:

```julia
Vector{T} where T>:Int
Vector{T} where Int<:T<:Real
```
There is also a concise syntax for nested `where` expressions. For example, this:

```julia
Pair{T, S} where S<:Array{T} where T<:Number
```
can be shortened to:

```julia
Pair{T, S} where {T<:Number, S<:Array{T}}
```
This form is often found on method signatures.

Note that in this form, the variables are listed outermost-first. This matches the
order in which variables are substituted when a type is "applied" to parameter values
using the syntax `T{p1, p2, ...}`.
"""
kw"where"

"""
    var

The syntax `var"#example#"` refers to a variable named `Symbol("#example#")`,
even though `#example#` is not a valid Julia identifier name.

This can be useful for interoperability with programming languages which have
different rules for the construction of valid identifiers. For example, to
refer to the `R` variable `draw.segments`, you can use `var"draw.segments"` in
your Julia code.

It is also used to `show` julia source code which has gone through macro
hygiene or otherwise contains variable names which can't be parsed normally.

Note that this syntax requires parser support so it is expanded directly by the
parser rather than being implemented as a normal string macro `@var_str`.

!!! compat "Julia 1.3"
    This syntax requires at least Julia 1.3.

"""
kw"var\"name\"", kw"@var_str"

"""
    ans

A variable referring to the last computed value, automatically set at the interactive prompt.
"""
kw"ans"

"""
    devnull

Used in a stream redirect to discard all data written to it. Essentially equivalent to
`/dev/null` on Unix or `NUL` on Windows. Usage:

```julia
run(pipeline(`cat test.txt`, devnull))
```
"""
devnull

# doc strings for code in boot.jl and built-ins

"""
    Nothing

A type with no fields that is the type of [`nothing`](@ref).
"""
Nothing

"""
    nothing

The singleton instance of type [`Nothing`](@ref), used by convention when there is no value to return
(as in a C `void` function) or when a variable or field holds no value.
"""
nothing

"""
    Core.TypeofBottom

The singleton type containing only the value `Union{}` (which represents the empty type).
"""
Core.TypeofBottom

"""
    Core.Type{T}

`Core.Type` is an abstract type which has all type objects as its instances.
The only instance of the singleton type `Core.Type{T} where T` is the object
`T`.

# Examples
```jldoctest
julia> isa(Type{Float64}, Type)
true

julia> isa(Float64, Type)
true

julia> isa(Real, Type{Float64})
false

julia> isa(Real, Type{Real})
true
```
"""
Core.Type

"""
    DataType <: Type{T}

`DataType` represents explicitly declared types that have names, explicitly
declared supertypes, and, optionally, parameters.  Every concrete value in the
system is an instance of some `DataType`.

# Examples
```jldoctest
julia> typeof(Real)
DataType

julia> typeof(Int)
DataType

julia> struct Point
           x::Int
           y
       end

julia> typeof(Point)
DataType
```
"""
Core.DataType

"""
    Function

Abstract type of all functions.

# Examples
```jldoctest
julia> isa(+, Function)
true

julia> typeof(sin)
typeof(sin)

julia> ans <: Function
true
```
"""
Function

"""
    ReadOnlyMemoryError()

An operation tried to write to memory that is read-only.
"""
ReadOnlyMemoryError

"""
    ErrorException(msg)

Generic error type. The error message, in the `.msg` field, may provide more specific details.

# Examples
```jldoctest
julia> ex = ErrorException("I've done a bad thing");

julia> ex.msg
"I've done a bad thing"
```
"""
ErrorException

"""
    WrappedException(msg)

Generic type for `Exception`s wrapping another `Exception`, such as `LoadError` and
`InitError`. Those exceptions contain information about the root cause of an
exception. Subtypes define a field `error` containing the causing `Exception`.
"""
Core.WrappedException

"""
    UndefRefError()

The item or field is not defined for the given object.

# Examples
```jldoctest
julia> struct MyType
           a::Vector{Int}
           MyType() = new()
       end

julia> A = MyType()
MyType(#undef)

julia> A.a
ERROR: UndefRefError: access to undefined reference
Stacktrace:
[...]
```
"""
UndefRefError

"""
    Float32(x [, mode::RoundingMode])

Create a `Float32` from `x`. If `x` is not exactly representable then `mode` determines how
`x` is rounded.

# Examples
```jldoctest
julia> Float32(1/3, RoundDown)
0.3333333f0

julia> Float32(1/3, RoundUp)
0.33333334f0
```

See [`RoundingMode`](@ref) for available rounding modes.
"""
Float32(x)

"""
    Float64(x [, mode::RoundingMode])

Create a `Float64` from `x`. If `x` is not exactly representable then `mode` determines how
`x` is rounded.

# Examples
```jldoctest
julia> Float64(pi, RoundDown)
3.141592653589793

julia> Float64(pi, RoundUp)
3.1415926535897936
```

See [`RoundingMode`](@ref) for available rounding modes.
"""
Float64(x)

"""
    OutOfMemoryError()

An operation allocated too much memory for either the system or the garbage collector to
handle properly.
"""
OutOfMemoryError

"""
    BoundsError([a],[i])

An indexing operation into an array, `a`, tried to access an out-of-bounds element at index `i`.

# Examples
```jldoctest; filter = r"Stacktrace:(\\n \\[[0-9]+\\].*)*"
julia> A = fill(1.0, 7);

julia> A[8]
ERROR: BoundsError: attempt to access 7-element Array{Float64,1} at index [8]
Stacktrace:
 [1] getindex(::Array{Float64,1}, ::Int64) at ./array.jl:660
 [2] top-level scope

julia> B = fill(1.0, (2,3));

julia> B[2, 4]
ERROR: BoundsError: attempt to access 2×3 Array{Float64,2} at index [2, 4]
Stacktrace:
 [1] getindex(::Array{Float64,2}, ::Int64, ::Int64) at ./array.jl:661
 [2] top-level scope

julia> B[9]
ERROR: BoundsError: attempt to access 2×3 Array{Float64,2} at index [9]
Stacktrace:
 [1] getindex(::Array{Float64,2}, ::Int64) at ./array.jl:660
 [2] top-level scope
```
"""
BoundsError

"""
    InexactError(name::Symbol, T, val)

Cannot exactly convert `val` to type `T` in a method of function `name`.

# Examples
```jldoctest
julia> convert(Float64, 1+2im)
ERROR: InexactError: Float64(1 + 2im)
Stacktrace:
[...]
```
"""
InexactError

"""
    DomainError(val)
    DomainError(val, msg)

The argument `val` to a function or constructor is outside the valid domain.

# Examples
```jldoctest
julia> sqrt(-1)
ERROR: DomainError with -1.0:
sqrt will only return a complex result if called with a complex argument. Try sqrt(Complex(x)).
Stacktrace:
[...]
```
"""
DomainError

"""
    Task(func)

Create a `Task` (i.e. coroutine) to execute the given function `func` (which must be
callable with no arguments). The task exits when this function returns.

# Examples
```jldoctest
julia> a() = sum(i for i in 1:1000);

julia> b = Task(a);
```

In this example, `b` is a runnable `Task` that hasn't started yet.
"""
Task

"""
    StackOverflowError()

The function call grew beyond the size of the call stack. This usually happens when a call
recurses infinitely.
"""
StackOverflowError

"""
    nfields(x) -> Int

Get the number of fields in the given object.

# Examples
```jldoctest
julia> a = 1//2;

julia> nfields(a)
2

julia> b = 1
1

julia> nfields(b)
0

julia> ex = ErrorException("I've done a bad thing");

julia> nfields(ex)
1
```

In these examples, `a` is a [`Rational`](@ref), which has two fields.
`b` is an `Int`, which is a primitive bitstype with no fields at all.
`ex` is an [`ErrorException`](@ref), which has one field.
"""
nfields

"""
    UndefVarError(var::Symbol)

A symbol in the current scope is not defined.

# Examples
```jldoctest
julia> a
ERROR: UndefVarError: a not defined

julia> a = 1;

julia> a
1
```
"""
UndefVarError

"""
    UndefKeywordError(var::Symbol)

The required keyword argument `var` was not assigned in a function call.

# Examples
```jldoctest; filter = r"Stacktrace:(\\n \\[[0-9]+\\].*)*"
julia> function my_func(;my_arg)
           return my_arg + 1
       end
my_func (generic function with 1 method)

julia> my_func()
ERROR: UndefKeywordError: keyword argument my_arg not assigned
Stacktrace:
 [1] my_func() at ./REPL[1]:2
 [2] top-level scope at REPL[2]:1
```
"""
UndefKeywordError

"""
    OverflowError(msg)

The result of an expression is too large for the specified type and will cause a wraparound.
"""
OverflowError

"""
    TypeError(func::Symbol, context::AbstractString, expected::Type, got)

A type assertion failure, or calling an intrinsic function with an incorrect argument type.
"""
TypeError

"""
    InterruptException()

The process was stopped by a terminal interrupt (CTRL+C).

Note that, in Julia script started without `-i` (interactive) option,
`InterruptException` is not thrown by default.  Calling
[`Base.exit_on_sigint(false)`](@ref Base.exit_on_sigint) in the script
can recover the behavior of the REPL.  Alternatively, a Julia script
can be started with

```sh
julia -e "include(popfirst!(ARGS))" script.jl
```

to let `InterruptException` be thrown by CTRL+C during the execution.
"""
InterruptException

"""
    applicable(f, args...) -> Bool

Determine whether the given generic function has a method applicable to the given arguments.

See also [`hasmethod`](@ref).

# Examples
```jldoctest
julia> function f(x, y)
           x + y
       end;

julia> applicable(f, 1)
false

julia> applicable(f, 1, 2)
true
```
"""
applicable

"""
    invoke(f, argtypes::Type, args...; kwargs...)

Invoke a method for the given generic function `f` matching the specified types `argtypes` on the
specified arguments `args` and passing the keyword arguments `kwargs`. The arguments `args` must
conform with the specified types in `argtypes`, i.e. conversion is not automatically performed.
This method allows invoking a method other than the most specific matching method, which is useful
when the behavior of a more general definition is explicitly needed (often as part of the
implementation of a more specific method of the same function).

Be careful when using `invoke` for functions that you don't write.  What definition is used
for given `argtypes` is an implementation detail unless the function is explicitly states
that calling with certain `argtypes` is a part of public API.  For example, the change
between `f1` and `f2` in the example below is usually considered compatible because the
change is invisible by the caller with a normal (non-`invoke`) call.  However, the change is
visible if you use `invoke`.

# Examples
```jldoctest
julia> f(x::Real) = x^2;

julia> f(x::Integer) = 1 + invoke(f, Tuple{Real}, x);

julia> f(2)
5

julia> f1(::Integer) = Integer
       f1(::Real) = Real;

julia> f2(x::Real) = _f2(x)
       _f2(::Integer) = Integer
       _f2(_) = Real;

julia> f1(1)
Integer

julia> f2(1)
Integer

julia> invoke(f1, Tuple{Real}, 1)
Real

julia> invoke(f2, Tuple{Real}, 1)
Integer
```
"""
invoke

"""
    isa(x, type) -> Bool

Determine whether `x` is of the given `type`. Can also be used as an infix operator, e.g.
`x isa type`.

# Examples
```jldoctest
julia> isa(1, Int)
true

julia> isa(1, Matrix)
false

julia> isa(1, Char)
false

julia> isa(1, Number)
true

julia> 1 isa Number
true
```
"""
isa

"""
    DivideError()

Integer division was attempted with a denominator value of 0.

# Examples
```jldoctest
julia> 2/0
Inf

julia> div(2, 0)
ERROR: DivideError: integer division error
Stacktrace:
[...]
```
"""
DivideError

"""
    Number

Abstract supertype for all number types.
"""
Number

"""
    Real <: Number

Abstract supertype for all real numbers.
"""
Real

"""
    AbstractFloat <: Real

Abstract supertype for all floating point numbers.
"""
AbstractFloat

"""
    Integer <: Real

Abstract supertype for all integers.
"""
Integer

"""
    Signed <: Integer

Abstract supertype for all signed integers.
"""
Signed

"""
    Unsigned <: Integer

Abstract supertype for all unsigned integers.
"""
Unsigned

"""
    Bool <: Integer

Boolean type, containing the values `true` and `false`.

`Bool` is a kind of number: `false` is numerically
equal to `0` and `true` is numerically equal to `1`.
Moreover, `false` acts as a multiplicative "strong zero":

```jldoctest
julia> false == 0
true

julia> true == 1
true

julia> 0 * NaN
NaN

julia> false * NaN
0.0
```
"""
Bool

for (bit, sign, exp, frac) in ((16, 1, 5, 10), (32, 1, 8, 23), (64, 1, 11, 52))
    @eval begin
        """
            Float$($bit) <: AbstractFloat

        $($bit)-bit floating point number type (IEEE 754 standard).

        Binary format: $($sign) sign, $($exp) exponent, $($frac) fraction bits.
        """
        $(Symbol("Float", bit))
    end
end

for bit in (8, 16, 32, 64, 128)
    @eval begin
        """
            Int$($bit) <: Signed

        $($bit)-bit signed integer type.
        """
        $(Symbol("Int", bit))

        """
            UInt$($bit) <: Unsigned

        $($bit)-bit unsigned integer type.
        """
        $(Symbol("UInt", bit))
    end
end

"""
    Symbol

The type of object used to represent identifiers in parsed julia code (ASTs).
Also often used as a name or label to identify an entity (e.g. as a dictionary key).
`Symbol`s can be entered using the `:` quote operator:
```jldoctest
julia> :name
:name

julia> typeof(:name)
Symbol

julia> x = 42
42

julia> eval(:x)
42
```
`Symbol`s can also be constructed from strings or other values by calling the
constructor `Symbol(x...)`.

`Symbol`s are immutable and should be compared using `===`.
The implementation re-uses the same object for all `Symbol`s with the same name,
so comparison tends to be efficient (it can just compare pointers).

Unlike strings, `Symbol`s are "atomic" or "scalar" entities that do not support
iteration over characters.
"""
Symbol

"""
    Symbol(x...) -> Symbol

Create a [`Symbol`](@ref) by concatenating the string representations of the arguments together.

# Examples
```jldoctest
julia> Symbol("my", "name")
:myname

julia> Symbol("day", 4)
:day4
```
"""
Symbol(x...)

"""
    tuple(xs...)

Construct a tuple of the given objects.

# Examples
```jldoctest
julia> tuple(1, 'a', pi)
(1, 'a', π)
```
"""
tuple

"""
    getfield(value, name::Symbol)
    getfield(value, i::Int)

Extract a field from a composite `value` by name or position.
See also [`getproperty`](@ref Base.getproperty) and [`fieldnames`](@ref).

# Examples
```jldoctest
julia> a = 1//2
1//2

julia> getfield(a, :num)
1

julia> a.num
1

julia> getfield(a, 1)
1
```
"""
getfield

"""
    setfield!(value, name::Symbol, x)

Assign `x` to a named field in `value` of composite type.
The `value` must be mutable and `x` must be a subtype of `fieldtype(typeof(value), name)`.
See also [`setproperty!`](@ref Base.setproperty!).

# Examples
```jldoctest
julia> mutable struct MyMutableStruct
           field::Int
       end

julia> a = MyMutableStruct(1);

julia> setfield!(a, :field, 2);

julia> getfield(a, :field)
2

julia> a = 1//2
1//2

julia> setfield!(a, :num, 3);
ERROR: setfield! immutable struct of type Rational cannot be changed
```
"""
setfield!

"""
    typeof(x)

Get the concrete type of `x`.

# Examples
```jldoctest
julia> a = 1//2;

julia> typeof(a)
Rational{Int64}

julia> M = [1 2; 3.5 4];

julia> typeof(M)
Array{Float64,2}
```
"""
typeof

"""
    isdefined(m::Module, s::Symbol)
    isdefined(object, s::Symbol)
    isdefined(object, index::Int)

Tests whether a global variable or object field is defined. The arguments can be a module and a symbol
or a composite object and field name (as a symbol) or index.

To test whether an array element is defined, use [`isassigned`](@ref) instead.

See also [`@isdefined`](@ref).

# Examples
```jldoctest
julia> isdefined(Base, :sum)
true

julia> isdefined(Base, :NonExistentMethod)
false

julia> a = 1//2;

julia> isdefined(a, 2)
true

julia> isdefined(a, 3)
false

julia> isdefined(a, :num)
true

julia> isdefined(a, :numerator)
false
```
"""
isdefined


"""
    Vector{T}(undef, n)

Construct an uninitialized [`Vector{T}`](@ref) of length `n`. See [`undef`](@ref).

# Examples
```julia-repl
julia> Vector{Float64}(undef, 3)
3-element Array{Float64,1}:
 6.90966e-310
 6.90966e-310
 6.90966e-310
```
"""
Vector{T}(::UndefInitializer, n)

"""
    Vector{T}(nothing, m)

Construct a [`Vector{T}`](@ref) of length `m`, initialized with
[`nothing`](@ref) entries. Element type `T` must be able to hold
these values, i.e. `Nothing <: T`.

# Examples
```jldoctest
julia> Vector{Union{Nothing, String}}(nothing, 2)
2-element Array{Union{Nothing, String},1}:
 nothing
 nothing
```
"""
Vector{T}(::Nothing, n)

"""
    Vector{T}(missing, m)

Construct a [`Vector{T}`](@ref) of length `m`, initialized with
[`missing`](@ref) entries. Element type `T` must be able to hold
these values, i.e. `Missing <: T`.

# Examples
```jldoctest
julia> Vector{Union{Missing, String}}(missing, 2)
2-element Array{Union{Missing, String},1}:
 missing
 missing
```
"""
Vector{T}(::Missing, n)

"""
    Matrix{T}(undef, m, n)

Construct an uninitialized [`Matrix{T}`](@ref) of size `m`×`n`. See [`undef`](@ref).

# Examples
```julia-repl
julia> Matrix{Float64}(undef, 2, 3)
2×3 Array{Float64,2}:
 6.93517e-310  6.93517e-310  6.93517e-310
 6.93517e-310  6.93517e-310  1.29396e-320
```
"""
Matrix{T}(::UndefInitializer, m, n)

"""
    Matrix{T}(nothing, m, n)

Construct a [`Matrix{T}`](@ref) of size `m`×`n`, initialized with
[`nothing`](@ref) entries. Element type `T` must be able to hold
these values, i.e. `Nothing <: T`.

# Examples
```jldoctest
julia> Matrix{Union{Nothing, String}}(nothing, 2, 3)
2×3 Array{Union{Nothing, String},2}:
 nothing  nothing  nothing
 nothing  nothing  nothing
```
"""
Matrix{T}(::Nothing, m, n)

"""
    Matrix{T}(missing, m, n)

Construct a [`Matrix{T}`](@ref) of size `m`×`n`, initialized with
[`missing`](@ref) entries. Element type `T` must be able to hold
these values, i.e. `Missing <: T`.

# Examples
```jldoctest
julia> Matrix{Union{Missing, String}}(missing, 2, 3)
2×3 Array{Union{Missing, String},2}:
 missing  missing  missing
 missing  missing  missing
```
"""
Matrix{T}(::Missing, m, n)

"""
    Array{T}(undef, dims)
    Array{T,N}(undef, dims)

Construct an uninitialized `N`-dimensional [`Array`](@ref)
containing elements of type `T`. `N` can either be supplied explicitly,
as in `Array{T,N}(undef, dims)`, or be determined by the length or number of `dims`.
`dims` may be a tuple or a series of integer arguments corresponding to the lengths
in each dimension. If the rank `N` is supplied explicitly, then it must
match the length or number of `dims`. See [`undef`](@ref).

# Examples
```julia-repl
julia> A = Array{Float64,2}(undef, 2, 3) # N given explicitly
2×3 Array{Float64,2}:
 6.90198e-310  6.90198e-310  6.90198e-310
 6.90198e-310  6.90198e-310  0.0

julia> B = Array{Float64}(undef, 2) # N determined by the input
2-element Array{Float64,1}:
 1.87103e-320
 0.0
```
"""
Array{T,N}(::UndefInitializer, dims)

"""
    Array{T}(nothing, dims)
    Array{T,N}(nothing, dims)

Construct an `N`-dimensional [`Array`](@ref) containing elements of type `T`,
initialized with [`nothing`](@ref) entries. Element type `T` must be able
to hold these values, i.e. `Nothing <: T`.

# Examples
```jldoctest
julia> Array{Union{Nothing, String}}(nothing, 2)
2-element Array{Union{Nothing, String},1}:
 nothing
 nothing

julia> Array{Union{Nothing, Int}}(nothing, 2, 3)
2×3 Array{Union{Nothing, Int64},2}:
 nothing  nothing  nothing
 nothing  nothing  nothing
```
"""
Array{T,N}(::Nothing, dims)


"""
    Array{T}(missing, dims)
    Array{T,N}(missing, dims)

Construct an `N`-dimensional [`Array`](@ref) containing elements of type `T`,
initialized with [`missing`](@ref) entries. Element type `T` must be able
to hold these values, i.e. `Missing <: T`.

# Examples
```jldoctest
julia> Array{Union{Missing, String}}(missing, 2)
2-element Array{Union{Missing, String},1}:
 missing
 missing

julia> Array{Union{Missing, Int}}(missing, 2, 3)
2×3 Array{Union{Missing, Int64},2}:
 missing  missing  missing
 missing  missing  missing
```
"""
Array{T,N}(::Missing, dims)

"""
    UndefInitializer

Singleton type used in array initialization, indicating the array-constructor-caller
would like an uninitialized array. See also [`undef`](@ref),
an alias for `UndefInitializer()`.

# Examples
```julia-repl
julia> Array{Float64,1}(UndefInitializer(), 3)
3-element Array{Float64,1}:
 2.2752528595e-314
 2.202942107e-314
 2.275252907e-314
```
"""
UndefInitializer

"""
    undef

Alias for `UndefInitializer()`, which constructs an instance of the singleton type
[`UndefInitializer`](@ref), used in array initialization to indicate the
array-constructor-caller would like an uninitialized array.

# Examples
```julia-repl
julia> Array{Float64,1}(undef, 3)
3-element Array{Float64,1}:
 2.2752528595e-314
 2.202942107e-314
 2.275252907e-314
```
"""
undef

"""
    Ptr{T}()

Creates a null pointer to type `T`.
"""
Ptr{T}()

"""
    +(x, y...)

Addition operator. `x+y+z+...` calls this function with all arguments, i.e. `+(x, y, z, ...)`.

# Examples
```jldoctest
julia> 1 + 20 + 4
25

julia> +(1, 20, 4)
25
```
"""
(+)(x, y...)

"""
    -(x)

Unary minus operator.

# Examples
```jldoctest
julia> -1
-1

julia> -(2)
-2

julia> -[1 2; 3 4]
2×2 Array{Int64,2}:
 -1  -2
 -3  -4
```
"""
-(x)

"""
    -(x, y)

Subtraction operator.

# Examples
```jldoctest
julia> 2 - 3
-1

julia> -(2, 4.5)
-2.5
```
"""
-(x, y)

"""
    *(x, y...)

Multiplication operator. `x*y*z*...` calls this function with all arguments, i.e. `*(x, y, z, ...)`.

# Examples
```jldoctest
julia> 2 * 7 * 8
112

julia> *(2, 7, 8)
112
```
"""
(*)(x, y...)

"""
    /(x, y)

Right division operator: multiplication of `x` by the inverse of `y` on the right. Gives
floating-point results for integer arguments.

# Examples
```jldoctest
julia> 1/2
0.5

julia> 4/2
2.0

julia> 4.5/2
2.25
```
"""
/(x, y)

"""
    ArgumentError(msg)

The parameters to a function call do not match a valid signature. Argument `msg` is a
descriptive error string.
"""
ArgumentError

"""
    MethodError(f, args)

A method with the required type signature does not exist in the given generic function.
Alternatively, there is no unique most-specific method.
"""
MethodError

"""
    AssertionError([msg])

The asserted condition did not evaluate to `true`.
Optional argument `msg` is a descriptive error string.

# Examples
```jldoctest
julia> @assert false "this is not true"
ERROR: AssertionError: this is not true
```

`AssertionError` is usually thrown from [`@assert`](@ref).
"""
AssertionError

"""
    LoadError(file::AbstractString, line::Int, error)

An error occurred while [`include`](@ref Base.include)ing, [`require`](@ref Base.require)ing, or [`using`](@ref) a file. The error specifics
should be available in the `.error` field.
"""
LoadError

"""
    InitError(mod::Symbol, error)

An error occurred when running a module's `__init__` function. The actual error thrown is
available in the `.error` field.
"""
InitError

"""
    Any::DataType

`Any` is the union of all types. It has the defining property `isa(x, Any) == true` for any `x`. `Any` therefore
describes the entire universe of possible values. For example `Integer` is a subset of `Any` that includes `Int`,
`Int8`, and other integer types.
"""
Any

"""
    Union{}

`Union{}`, the empty [`Union`](@ref) of types, is the type that has no values. That is, it has the defining
property `isa(x, Union{}) == false` for any `x`. `Base.Bottom` is defined as its alias and the type of `Union{}`
is `Core.TypeofBottom`.

# Examples
```jldoctest
julia> isa(nothing, Union{})
false
```
"""
kw"Union{}", Base.Bottom

"""
    Union{Types...}

A type union is an abstract type which includes all instances of any of its argument types. The empty
union [`Union{}`](@ref) is the bottom type of Julia.

# Examples
```jldoctest
julia> IntOrString = Union{Int,AbstractString}
Union{Int64, AbstractString}

julia> 1 :: IntOrString
1

julia> "Hello!" :: IntOrString
"Hello!"

julia> 1.0 :: IntOrString
ERROR: TypeError: in typeassert, expected Union{Int64, AbstractString}, got a value of type Float64
```
"""
Union


"""
    UnionAll

A union of types over all values of a type parameter. `UnionAll` is used to describe parametric types
where the values of some parameters are not known.

# Examples
```jldoctest
julia> typeof(Vector)
UnionAll

julia> typeof(Vector{Int})
DataType
```
"""
UnionAll

"""
    ::

With the `::`-operator type annotations are attached to expressions and variables in programs.
See the manual section on [Type Declarations](@ref).

Outside of declarations `::` is used to assert that expressions and variables in programs have a given type.

# Examples
```jldoctest
julia> (1+2)::AbstractFloat
ERROR: TypeError: typeassert: expected AbstractFloat, got a value of type Int64

julia> (1+2)::Int
3
```
"""
kw"::"

"""
    Vararg{T,N}

The last parameter of a tuple type [`Tuple`](@ref) can be the special type `Vararg`, which denotes any
number of trailing elements. The type `Vararg{T,N}` corresponds to exactly `N` elements of type `T`.
`Vararg{T}` corresponds to zero or more elements of type `T`. `Vararg` tuple types are used to represent the
arguments accepted by varargs methods (see the section on [Varargs Functions](@ref) in the manual.)

# Examples
```jldoctest
julia> mytupletype = Tuple{AbstractString,Vararg{Int}}
Tuple{AbstractString,Vararg{Int64,N} where N}

julia> isa(("1",), mytupletype)
true

julia> isa(("1",1), mytupletype)
true

julia> isa(("1",1,2), mytupletype)
true

julia> isa(("1",1,2,3.0), mytupletype)
false
```
"""
Vararg

"""
    Tuple{Types...}

Tuples are an abstraction of the arguments of a function – without the function itself. The salient aspects of
a function's arguments are their order and their types. Therefore a tuple type is similar to a parameterized
immutable type where each parameter is the type of one field. Tuple types may have any number of parameters.

Tuple types are covariant in their parameters: `Tuple{Int}` is a subtype of `Tuple{Any}`. Therefore `Tuple{Any}`
is considered an abstract type, and tuple types are only concrete if their parameters are. Tuples do not have
field names; fields are only accessed by index.

See the manual section on [Tuple Types](@ref).
"""
Tuple

"""
    NamedTuple{names}(args::Tuple)

Construct a named tuple with the given `names` (a tuple of Symbols) from a tuple of values.
"""
NamedTuple{names}(args::Tuple)

"""
    NamedTuple{names,T}(args::Tuple)

Construct a named tuple with the given `names` (a tuple of Symbols) and field types `T`
(a `Tuple` type) from a tuple of values.
"""
NamedTuple{names,T}(args::Tuple)

"""
    NamedTuple{names}(nt::NamedTuple)

Construct a named tuple by selecting fields in `names` (a tuple of Symbols) from
another named tuple.
"""
NamedTuple{names}(nt::NamedTuple)

"""
    typeassert(x, type)

Throw a [`TypeError`](@ref) unless `x isa type`.
The syntax `x::type` calls this function.

# Examples
```jldoctest
julia> typeassert(2.5, Int)
ERROR: TypeError: in typeassert, expected Int64, got a value of type Float64
Stacktrace:
[...]
```
"""
typeassert

"""
    getproperty(value, name::Symbol)

The syntax `a.b` calls `getproperty(a, :b)`.

# Examples
```jldoctest
julia> struct MyType
           x
       end

julia> function Base.getproperty(obj::MyType, sym::Symbol)
           if sym === :special
               return obj.x + 1
           else # fallback to getfield
               return getfield(obj, sym)
           end
       end

julia> obj = MyType(1);

julia> obj.special
2

julia> obj.x
1
```

See also [`propertynames`](@ref Base.propertynames) and
[`setproperty!`](@ref Base.setproperty!).
"""
Base.getproperty

"""
    setproperty!(value, name::Symbol, x)

The syntax `a.b = c` calls `setproperty!(a, :b, c)`.

See also [`propertynames`](@ref Base.propertynames) and
[`getproperty`](@ref Base.getproperty).
"""
Base.setproperty!

"""
    StridedArray{T, N}

An `N` dimensional *strided* array with elements of type `T`. These arrays follow
the [strided array interface](@ref man-interface-strided-arrays). If `A` is a
`StridedArray`, then its elements are stored in memory with offsets, which may
vary between dimensions but are constant within a dimension. For example, `A` could
have stride 2 in dimension 1, and stride 3 in dimension 2. Incrementing `A` along
dimension `d` jumps in memory by [`strides(A, d)`] slots. Strided arrays are
particularly important and useful because they can sometimes be passed directly
as pointers to foreign language libraries like BLAS.
"""
StridedArray

"""
    StridedVector{T}

One dimensional [`StridedArray`](@ref) with elements of type `T`.
"""
StridedVector

"""
    StridedMatrix{T}

Two dimensional [`StridedArray`](@ref) with elements of type `T`.
"""
StridedMatrix

"""
    StridedVecOrMat{T}

Union type of [`StridedVector`](@ref) and [`StridedMatrix`](@ref) with elements of type `T`.
"""
StridedVecOrMat

"""
    Module

A `Module` is a separate global variable workspace. See [`module`](@ref) and the [manual section about modules](@ref modules) for details.
"""
Module

"""
    Core

`Core` is the module that contains all identifiers considered "built in" to the language, i.e. part of the core language and not libraries. Every module implicitly specifies `using Core`, since you can't do anything without those definitions.
"""
Core.Core

"""
    Main

`Main` is the top-level module, and Julia starts with `Main` set as the current module.  Variables defined at the prompt go in `Main`, and `varinfo` lists variables in `Main`.
```jldoctest
julia> @__MODULE__
Main
```
"""
Main.Main

"""
    Base

The base library of Julia. `Base` is a module that contains basic functionality (the contents of `base/`). All modules implicitly contain `using Base`, since this is needed in the vast majority of cases.
"""
Base.Base

"""
    QuoteNode

A quoted piece of code, that does not support interpolation. See the [manual section about QuoteNodes](@ref man-quote-node) for details.
"""
QuoteNode

end
