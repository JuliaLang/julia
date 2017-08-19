# This file is a part of Julia. License is MIT: https://julialang.org/license

module BaseDocs

struct Keyword
    name :: Symbol
end
macro kw_str(text) Keyword(Symbol(text)) end

"""
**Welcome to Julia $(string(VERSION)).** The full manual is available at

    https://docs.julialang.org/

as well many great tutorials and learning resources:

    https://julialang.org/learning/

For help on a specific function or macro, type `?` followed
by its name, e.g. `?cos`, or `?@time`, and press enter.
"""
kw"help", kw"?", kw"julia"

"""
`using` will load the given module or package and make some of its names available for
use (see also `export`). For example:

    using Gadfly

loads the plotting package, Gadfly, so that the `plot` function can be used.

Names can be used via dot syntax, whether they are exported or not:

    Gadfly.plot(...)

If you don't want to use the packages exports directly, see also `import`. If you're not
sure, `using` is almost definitely what you want.
"""
kw"using"

"""
    import Gadfly

`import`, like `using`, will load modules and packages for use. Unlike `using`, however,
it will *not* make any `export`ed names available for use. To use Gadfly's `plot`
function after importing it, for example, you have to write:

    Gadfly.plot(...)

Import can also be used with specific names, for example

    import Gadfly: plot, render

This syntax is used when you want to extend the modules functions with new methods.
"""
kw"import"

"""
`export` is used within modules and packages to tell Julia which functions should be
made available to the user. For example:

    module Test
    export foo # foo is exported, but bar isn't
    foo(x) = x
    bar(y) = y
    end

    using Test
    foo(1) # 1
    bar(1) # Error: bar not defined
    Test.bar(1) # 1
"""
kw"export"

"""
`abstract type` declares a type that cannot be instantiated, and serves only as a node in the
type graph, thereby describing sets of related concrete types: those concrete types
which are their descendants. Abstract types form the conceptual hierarchy which makes
Julia’s type system more than just a collection of object implementations. For example:

    abstract type Number end
    abstract type Real <: Number end

[`Number`](@ref) has no supertype, whereas [`Real`](@ref) is an abstract subtype of `Number`.
"""
kw"abstract type"

"""
`module` declares a Module, which is a separate global variable workspace.  Within a
module, you can control which names from other modules are visible (via importing), and
specify which of your names are intended to be public (via exporting). For example:

    module
    import Base.show
    export MyType, foo

    type MyType
        x
    end

    bar(x) = 2x
    foo(a::MyType) = bar(a.x) + 1
    show(io, a::MyType) = print(io, "MyType \$(a.x)")
    end

Modules allow you to create top-level definitions without worrying about name conflicts
when your code is used together with somebody else’s.
"""
kw"module"

"""
`baremodule` declares a module that does not contain `using Base`
or a definition of `eval`.  It does still import `Core`.
"""
kw"baremodule"

"""
`primitive type` declares a concrete type whose data consists only of a series of bits. Classic
examples of primitive types are integers and floating-point values. Some example built-in
primitive type declarations:

    primitive type Char 32 end
    primitive type Bool <: Integer 8 end

The number after the name indicates how many bits of storage the type requires. Currently,
only sizes that are multiples of 8 bits are supported.
The [`Bool`](@ref) declaration shows how a primitive type can be optionally
declared to be a subtype of some supertype.
"""
kw"primitive type"

"""
`macro` defines a method to include generated code in the final body of a program. A
macro maps a tuple of arguments to a returned expression, and the resulting expression
is compiled directly rather than requiring a runtime `eval()` call. Macro arguments may
include expressions, literal values, and symbols. For example:

    macro sayhello(name)
        return :( println("Hello, ", \$name) )
    end

This macro takes one argument: `name`. When `@sayhello` is encountered, the quoted
expression is expanded to interpolate the value of the argument into the final
expression.
"""
kw"macro"

"""
`importall` imports all names exported by the specified module, as if `import` were used
individually on all of them.  For example:

    importall Distributions

As with `import`, functions imported by `importall` can be extended.
"""
kw"importall"

"""
`local` introduces a new local variable. For example:

    function foo(n)
        x = 0
        for i = 1:n
            local x
            x = i
        end
        x
    end

    julia> foo(10)
    0

Here `local x` introduces a separate `x` inside the loop, so the function returns `0`.
"""
kw"local"

"""
`global x` makes `x` in the current scope and its inner scopes refer to the global
variable of that name.   In the example below, `global` is needed so the function can
modify the global variable `z`:

    z=3
    function foo()
        global z=6
    end

    julia> foo()
    6
    julia> z
    6

Without the `global` declaration in `foo()`, a new local variable would have been
created inside foo(), and the `z` in the global scope would have remained equal to `3`.
"""
kw"global"

"""
`let` statements allocate new variable bindings each time they run. Whereas an
assignment modifies an existing value location, `let` creates new locations. This
difference is only detectable in the case of variables that outlive their scope via
closures. The `let` syntax accepts a comma-separated series of assignments and variable
names:

    let var1 = value1, var2, var3 = value3
        code
    end

The assignments are evaluated in order, with each right-hand side evaluated in the scope
before the new variable on the left-hand side has been introduced. Therefore it makes
sense to write something like `let x = x`, since the two `x` variables are distinct and
have separate storage.
"""
kw"let"

"""
`quote` creates multiple expression objects in a block without using the explicit `Expr`
constructor. For example:

    ex = quote
        x = 1
        y = 2
        x + y
    end

Unlike the other means of quoting, `:( ... )`, this form introduces `QuoteNode` elements
to the expression tree, which must be considered when directly manipulating the tree.
For other purposes, `:( ... )` and `quote .. end` blocks are treated identically.
"""
kw"quote"

"""
`'` is the conjugate transposition operator:

    julia> A = reshape(1:4, 2,2)
    2×2 Array{Int64,2}:
     1  3
     2  4

    julia> A'
    2×2 Array{Int64,2}:
     1  2
     3  4

    julia> B = A + im
    2×2 Array{Complex{Int64},2}:
     1+1im  3+1im
     2+1im  4+1im

    julia> B'
    2×2 Array{Complex{Int64},2}:
     1-1im  2-1im
     3-1im  4-1im

"""
kw"'"


"""
`.'` is the transposition operator:

    julia> A = reshape(1:4, 2,2)
    2×2 Array{Int64,2}:
     1  3
     2  4

    julia> A.'
    2×2 Array{Int64,2}:
     1  2
     3  4

    julia> B = A + im
    2×2 Array{Complex{Int64},2}:
     1+1im  3+1im
     2+1im  4+1im

    julia> B.'
    2×2 Array{Complex{Int64},2}:
     1+1im  2+1im
     3+1im  4+1im

    julia> v = [1,2,3]
    3-element Array{Int64,1}:
     1
     2
     3

    julia> v.'
    1×3 RowVector{Int64,Array{Int64,1}}:
     1  2  3

"""
kw".'"

"""
`const` is used to declare global variables which are also constant. In almost all code
(and particularly performance sensitive code) global variables should be declared
constant in this way.

    const x = 5

Note that "constant-ness" is not enforced inside containers, so if `x` is an array or
dictionary (for example) you can still add and remove elements.

Technically, you can even redefine `const` variables, although this will generate a
warning from the compiler. The only strict requirement is that the *type* of the
variable does not change, which is why `const` variables are much faster than regular
globals.
"""
kw"const"

"""
Functions are defined with the `function` keyword:

    function add(a, b)
        return a + b
    end

Or the short form notation:

    add(a, b) = a + b

The use of the `return` keyword is exactly the same as in other languages, but is often
optional. When it's not used, the last expression in the function body will be returned
by default:

    function compare(a, b)
        a == b && return "equal to"
        a < b ? "less than" : "greater than"
    end
"""
kw"function"

"""
`return` can be used function bodies to exit early and return a given value, e.g.

    function compare(a, b)
        a == b && return "equal to"
        a < b ? "less than" : "greater than"
    end

In general you can place a `return` statement anywhere within a function body, including
within deeply nested loops or conditionals, but be careful with `do` blocks. For
example:

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

In the first example, the return breaks out of its enclosing function as soon as it hits
an even number, so `test1([5,6,7])` returns `12`.

You might expect the second example to behave the same way, but in fact the `return`
there only breaks out of the *inner* function (inside the `do` block) and gives a value
back to `map`. `test2([5,6,7])` then returns `[5,12,7]`.
"""
kw"return"

"""
`if`-`elseif`-`else` performs conditional evaluation, which allows portions of code to
be evaluated or not evaluated depending on the value of a boolean expression. Here is
the anatomy of the `if`-`elseif`-`else` conditional syntax:

    if x < y
        println("x is less than y")
    elseif x > y
        println("x is greater than y")
    else
        println("x is equal to y")
    end

If the condition expression `x < y` is true, then the corresponding block is evaluated;
otherwise the condition expression `x > y` is evaluated, and if it is true, the
corresponding block is evaluated; if neither expression is true, the `else` block is
evaluated. The `elseif` and `else` blocks are optional, and as many `elseif` blocks as
desired can be used.
"""
kw"if", kw"elseif", kw"else"

"""
`for` loops repeatedly evaluate the body of the loop by iterating over a sequence of
values. For example:

    for i in [1,4,0]
        println(i)
    end
"""
kw"for"

"""
`while` loops repeatedly evaluate a conditional expression, and continues evaluating the
body of the while loop so long as the expression remains `true`. If the condition
expression is false when the while loop is first reached, the body is never evaluated.
For example:

    while i <= 5
        println(i)
        i += 1
    end
"""
kw"while"

"""
`end` marks the conclusion of a block of expressions. In the example below, `end` marks
the conclusion of a `function`.

    function foo()
        println("hello, world")
    end

`end` marks the conclusion of all kinds of expression blocks: `module`, `type`, `begin`,
`let`, `for`, etc.

In addition, `end` may be used when indexing into an array to represent the last index
of each dimension:

    x[1:end, 2:end-1]
"""
kw"end"

"""
A `try/catch` statement allows for `Exception`s to be tested for. For example, a
customized square root function can be written to automatically call either the real or
complex square root method on demand using `Exception`s:

    f(x) = try
        sqrt(x)
    catch
        sqrt(complex(x, 0))
    end

`try/catch` statements also allow the `Exception` to be saved in a variable, e.g. `catch y`.

The `catch` clause is not strictly necessary; when omitted, the default return value is
`nothing`. The power of the `try/catch` construct lies in the ability to unwind a deeply
nested computation immediately to a much higher level in the stack of calling functions.
"""
kw"try", kw"catch"

"""
`finally` provides a way to run some code when a given block of code exits, regardless
of how it exits. For example, here is how we can guarantee that an opened file is
closed:

    f = open("file")
    try
        operate_on_file(f)
    finally
        close(f)
    end

When control leaves the `try` block (for example due to a `return`, or just finishing
normally), `close(f)` will be executed. If the `try` block exits due to an exception,
the exception will continue propagating. A `catch` block may be combined with `try` and
`finally` as well. In this case the `finally` block will run after `catch` has handled
the error.
"""
kw"finally"

"""
`break` breaks out of a loop immediately. For example

    i = 0
    while true
        i += 1
        i > 10 && break
        println(i)
    end

prints the numbers 1 to 10.
"""
kw"break"

"""
`continue` skips the rest of the current loop, then carries on looping. For example

    for i = 1:10
        iseven(i) && continue
        println(i)
    end

prints the numbers 1, 3, 5..., skipping the even numbers.
"""
kw"continue"

"""
The `do` keyword creates an anonymous function. For example

    map(1:10) do x
        2x
    end

is equivalent to `map(x->2x, 1:10)`.

Use multiple arguments like so:

    map(1:10, 11:20) do x, y
        x + y
    end
"""
kw"do"

"""
The "splat" operator, `...`, represents a sequence of arguments. For example

    add(xs...) = reduce(+, xs)

can take any number of arguments:

    add(1, 2, 3, 4, 5)

`...` can also be used to apply a function to a sequence of arguments like so:

    add([1, 2, 3]...) # 6
    add(7, 1:100..., 1000:1100...) # 111107
"""
kw"..."

"""
`;` has a similar role in Julia as in many C-like languages, and is used to delimit the
end of the previous statement. `;` is not necessary after new lines, but can be used to
separate statements on a single line or to join statements into a single expression:

    function foo()
        println("Hello, "); println("World!")
        return true
    end

    foo() = (println("Hello, World!"); true)

`;` is also used to suppress output in the REPL and similar interfaces.
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
    ccall(function_pointer, returntype, (argtype1, ...), argvalue1, ...)

Call a function in a C-exported shared library, specified by the tuple `(function_name, library)`,
where each component is either a string or symbol. Alternatively, `ccall` may
also be used to call a function pointer `function_pointer`, such as one returned by `dlsym`.

Note that the argument type tuple must be a literal tuple, and not a tuple-valued
variable or expression.

Each `argvalue` to the `ccall` will be converted to the corresponding
`argtype`, by automatic insertion of calls to `unsafe_convert(argtype,
cconvert(argtype, argvalue))`. (See also the documentation for each of these
functions for further details.) In most cases, this simply results in a call to
`convert(argtype, argvalue)`.
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
cconvert(ArgumentType, ArgumentValue))`. (see also the documentation for each of these
functions for further details). In most cases, this simply results in a call to
`convert(ArgumentType, ArgumentValue)`.

See `test/llvmcall.jl` for usage examples.
"""
Core.Intrinsics.llvmcall

"""
`begin...end` denotes a block of code.

    begin
        println("Hello, ")
        println("World!")
    end

Usually `begin` will not be necessary, since keywords such as `function` and `let`
implicitly begin blocks of code. See also `;`.
"""
kw"begin"

"""
The most commonly used kind of type in Julia is a struct, specified as a name and a
set of fields.

    struct Point
        x
        y
    end

Fields can have type restrictions, which may be parameterized:

    struct Point{X}
        x::X
        y::Float64
    end

A struct can also declare an abstract super type via `<:` syntax:

    struct Point <: AbstractPoint
        ...

Structs are immutable by default; an instance of one of these types cannot
be modified after construction. Use `mutable struct` instead to declare a
type whose instances can be modified.

See the manual for more details, such as how to define constructors.
"""
kw"struct"

"""
`mutable struct` is similar to  `struct`, but additionally allows the fields of the type
to be set after construction. See `struct` and the manual for more information.
"""
kw"mutable struct"

"""
The `where` keyword creates a type that is an iterated union of other types, over all
values of some variable. For example `Vector{T} where T<:Real` includes all `Vector`s
where the element type is some kind of `Real` number.

The variable bound defaults to `Any` if it is omitted:

    Vector{T} where T    # short for `where T<:Any`

Variables can also have lower bounds:

    Vector{T} where T>:Int
    Vector{T} where Int<:T<:Real

There is also a concise syntax for nested `where` expressions. For example, this:

    Pair{T, S} where S<:Array{T} where T<:Number

can be shortened to:

    Pair{T, S} where {T<:Number, S<:Array{T}}

This form is often found on method signatures.

Note that in this form, the variables are listed outermost-first. This matches the
order in which variables are substituted when a type is "applied" to parameter values
using the syntax `T{p1, p2, ...}`.
"""
kw"where"

"""
    ans

A variable referring to the last computed value, automatically set at the interactive prompt.
"""
kw"ans"

"""
    DevNull

Used in a stream redirect to discard all data written to it. Essentially equivalent to
/dev/null on Unix or NUL on Windows. Usage:

```julia
run(pipeline(`cat test.txt`, DevNull))
```
"""
DevNull

# doc strings for code in boot.jl and built-ins

"""
    nothing

The singleton instance of type `Void`, used by convention when there is no value to return
(as in a C `void` function). Can be converted to an empty `Nullable` value.
"""
nothing

"""
    Core.TypeofBottom

The singleton type containing only the value `Union{}`.
"""
Core.TypeofBottom

"""
    Function

Abstract type of all functions.

```jldoctest
julia> isa(+, Function)
true

julia> typeof(sin)
Base.#sin

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
"""
ErrorException

"""
    UndefRefError()

The item or field is not defined for the given object.
"""
UndefRefError

"""
    Float32(x [, mode::RoundingMode])

Create a Float32 from `x`. If `x` is not exactly representable then `mode` determines how
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

Create a Float64 from `x`. If `x` is not exactly representable then `mode` determines how
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
```jldoctest
julia> A = ones(7);

julia> A[8]
ERROR: BoundsError: attempt to access 7-element Array{Float64,1} at index [8]
Stacktrace:
 [1] getindex(::Array{Float64,1}, ::Int64) at ./array.jl:586

julia> B = ones(2, 3);

julia> B[2, 4]
ERROR: BoundsError: attempt to access 2×3 Array{Float64,2} at index [2, 4]
Stacktrace:
 [1] getindex(::Array{Float64,2}, ::Int64, ::Int64) at ./array.jl:587

julia> B[9]
ERROR: BoundsError: attempt to access 2×3 Array{Float64,2} at index [9]
Stacktrace:
 [1] getindex(::Array{Float64,2}, ::Int64) at ./array.jl:586
```
"""
BoundsError

"""
    InexactError(name::Symbol, T, val)

Cannot exactly convert `val` to type `T` in a method of function `name`.

# Examples
```jldoctest
julia> convert(Float64, 1+2im)
ERROR: InexactError: convert(Float64, 1 + 2im)
Stacktrace:
 [1] convert(::Type{Float64}, ::Complex{Int64}) at ./complex.jl:37
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
 [1] throw_complex_domainerror(::Symbol, ::Float64) at ./math.jl:31
 [2] sqrt at ./math.jl:462 [inlined]
 [3] sqrt(::Int64) at ./math.jl:472
```
"""
DomainError

"""
    Task(func)

Create a `Task` (i.e. coroutine) to execute the given function (which must be
callable with no arguments). The task exits when this function returns.

# Examples
```jldoctest
julia> a() = det(rand(1000, 1000));

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
"""
nfields

"""
    UndefVarError(var::Symbol)

A symbol in the current scope is not defined.
"""
UndefVarError

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
"""
InterruptException

"""
    applicable(f, args...) -> Bool

Determine whether the given generic function has a method applicable to the given arguments.

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

# Examples
```jldoctest
julia> f(x::Real) = x^2;

julia> f(x::Integer) = 1 + invoke(f, Tuple{Real}, x);

julia> f(2)
5
```
"""
invoke

"""
    isa(x, type) -> Bool

Determine whether `x` is of the given `type`. Can also be used as an infix operator, e.g.
`x isa type`.
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
 [1] div(::Int64, ::Int64) at ./int.jl:183
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

Boolean type.
"""
Bool

for bit in (16, 32, 64)
    @eval begin
        """
            Float$($bit) <: AbstractFloat

        $($bit)-bit floating point number type.
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
    Symbol(x...) -> Symbol

Create a `Symbol` by concatenating the string representations of the arguments together.
"""
Symbol

"""
    tuple(xs...)

Construct a tuple of the given objects.

# Examples
```jldoctest
julia> tuple(1, 'a', pi)
(1, 'a', π = 3.1415926535897...)
```
"""
tuple

"""
    getfield(value, name::Symbol)

Extract a named field from a `value` of composite type. The syntax `a.b` calls
`getfield(a, :b)`.

# Examples
```jldoctest
julia> a = 1//2
1//2

julia> getfield(a, :num)
1
```
"""
getfield

"""
    setfield!(value, name::Symbol, x)

Assign `x` to a named field in `value` of composite type. The syntax `a.b = c` calls
`setfield!(a, :b, c)`.
"""
setfield!

"""
    typeof(x)

Get the concrete type of `x`.
"""
typeof

"""
    isdefined(m::Module, s::Symbol)
    isdefined(object, s::Symbol)
    isdefined(object, index::Int)

Tests whether an assignable location is defined. The arguments can be a module and a symbol
or a composite object and field name (as a symbol) or index.
"""
isdefined


"""
    Vector{T}(n)

Construct an uninitialized [`Vector{T}`](@ref) of length `n`.

# Examples
```julia-repl
julia> Vector{Float64}(3)
3-element Array{Float64,1}:
 6.90966e-310
 6.90966e-310
 6.90966e-310
```
"""
Vector{T}(n)

"""
    Matrix{T}(m, n)

Construct an uninitialized [`Matrix{T}`](@ref) of size `m`×`n`.

# Examples
```julia-repl
julia> Matrix{Float64}(2, 3)
2×3 Array{Float64,2}:
 6.93517e-310  6.93517e-310  6.93517e-310
 6.93517e-310  6.93517e-310  1.29396e-320
```
"""
Matrix{T}(m, n)

"""
    Array{T}(dims)
    Array{T,N}(dims)

Construct an uninitialized `N`-dimensional [`Array`](@ref)
containing elements of type `T`. `N` can either be supplied explicitly,
as in `Array{T,N}(dims)`, or be determined by the length or number of `dims`.
`dims` may be a tuple or a series of integer arguments corresponding to the lengths
in each dimension. If the rank `N` is supplied explicitly, then it must
match the length or number of `dims`.

# Examples
```julia-repl
julia> A = Array{Float64,2}(2, 3) # N given explicitly
2×3 Array{Float64,2}:
 6.90198e-310  6.90198e-310  6.90198e-310
 6.90198e-310  6.90198e-310  0.0

julia> B = Array{Float64}(2) # N determined by the input
2-element Array{Float64,1}:
 1.87103e-320
 0.0
```
"""
Array{T,N}(dims)

"""
    +(x, y...)

Addition operator. `x+y+z+...` calls this function with all arguments, i.e. `+(x, y, z, ...)`.
"""
(+)(x, y...)

"""
    -(x)

Unary minus operator.
"""
-(x)

"""
    -(x, y)

Subtraction operator.
"""
-(x, y)

"""
    *(x, y...)

Multiplication operator. `x*y*z*...` calls this function with all arguments, i.e. `*(x, y, z, ...)`.
"""
(*)(x, y...)

"""
    /(x, y)

Right division operator: multiplication of `x` by the inverse of `y` on the right. Gives
floating-point results for integer arguments.
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
"""
AssertionError

"""
    LoadError(file::AbstractString, line::Int, error)

An error occurred while `include`ing, `require`ing, or `using` a file. The error specifics
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
is [`Core.TypeofBottom`](@ref).

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
Union{AbstractString, Int64}

julia> 1 :: IntOrString
1

julia> "Hello!" :: IntOrString
"Hello!"

julia> 1.0 :: IntOrString
ERROR: TypeError: typeassert: expected Union{AbstractString, Int64}, got Float64
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
ERROR: TypeError: typeassert: expected AbstractFloat, got Int64

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
kw"Tuple"

end
