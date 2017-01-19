# This file is a part of Julia. License is MIT: http://julialang.org/license

module BaseDocs

immutable Keyword
    name :: Symbol
end
macro kw_str(text) Keyword(Symbol(text)) end

"Hello, Human."
kw"hello", kw"hi"

"""
**Welcome to Julia $(string(VERSION)).** The full manual is available at

    http://docs.julialang.org/

as well many great tutorials and learning resources:

    http://julialang.org/learning/

For help on a specific function or macro, type `?` followed
by its name, e.g. `?fft`, or `?@time`, and press enter.
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
`abstract` declares a type that cannot be instantiated, and serves only as a node in the
type graph, thereby describing sets of related concrete types: those concrete types
which are their descendants. Abstract types form the conceptual hierarchy which makes
Julia’s type system more than just a collection of object implementations. For example:

    abstract Number
    abstract Real <: Number

`abstract Number` has no supertype, whereas `abstract Real` is an abstract subtype of `Number`.
"""
kw"abstract"

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
`bitstype` declares a concrete type whose data consists of plain old bits. Classic
examples of bits types are integers and floating-point values. Some example built-in
bits type declarations:

    bitstype 32 Char
    bitstype 8  Bool <: Integer

The first parameter indicates how many bits of storage the type requires. Currently,
only sizes that are multiples of 8 bits are supported. The second parameter gives the
name of the type.  The `Bool` declaration shows how a bits type can be optionally
declared to be a subtype of some supertype.
"""
kw"bitstype"

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
    ccall((symbol, library) or function_pointer, ReturnType, (ArgumentType1, ...), ArgumentValue1, ...)

Call function in C-exported shared library, specified by `(function name, library)`
tuple, where each component is a string or symbol.

Note that the argument type tuple must be a literal tuple, and not a tuple-valued
variable or expression. Alternatively, `ccall` may also be used to call a function
pointer, such as one returned by `dlsym`.

Each `ArgumentValue` to the `ccall` will be converted to the corresponding
`ArgumentType`, by automatic insertion of calls to `unsafe_convert(ArgumentType,
cconvert(ArgumentType, ArgumentValue))`. (See also the documentation for each of these
functions for further details.) In most cases, this simply results in a call to
`convert(ArgumentType, ArgumentValue)`.
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
At their most basic, Julia types are specified as a name and a set of fields.

    type Point
        x
        y
    end

Fields can have type restrictions, which may be parametrised:

    type Point{X}
        x::X
        y::Float64
    end

Type can also declare an abstract super type via `<:` syntax:

    type Point <: AbstractPoint
        ...

See the manual for more details, such as information on inner constructors.
"""
kw"type"

"""
Introduce a new name for an already expressible type. For example, in `base/boot.jl`,
`UInt` is type aliased to either `UInt64` or `UInt32` as appropriate for the size of
pointers on the system:

    if Int === Int64
        typealias UInt UInt64
    else
        typealias UInt UInt32
    end

For parametric types, `typealias` can be convenient for providing names in cases where
some parameter choices are fixed.  In `base` for example:

    typealias Vector{T} Array{T,1}
"""
kw"typealias"

"""
`immutable` acts in the same way as `type`, but declares that the fields of the type may
not be set after construction. See `type` and the manual for more information.
"""
kw"immutable"

"""
    @__LINE__ -> Int

`@__LINE__` expands to the line number of the call-site.
"""
kw"@__LINE__"

"""
    ans

A variable referring to the last computed value, automatically set at the interactive prompt.
"""
kw"ans"

"""
    nothing

The singleton instance of type `Void`, used by convention when there is no value to return
(as in a C `void` function). Can be converted to an empty `Nullable` value.
"""
nothing

"""
    ANY

Equivalent to `Any` for dispatch purposes, but signals the compiler to skip code
generation specialization for that field.
"""
ANY

"""
    DevNull

Used in a stream redirect to discard all data written to it. Essentially equivalent to
/dev/null on Unix or NUL on Windows. Usage:

```julia
run(pipeline(`cat test.txt`, DevNull))
```
"""
DevNull

end
