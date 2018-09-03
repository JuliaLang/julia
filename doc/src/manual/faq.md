# Frequently Asked Questions

## Sessions and the REPL

### How do I delete an object in memory?

Julia does not have an analog of MATLAB's `clear` function; once a name is defined in a Julia
session (technically, in module `Main`), it is always present.

If memory usage is your concern, you can always replace objects with ones that consume less memory.
 For example, if `A` is a gigabyte-sized array that you no longer need, you can free the memory
with `A = nothing`.  The memory will be released the next time the garbage collector runs; you can force
this to happen with [`gc()`](@ref Base.GC.gc). Moreover, an attempt to use `A` will likely result in an error, because most methods are not defined on type `Nothing`.

### How can I modify the declaration of a type in my session?

Perhaps you've defined a type and then realize you need to add a new field.  If you try this at
the REPL, you get the error:

```
ERROR: invalid redefinition of constant MyType
```

Types in module `Main` cannot be redefined.

While this can be inconvenient when you are developing new code, there's an excellent workaround.
 Modules can be replaced by redefining them, and so if you wrap all your new code inside a module
you can redefine types and constants.  You can't import the type names into `Main` and then expect
to be able to redefine them there, but you can use the module name to resolve the scope.  In other
words, while developing you might use a workflow something like this:

```julia
include("mynewcode.jl")              # this defines a module MyModule
obj1 = MyModule.ObjConstructor(a, b)
obj2 = MyModule.somefunction(obj1)
# Got an error. Change something in "mynewcode.jl"
include("mynewcode.jl")              # reload the module
obj1 = MyModule.ObjConstructor(a, b) # old objects are no longer valid, must reconstruct
obj2 = MyModule.somefunction(obj1)   # this time it worked!
obj3 = MyModule.someotherfunction(obj2, c)
...
```

### How do I check if the current file is being run as the main script?

When a file is run as the main script using `julia file.jl` one might want to activate extra
functionality like command line argument handling. A way to determine that a file is run in
this fashion is to check if `abspath(PROGRAM_FILE) == @__FILE__` is `true`.

## Functions

### I passed an argument `x` to a function, modified it inside that function, but on the outside, the variable `x` is still unchanged. Why?

Suppose you call a function like this:

```jldoctest
julia> x = 10
10

julia> function change_value!(y)
           y = 17
       end
change_value! (generic function with 1 method)

julia> change_value!(x)
17

julia> x # x is unchanged!
10
```

In Julia, the binding of a variable `x` cannot be changed by passing `x` as an argument to a function.
When calling `change_value!(x)` in the above example, `y` is a newly created variable, bound initially
to the value of `x`, i.e. `10`; then `y` is rebound to the constant `17`, while the variable
`x` of the outer scope is left untouched.

But here is a thing you should pay attention to: suppose `x` is bound to an object of type `Array`
(or any other *mutable* type). From within the function, you cannot "unbind" `x` from this Array,
but you can change its content. For example:

```jldoctest
julia> x = [1,2,3]
3-element Array{Int64,1}:
 1
 2
 3

julia> function change_array!(A)
           A[1] = 5
       end
change_array! (generic function with 1 method)

julia> change_array!(x)
5

julia> x
3-element Array{Int64,1}:
 5
 2
 3
```

Here we created a function `change_array!`, that assigns `5` to the first element of the passed
array (bound to `x` at the call site, and bound to `A` within the function). Notice that, after
the function call, `x` is still bound to the same array, but the content of that array changed:
the variables `A` and `x` were distinct bindings referring to the same mutable `Array` object.

### Can I use `using` or `import` inside a function?

No, you are not allowed to have a `using` or `import` statement inside a function.  If you want
to import a module but only use its symbols inside a specific function or set of functions, you
have two options:

1. Use `import`:

   ```julia
   import Foo
   function bar(...)
       # ... refer to Foo symbols via Foo.baz ...
   end
   ```

   This loads the module `Foo` and defines a variable `Foo` that refers to the module, but does not
   import any of the other symbols from the module into the current namespace.  You refer to the
   `Foo` symbols by their qualified names `Foo.bar` etc.
2. Wrap your function in a module:

   ```julia
   module Bar
   export bar
   using Foo
   function bar(...)
       # ... refer to Foo.baz as simply baz ....
   end
   end
   using Bar
   ```

   This imports all the symbols from `Foo`, but only inside the module `Bar`.

### What does the `...` operator do?

### The two uses of the `...` operator: slurping and splatting

Many newcomers to Julia find the use of `...` operator confusing. Part of what makes the `...`
operator confusing is that it means two different things depending on context.

### `...` combines many arguments into one argument in function definitions

In the context of function definitions, the `...` operator is used to combine many different arguments
into a single argument. This use of `...` for combining many different arguments into a single
argument is called slurping:

```jldoctest
julia> function printargs(args...)
           println(typeof(args))
           for (i, arg) in enumerate(args)
               println("Arg #$i = $arg")
           end
       end
printargs (generic function with 1 method)

julia> printargs(1, 2, 3)
Tuple{Int64,Int64,Int64}
Arg #1 = 1
Arg #2 = 2
Arg #3 = 3
```

If Julia were a language that made more liberal use of ASCII characters, the slurping operator
might have been written as `<-...` instead of `...`.

### `...` splits one argument into many different arguments in function calls

In contrast to the use of the `...` operator to denote slurping many different arguments into
one argument when defining a function, the `...` operator is also used to cause a single function
argument to be split apart into many different arguments when used in the context of a function
call. This use of `...` is called splatting:

```jldoctest
julia> function threeargs(a, b, c)
           println("a = $a::$(typeof(a))")
           println("b = $b::$(typeof(b))")
           println("c = $c::$(typeof(c))")
       end
threeargs (generic function with 1 method)

julia> x = [1, 2, 3]
3-element Array{Int64,1}:
 1
 2
 3

julia> threeargs(x...)
a = 1::Int64
b = 2::Int64
c = 3::Int64
```

If Julia were a language that made more liberal use of ASCII characters, the splatting operator
might have been written as `...->` instead of `...`.

### What is the return value of an assignment?

The operator `=` always returns the right-hand side, therefore:

```jldoctest
julia> function threeint()
           x::Int = 3.0
           x # returns variable x
       end
threeint (generic function with 1 method)

julia> function threefloat()
           x::Int = 3.0 # returns 3.0
       end
threefloat (generic function with 1 method)

julia> threeint()
3

julia> threefloat()
3.0
```

and similarly:

```jldoctest
julia> function threetup()
           x, y = [3, 3]
           x, y # returns a tuple
       end
threetup (generic function with 1 method)

julia> function threearr()
           x, y = [3, 3] # returns an array
       end
threearr (generic function with 1 method)

julia> threetup()
(3, 3)

julia> threearr()
2-element Array{Int64,1}:
 3
 3
```

## Types, type declarations, and constructors

### [What does "type-stable" mean?](@id man-type-stability)

It means that the type of the output is predictable from the types of the inputs.  In particular,
it means that the type of the output cannot vary depending on the *values* of the inputs. The
following code is *not* type-stable:

```jldoctest
julia> function unstable(flag::Bool)
           if flag
               return 1
           else
               return 1.0
           end
       end
unstable (generic function with 1 method)
```

It returns either an `Int` or a [`Float64`](@ref) depending on the value of its argument.
Since Julia can't predict the return type of this function at compile-time, any computation
that uses it will have to guard against both types possibly occurring, making generation of
fast machine code difficult.

### [Why does Julia give a `DomainError` for certain seemingly-sensible operations?](@id faq-domain-errors)

Certain operations make mathematical sense but result in errors:

```jldoctest
julia> sqrt(-2.0)
ERROR: DomainError with -2.0:
sqrt will only return a complex result if called with a complex argument. Try sqrt(Complex(x)).
Stacktrace:
[...]
```

This behavior is an inconvenient consequence of the requirement for type-stability.  In the case
of [`sqrt`](@ref), most users want `sqrt(2.0)` to give a real number, and would be unhappy if
it produced the complex number `1.4142135623730951 + 0.0im`.  One could write the [`sqrt`](@ref)
function to switch to a complex-valued output only when passed a negative number (which is what
[`sqrt`](@ref) does in some other languages), but then the result would not be [type-stable](@ref man-type-stability)
and the [`sqrt`](@ref) function would have poor performance.

In these and other cases, you can get the result you want by choosing an *input type* that conveys
your willingness to accept an *output type* in which the result can be represented:

```jldoctest
julia> sqrt(-2.0+0im)
0.0 + 1.4142135623730951im
```

### Why does Julia use native machine integer arithmetic?

Julia uses machine arithmetic for integer computations. This means that the range of `Int` values
is bounded and wraps around at either end so that adding, subtracting and multiplying integers
can overflow or underflow, leading to some results that can be unsettling at first:

```jldoctest
julia> typemax(Int)
9223372036854775807

julia> ans+1
-9223372036854775808

julia> -ans
-9223372036854775808

julia> 2*ans
0
```

Clearly, this is far from the way mathematical integers behave, and you might think it less than
ideal for a high-level programming language to expose this to the user. For numerical work where
efficiency and transparency are at a premium, however, the alternatives are worse.

One alternative to consider would be to check each integer operation for overflow and promote
results to bigger integer types such as [`Int128`](@ref) or [`BigInt`](@ref) in the case of overflow.
Unfortunately, this introduces major overhead on every integer operation (think incrementing a
loop counter) – it requires emitting code to perform run-time overflow checks after arithmetic
instructions and branches to handle potential overflows. Worse still, this would cause every computation
involving integers to be type-unstable. As we mentioned above, [type-stability is crucial](@ref man-type-stability)
for effective generation of efficient code. If you can't count on the results of integer operations
being integers, it's impossible to generate fast, simple code the way C and Fortran compilers
do.

A variation on this approach, which avoids the appearance of type instability is to merge the
`Int` and [`BigInt`](@ref) types into a single hybrid integer type, that internally changes representation
when a result no longer fits into the size of a machine integer. While this superficially avoids
type-instability at the level of Julia code, it just sweeps the problem under the rug by foisting
all of the same difficulties onto the C code implementing this hybrid integer type. This approach
*can* be made to work and can even be made quite fast in many cases, but has several drawbacks.
One problem is that the in-memory representation of integers and arrays of integers no longer
match the natural representation used by C, Fortran and other languages with native machine integers.
Thus, to interoperate with those languages, we would ultimately need to introduce native integer
types anyway. Any unbounded representation of integers cannot have a fixed number of bits, and
thus cannot be stored inline in an array with fixed-size slots – large integer values will always
require separate heap-allocated storage. And of course, no matter how clever a hybrid integer
implementation one uses, there are always performance traps – situations where performance degrades
unexpectedly. Complex representation, lack of interoperability with C and Fortran, the inability
to represent integer arrays without additional heap storage, and unpredictable performance characteristics
make even the cleverest hybrid integer implementations a poor choice for high-performance numerical
work.

An alternative to using hybrid integers or promoting to BigInts is to use saturating integer arithmetic,
where adding to the largest integer value leaves it unchanged and likewise for subtracting from
the smallest integer value. This is precisely what Matlab™ does:

```
>> int64(9223372036854775807)

ans =

  9223372036854775807

>> int64(9223372036854775807) + 1

ans =

  9223372036854775807

>> int64(-9223372036854775808)

ans =

 -9223372036854775808

>> int64(-9223372036854775808) - 1

ans =

 -9223372036854775808
```

At first blush, this seems reasonable enough since 9223372036854775807 is much closer to 9223372036854775808
than -9223372036854775808 is and integers are still represented with a fixed size in a natural
way that is compatible with C and Fortran. Saturated integer arithmetic, however, is deeply problematic.
The first and most obvious issue is that this is not the way machine integer arithmetic works,
so implementing saturated operations requires emitting instructions after each machine integer
operation to check for underflow or overflow and replace the result with [`typemin(Int)`](@ref)
or [`typemax(Int)`](@ref) as appropriate. This alone expands each integer operation from a single,
fast instruction into half a dozen instructions, probably including branches. Ouch. But it gets
worse – saturating integer arithmetic isn't associative. Consider this Matlab computation:

```
>> n = int64(2)^62
4611686018427387904

>> n + (n - 1)
9223372036854775807

>> (n + n) - 1
9223372036854775806
```

This makes it hard to write many basic integer algorithms since a lot of common techniques depend
on the fact that machine addition with overflow *is* associative. Consider finding the midpoint
between integer values `lo` and `hi` in Julia using the expression `(lo + hi) >>> 1`:

```jldoctest
julia> n = 2^62
4611686018427387904

julia> (n + 2n) >>> 1
6917529027641081856
```

See? No problem. That's the correct midpoint between 2^62 and 2^63, despite the fact that `n + 2n`
is -4611686018427387904. Now try it in Matlab:

```
>> (n + 2*n)/2

ans =

  4611686018427387904
```

Oops. Adding a `>>>` operator to Matlab wouldn't help, because saturation that occurs when adding
`n` and `2n` has already destroyed the information necessary to compute the correct midpoint.

Not only is lack of associativity unfortunate for programmers who cannot rely it for techniques
like this, but it also defeats almost anything compilers might want to do to optimize integer
arithmetic. For example, since Julia integers use normal machine integer arithmetic, LLVM is free
to aggressively optimize simple little functions like `f(k) = 5k-1`. The machine code for this
function is just this:

```julia-repl
julia> code_native(f, Tuple{Int})
  .text
Filename: none
  pushq %rbp
  movq  %rsp, %rbp
Source line: 1
  leaq  -1(%rdi,%rdi,4), %rax
  popq  %rbp
  retq
  nopl  (%rax,%rax)
```

The actual body of the function is a single `leaq` instruction, which computes the integer multiply
and add at once. This is even more beneficial when `f` gets inlined into another function:

```julia-repl
julia> function g(k, n)
           for i = 1:n
               k = f(k)
           end
           return k
       end
g (generic function with 1 methods)

julia> code_native(g, Tuple{Int,Int})
  .text
Filename: none
  pushq %rbp
  movq  %rsp, %rbp
Source line: 2
  testq %rsi, %rsi
  jle L26
  nopl  (%rax)
Source line: 3
L16:
  leaq  -1(%rdi,%rdi,4), %rdi
Source line: 2
  decq  %rsi
  jne L16
Source line: 5
L26:
  movq  %rdi, %rax
  popq  %rbp
  retq
  nop
```

Since the call to `f` gets inlined, the loop body ends up being just a single `leaq` instruction.
Next, consider what happens if we make the number of loop iterations fixed:

```julia-repl
julia> function g(k)
           for i = 1:10
               k = f(k)
           end
           return k
       end
g (generic function with 2 methods)

julia> code_native(g,(Int,))
  .text
Filename: none
  pushq %rbp
  movq  %rsp, %rbp
Source line: 3
  imulq $9765625, %rdi, %rax    # imm = 0x9502F9
  addq  $-2441406, %rax         # imm = 0xFFDABF42
Source line: 5
  popq  %rbp
  retq
  nopw  %cs:(%rax,%rax)
```

Because the compiler knows that integer addition and multiplication are associative and that multiplication
distributes over addition – neither of which is true of saturating arithmetic – it can optimize
the entire loop down to just a multiply and an add. Saturated arithmetic completely defeats this
kind of optimization since associativity and distributivity can fail at each loop iteration, causing
different outcomes depending on which iteration the failure occurs in. The compiler can unroll
the loop, but it cannot algebraically reduce multiple operations into fewer equivalent operations.

The most reasonable alternative to having integer arithmetic silently overflow is to do checked
arithmetic everywhere, raising errors when adds, subtracts, and multiplies overflow, producing
values that are not value-correct. In this [blog post](http://danluu.com/integer-overflow/), Dan
Luu analyzes this and finds that rather than the trivial cost that this approach should in theory
have, it ends up having a substantial cost due to compilers (LLVM and GCC) not gracefully optimizing
around the added overflow checks. If this improves in the future, we could consider defaulting
to checked integer arithmetic in Julia, but for now, we have to live with the possibility of overflow.

### What are the possible causes of an `UndefVarError` during remote execution?

As the error states, an immediate cause of an `UndefVarError` on a remote node is that a binding
by that name does not exist. Let us explore some of the possible causes.

```julia-repl
julia> module Foo
           foo() = remotecall_fetch(x->x, 2, "Hello")
       end

julia> Foo.foo()
ERROR: On worker 2:
UndefVarError: Foo not defined
Stacktrace:
[...]
```

The closure `x->x` carries a reference to `Foo`, and since `Foo` is unavailable on node 2,
an `UndefVarError` is thrown.

Globals under modules other than `Main` are not serialized by value to the remote node. Only a reference is sent.
Functions which create global bindings (except under `Main`) may cause an `UndefVarError` to be thrown later.

```julia-repl
julia> @everywhere module Foo
           function foo()
               global gvar = "Hello"
               remotecall_fetch(()->gvar, 2)
           end
       end

julia> Foo.foo()
ERROR: On worker 2:
UndefVarError: gvar not defined
Stacktrace:
[...]
```

In the above example, `@everywhere module Foo` defined `Foo` on all nodes. However the call to `Foo.foo()` created
a new global binding `gvar` on the local node, but this was not found on node 2 resulting in an `UndefVarError` error.

Note that this does not apply to globals created under module `Main`. Globals under module `Main` are serialized
and new bindings created under `Main` on the remote node.

```julia-repl
julia> gvar_self = "Node1"
"Node1"

julia> remotecall_fetch(()->gvar_self, 2)
"Node1"

julia> remotecall_fetch(varinfo, 2)
name          size summary
––––––––– –––––––– –––––––
Base               Module
Core               Module
Main               Module
gvar_self 13 bytes String
```

This does not apply to `function` or `struct` declarations. However, anonymous functions bound to global
variables are serialized as can be seen below.

```julia-repl
julia> bar() = 1
bar (generic function with 1 method)

julia> remotecall_fetch(bar, 2)
ERROR: On worker 2:
UndefVarError: #bar not defined
[...]

julia> anon_bar  = ()->1
(::#21) (generic function with 1 method)

julia> remotecall_fetch(anon_bar, 2)
1
```

## Packages and Modules

### What is the difference between "using" and "import"?

There is only one difference, and on the surface (syntax-wise) it may seem very minor. The difference
between `using` and `import` is that with `using` you need to say `function Foo.bar(..` to
extend module Foo's function bar with a new method, but with `import Foo.bar`,
you only need to say `function bar(...` and it automatically extends module Foo's function bar.

The reason this is important enough to have been given separate syntax is that you don't want
to accidentally extend a function that you didn't know existed, because that could easily cause
a bug. This is most likely to happen with a method that takes a common type like a string or integer,
because both you and the other module could define a method to handle such a common type. If you
use `import`, then you'll replace the other module's implementation of `bar(s::AbstractString)`
with your new implementation, which could easily do something completely different (and break
all/many future usages of the other functions in module Foo that depend on calling bar).

## Nothingness and missing values

### [How does "null", "nothingness" or "missingness" work in Julia?](@id faq-nothing)

Unlike many languages (for example, C and Java), Julia objects cannot be "null" by default.
When a reference (variable, object field, or array element) is uninitialized, accessing it
will immediately throw an error. This situation can be detected using the
[`isdefined`](@ref) or [`isassigned`](@ref Base.isassigned) functions.

Some functions are used only for their side effects, and do not need to return a value. In these
cases, the convention is to return the value `nothing`, which is just a singleton object of type
`Nothing`. This is an ordinary type with no fields; there is nothing special about it except for
this convention, and that the REPL does not print anything for it. Some language constructs that
would not otherwise have a value also yield `nothing`, for example `if false; end`.

For situations where a value `x` of type `T` exists only sometimes, the `Union{T, Nothing}`
type can be used for function arguments, object fields and array element types
as the equivalent of [`Nullable`, `Option` or `Maybe`](https://en.wikipedia.org/wiki/Nullable_type)
in other languages. If the value itself can be `nothing` (notably, when `T` is `Any`),
the `Union{Some{T}, Nothing}` type is more appropriate since `x == nothing` then indicates
the absence of a value, and `x == Some(nothing)` indicates the presence of a value equal
to `nothing`. The [`something`](@ref) function allows unwrapping `Some` objects and
using a default value instead of `nothing` arguments. Note that the compiler is able to
generate efficient code when working with `Union{T, Nothing}` arguments or fields.

To represent missing data in the statistical sense (`NA` in R or `NULL` in SQL), use the
[`missing`](@ref) object. See the [`Missing Values`](@ref missing) section for more details.

The empty tuple (`()`) is another form of nothingness. But, it should not really be thought of
as nothing but rather a tuple of zero values.

The empty (or "bottom") type, written as `Union{}` (an empty union type), is a type with
no values and no subtypes (except itself). You will generally not need to use this type.

## Memory

### Why does `x += y` allocate memory when `x` and `y` are arrays?

In Julia, `x += y` gets replaced during parsing by `x = x + y`. For arrays, this has the consequence
that, rather than storing the result in the same location in memory as `x`, it allocates a new
array to store the result.

While this behavior might surprise some, the choice is deliberate. The main reason is the presence
of immutable objects within Julia, which cannot change their value once created.  Indeed, a
number is an immutable object; the statements `x = 5; x += 1` do not modify the meaning of `5`,
they modify the value bound to `x`. For an immutable, the only way to change the value is to reassign
it.

To amplify a bit further, consider the following function:

```julia
function power_by_squaring(x, n::Int)
    ispow2(n) || error("This implementation only works for powers of 2")
    while n >= 2
        x *= x
        n >>= 1
    end
    x
end
```

After a call like `x = 5; y = power_by_squaring(x, 4)`, you would get the expected result: `x == 5 && y == 625`.
 However, now suppose that `*=`, when used with matrices, instead mutated the left hand side.
 There would be two problems:

  * For general square matrices, `A = A*B` cannot be implemented without temporary storage: `A[1,1]`
    gets computed and stored on the left hand side before you're done using it on the right hand side.
  * Suppose you were willing to allocate a temporary for the computation (which would eliminate most
    of the point of making `*=` work in-place); if you took advantage of the mutability of `x`, then
    this function would behave differently for mutable vs. immutable inputs. In particular, for immutable
    `x`, after the call you'd have (in general) `y != x`, but for mutable `x` you'd have `y == x`.

Because supporting generic programming is deemed more important than potential performance optimizations
that can be achieved by other means (e.g., using explicit loops), operators like `+=` and `*=`
work by rebinding new values.

## Asynchronous IO and concurrent synchronous writes

### Why do concurrent writes to the same stream result in inter-mixed output?

While the streaming I/O API is synchronous, the underlying implementation is fully asynchronous.

Consider the printed output from the following:

```jldoctest
julia> @sync for i in 1:3
           @async write(stdout, string(i), " Foo ", " Bar ")
       end
123 Foo  Foo  Foo  Bar  Bar  Bar
```

This is happening because, while the `write` call is synchronous, the writing of each argument
yields to other tasks while waiting for that part of the I/O to complete.

`print` and `println` "lock" the stream during a call. Consequently changing `write` to `println`
in the above example results in:

```jldoctest
julia> @sync for i in 1:3
           @async println(stdout, string(i), " Foo ", " Bar ")
       end
1 Foo  Bar
2 Foo  Bar
3 Foo  Bar
```

You can lock your writes with a `ReentrantLock` like this:

```jldoctest
julia> l = ReentrantLock()
ReentrantLock(nothing, Condition(Any[]), 0)

julia> @sync for i in 1:3
           @async begin
               lock(l)
               try
                   write(stdout, string(i), " Foo ", " Bar ")
               finally
                   unlock(l)
               end
           end
       end
1 Foo  Bar 2 Foo  Bar 3 Foo  Bar
```

## Arrays

### What are the differences between zero-dimensional arrays and scalars?

Zero-dimensional arrays are arrays of the form `Array{T,0}`. They behave similar
to scalars, but there are important differences. They deserve a special mention
because they are a special case which makes logical sense given the generic
definition of arrays, but might be a bit unintuitive at first. The following
line defines a zero-dimensional array:

```
julia> A = zeros()
0-dimensional Array{Float64,0}:
0.0
```

In this example, `A` is a mutable container that contains one element, which can
be set by `A[] = 1.0` and retrieved with `A[]`. All zero-dimensional arrays have
the same size (`size(A) == ()`), and length (`length(A) == 1`). In particular,
zero-dimensional arrays are not empty. If you find this unintuitive, here are
some ideas that might help to understand Julia's definition.

* Zero-dimensional arrays are the "point" to vector's "line" and matrix's
  "plane". Just as a line has no area (but still represents a set of things), a
  point has no length or any dimensions at all (but still represents a thing).
* We define `prod(())` to be 1, and the total number of elements in an array is
  the product of the size. The size of a zero-dimensional array is `()`, and
  therefore its length is `1`.
* Zero-dimensional arrays don't natively have any dimensions into which you
  index -- they’re just `A[]`. We can apply the same "trailing one" rule for them
  as for all other array dimensionalities, so you can indeed index them as
  `A[1]`, `A[1,1]`, etc.

It is also important to understand the differences to ordinary scalars. Scalars
are not mutable containers (even though they are iterable and define things
like `length`, `getindex`, *e.g.* `1[] == 1`). In particular, if `x = 0.0` is
defined as a scalar, it is an error to attempt to change its value via
`x[] = 1.0`. A scalar `x` can be converted into a zero-dimensional array
containing it via `fill(x)`, and conversely, a zero-dimensional array `a` can
be converted to the contained scalar via `a[]`. Another difference is that
a scalar can participate in linear algebra operations such as `2 * rand(2,2)`,
but the analogous operation with a zero-dimensional array
`fill(2) * rand(2,2)` is an error.

## Julia Releases

### Do I want to use a release, beta, or nightly version of Julia?

You may prefer the release version of Julia if you are looking for a stable code base. Releases
generally occur every 6 months, giving you a stable platform for writing code.

You may prefer the beta version of Julia if you don't mind being slightly behind the latest bugfixes
and changes, but find the slightly faster rate of changes more appealing. Additionally, these
binaries are tested before they are published to ensure they are fully functional.

You may prefer the nightly version of Julia if you want to take advantage of the latest updates
to the language, and don't mind if the version available today occasionally doesn't actually work.

Finally, you may also consider building Julia from source for yourself. This option is mainly
for those individuals who are comfortable at the command line, or interested in learning. If this
describes you, you may also be interested in reading our [guidelines for contributing](https://github.com/JuliaLang/julia/blob/master/CONTRIBUTING.md).

Links to each of these download types can be found on the download page at [https://julialang.org/downloads/](https://julialang.org/downloads/).
Note that not all versions of Julia are available for all platforms.

### When are deprecated functions removed?

Deprecated functions are removed after the subsequent release. For example, functions marked as
deprecated in the 0.1 release will not be available starting with the 0.2 release.
