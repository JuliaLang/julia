.. _man-faq:

.. currentmodule:: Base

****************************
 Frequently Asked Questions
****************************

Sessions and the REPL
---------------------

How do I delete an object in memory?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Julia does not have an analog of MATLAB's ``clear`` function; once a
name is defined in a Julia session (technically, in module ``Main``),
it is always present.

If memory usage is your concern, you can always replace objects with
ones that consume less memory.  For example, if ``A`` is a
gigabyte-sized array that you no longer need, you can free the memory
with ``A = 0``.  The memory will be released the next time the garbage
collector runs; you can force this to happen with :func:`gc`.

How can I modify the declaration of a type/immutable in my session?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Perhaps you've defined a type and then realize you need to add a
new field.  If you try this at the REPL, you get the error::

    ERROR: invalid redefinition of constant MyType

Types in module ``Main`` cannot be redefined.

While this can be inconvenient when you are developing new code,
there's an excellent workaround.  Modules can be replaced by
redefining them, and so if you wrap all your new code inside a module
you can redefine types and constants.  You can't import the type names
into ``Main`` and then expect to be able to redefine them there, but
you can use the module name to resolve the scope.  In other words,
while developing you might use a workflow something like this::

    include("mynewcode.jl")              # this defines a module MyModule
    obj1 = MyModule.ObjConstructor(a, b)
    obj2 = MyModule.somefunction(obj1)
    # Got an error. Change something in "mynewcode.jl"
    include("mynewcode.jl")              # reload the module
    obj1 = MyModule.ObjConstructor(a, b) # old objects are no longer valid, must reconstruct
    obj2 = MyModule.somefunction(obj1)   # this time it worked!
    obj3 = MyModule.someotherfunction(obj2, c)
    ...

Functions
---------

I passed an argument ``x`` to a function, modified it inside that function, but on the outside, the variable ``x`` is still unchanged. Why?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Suppose you call a function like this::

	julia> x = 10
	julia> function change_value!(y) # Create a new function
	           y = 17
	       end
	julia> change_value!(x)
	julia> x # x is unchanged!
	10

In Julia, any function (including ``change_value!()``) can't change the binding of a local variable. If ``x`` (in the calling scope) is bound to a immutable object (like a real number), you can't modify the object; likewise, if x is bound in the calling scope to a Dict, you can't change it to be bound to an ASCIIString.

But here is a thing you should pay attention to: suppose ``x`` is bound to an Array (or any other mutable type). You cannot "unbind" ``x`` from this Array. But, since an Array is a *mutable* type, you can change its content. For example::

	julia> x = [1,2,3]
	3-element Array{Int64,1}:
	1
	2
	3

	julia> function change_array!(A) # Create a new function
	           A[1] = 5
	       end
	julia> change_array!(x)
	julia> x
	3-element Array{Int64,1}:
	5
	2
	3

Here we created a function ``change_array!()``, that assigns ``5`` to the first element of the Array. We passed ``x`` (which was previously bound to an Array) to the function. Notice that, after the function call, ``x`` is still bound to the same Array, but the content of that Array changed.


Can I use ``using`` or ``import`` inside a function?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

No, you are not allowed to have a ``using`` or ``import`` statement inside
a function.  If you want to import a module but only use its symbols
inside a specific function or set of functions, you have two options:

1.  Use ``import``::

        import Foo
        function bar(...)
            ... refer to Foo symbols via Foo.baz ...
        end


    This loads the module ``Foo`` and defines a variable ``Foo`` that refers
    to the module, but does not import any of the other symbols from the
    module into the current namespace.  You refer to the ``Foo`` symbols by
    their qualified names ``Foo.bar`` etc.


2.  Wrap your function in a module::

        module Bar
        export bar
        using Foo
        function bar(...)
            ... refer to Foo.baz as simply baz ....
        end
        end
        using Bar

    This imports all the symbols from ``Foo``, but only inside the module ``Bar``.

.. _man-slurping-splatting:

What does the ``...`` operator do?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The two uses of the `...` operator: slurping and splatting
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Many newcomers to Julia find the use of ``...`` operator confusing. Part of
what makes the ``...`` operator confusing is that it means two different things
depending on context.

``...`` combines many arguments into one argument in function definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the context of function definitions, the ``...`` operator is used to combine
many different arguments into a single argument. This use of ``...`` for
combining many different arguments into a single argument is called slurping::

    julia> function printargs(args...)
               @printf("%s\n", typeof(args))
               for (i, arg) in enumerate(args)
                   @printf("Arg %d = %s\n", i, arg)
               end
           end
    printargs (generic function with 1 method)

    julia> printargs(1, 2, 3)
    (Int64,Int64,Int64)
    Arg 1 = 1
    Arg 2 = 2
    Arg 3 = 3

If Julia were a language that made more liberal use of ASCII characters, the
slurping operator might have been written as ``<-...`` instead of ``...``.

``...`` splits one argument into many different arguments in function calls
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In contrast to the use of the ``...`` operator to denote slurping many
different arguments into one argument when defining a function, the ``...``
operator is also used to cause a single function argument to be split apart
into many different arguments when used in the context of a function call. This
use of ``...`` is called splatting::

    julia> function threeargs(a, b, c)
               @printf("a = %s::%s\n", a, typeof(a))
               @printf("b = %s::%s\n", b, typeof(b))
               @printf("c = %s::%s\n", c, typeof(c))
           end
    threeargs (generic function with 1 method)

    julia> vec = [1, 2, 3]
    3-element Array{Int64,1}:
     1
     2
     3

    julia> threeargs(vec...)
    a = 1::Int64
    b = 2::Int64
    c = 3::Int64

If Julia were a language that made more liberal use of ASCII characters,
the splatting operator might have been written as ``...->`` instead of ``...``.

Types, type declarations, and constructors
------------------------------------------

.. _man-type-stable:

What does "type-stable" mean?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It means that the type of the output is predictable from the types
of the inputs.  In particular, it means that the type of the output
cannot vary depending on the *values* of the inputs. The following
code is *not* type-stable::

    function unstable(flag::Bool)
        if flag
            return 1
        else
            return 1.0
        end
    end

It returns either an ``Int`` or a ``Float64`` depending on the value of its
argument. Since Julia can't predict the return type of this function at
compile-time, any computation that uses it will have to guard against both
types possibly occurring, making generation of fast machine code difficult.

.. _man-domain-error:

Why does Julia give a ``DomainError`` for certain seemingly-sensible operations?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Certain operations make mathematical sense but result in errors::

    julia> sqrt(-2.0)
    ERROR: DomainError
     in sqrt at math.jl:128

    julia> 2^-5
    ERROR: DomainError
     in power_by_squaring at intfuncs.jl:70
     in ^ at intfuncs.jl:84

This behavior is an inconvenient consequence of the requirement for
type-stability.  In the case of :func:`sqrt`, most users want
``sqrt(2.0)`` to give a real number, and would be unhappy if it
produced the complex number ``1.4142135623730951 + 0.0im``.  One could
write the :func:`sqrt` function to switch to a complex-valued output only
when passed a negative number (which is what :func:`sqrt` does in some
other languages), but then the result would not be `type-stable
<#man-type-stable>`_ and the :func:`sqrt` function would have poor
performance.

In these and other cases, you can get the result you want by choosing
an *input type* that conveys your willingness to accept an *output type* in
which the result can be represented::

    julia> sqrt(-2.0+0im)
    0.0 + 1.4142135623730951im

    julia> 2.0^-5
    0.03125


Why does Julia use native machine integer arithmetic?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Julia uses machine arithmetic for integer computations. This means that the range of ``Int`` values is bounded and wraps around at either end so that adding, subtracting and multiplying integers can overflow or underflow, leading to some results that can be unsettling at first::

    julia> typemax(Int)
    9223372036854775807

    julia> ans+1
    -9223372036854775808

    julia> -ans
    -9223372036854775808

    julia> 2*ans
    0

Clearly, this is far from the way mathematical integers behave, and you might
think it less than ideal for a high-level programming language to expose this
to the user. For numerical work where efficiency and transparency are at a
premium, however, the alternatives are worse.

One alternative to consider would be to check each integer operation for
overflow and promote results to bigger integer types such as ``Int128`` or
:class:`BigInt` in the case of overflow. Unfortunately, this introduces major
overhead on every integer operation (think incrementing a loop counter) – it
requires emitting code to perform run-time overflow checks after arithmetic
instructions and branches to handle potential overflows. Worse still, this
would cause every computation involving integers to be type-unstable. As we
mentioned above, `type-stability is crucial <#man-type-stable>`_ for effective
generation of efficient code. If you can't count on the results of integer
operations being integers, it's impossible to generate fast, simple code the
way C and Fortran compilers do.

A variation on this approach, which avoids the appearance of type instability is to merge the ``Int`` and :class:`BigInt` types into a single hybrid integer type, that internally changes representation when a result no longer fits into the size of a machine integer. While this superficially avoids type-instability at the level of Julia code, it just sweeps the problem under the rug by foisting all of the same difficulties onto the C code implementing this hybrid integer type. This approach *can* be made to work and can even be made quite fast in many cases, but has several drawbacks. One problem is that the in-memory representation of integers and arrays of integers no longer match the natural representation used by C, Fortran and other languages with native machine integers. Thus, to interoperate with those languages, we would ultimately need to introduce native integer types anyway. Any unbounded representation of integers cannot have a fixed number of bits, and thus cannot be stored inline in an array with fixed-size slots – large integer values will always require separate heap-allocated storage. And of course, no matter how clever a hybrid integer implementation one uses, there are always performance traps – situations where performance degrades unexpectedly. Complex representation, lack of interoperability with C and Fortran, the inability to represent integer arrays without additional heap storage, and unpredictable performance characteristics make even the cleverest hybrid integer implementations a poor choice for high-performance numerical work.

An alternative to using hybrid integers or promoting to BigInts is to use
saturating integer arithmetic, where adding to the largest integer value
leaves it unchanged and likewise for subtracting from the smallest integer
value. This is precisely what Matlab™ does::

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

At first blush, this seems reasonable enough since 9223372036854775807 is much closer to 9223372036854775808 than -9223372036854775808 is and integers are still represented with a fixed size in a natural way that is compatible with C and Fortran. Saturated integer arithmetic, however, is deeply problematic. The first and most obvious issue is that this is not the way machine integer arithmetic works, so implementing saturated operations requires emitting instructions after each machine integer operation to check for underflow or overflow and replace the result with :func:`typemin(Int) <typemin>` or :func:`typemax(Int) <typemax>` as appropriate. This alone expands each integer operation from a single, fast instruction into half a dozen instructions, probably including branches. Ouch. But it gets worse – saturating integer arithmetic isn't associative. Consider this Matlab computation::

    >> n = int64(2)^62
    4611686018427387904

    >> n + (n - 1)
    9223372036854775807

    >> (n + n) - 1
    9223372036854775806

This makes it hard to write many basic integer algorithms since a lot of
common techniques depend on the fact that machine addition with overflow *is*
associative. Consider finding the midpoint between integer values ``lo`` and
``hi`` in Julia using the expression ``(lo + hi) >>> 1``::

    julia> n = 2^62
    4611686018427387904

    julia> (n + 2n) >>> 1
    6917529027641081856

See? No problem. That's the correct midpoint between 2^62 and 2^63, despite
the fact that ``n + 2n`` is -4611686018427387904. Now try it in Matlab::

    >> (n + 2*n)/2

    ans =

      4611686018427387904

Oops. Adding a ``>>>`` operator to Matlab wouldn't help, because saturation
that occurs when adding ``n`` and ``2n`` has already destroyed the information
necessary to compute the correct midpoint.

Not only is lack of associativity unfortunate for programmers who cannot rely
it for techniques like this, but it also defeats almost anything compilers
might want to do to optimize integer arithmetic. For example, since Julia
integers use normal machine integer arithmetic, LLVM is free to aggressively
optimize simple little functions like ``f(k) = 5k-1``. The machine code for
this function is just this::

    julia> code_native(f,(Int,))
        .section    __TEXT,__text,regular,pure_instructions
    Filename: none
    Source line: 1
        push    RBP
        mov RBP, RSP
    Source line: 1
        lea RAX, QWORD PTR [RDI + 4*RDI - 1]
        pop RBP
        ret

The actual body of the function is a single ``lea`` instruction, which
computes the integer multiply and add at once. This is even more beneficial
when ``f`` gets inlined into another function::

    julia> function g(k,n)
             for i = 1:n
               k = f(k)
             end
             return k
           end
    g (generic function with 2 methods)

    julia> code_native(g,(Int,Int))
        .section    __TEXT,__text,regular,pure_instructions
    Filename: none
    Source line: 3
        push    RBP
        mov RBP, RSP
        test    RSI, RSI
        jle 22
        mov EAX, 1
    Source line: 3
        lea RDI, QWORD PTR [RDI + 4*RDI - 1]
        inc RAX
        cmp RAX, RSI
    Source line: 2
        jle -17
    Source line: 5
        mov RAX, RDI
        pop RBP
        ret

Since the call to ``f`` gets inlined, the loop body ends up being just a
single ``lea`` instruction. Next, consider what happens if we make the number
of loop iterations fixed::

    julia> function g(k)
             for i = 1:10
               k = f(k)
             end
             return k
           end
    g (generic function with 2 methods)

    julia> code_native(g,(Int,))
        .section    __TEXT,__text,regular,pure_instructions
    Filename: none
    Source line: 3
        push    RBP
        mov RBP, RSP
    Source line: 3
        imul    RAX, RDI, 9765625
        add RAX, -2441406
    Source line: 5
        pop RBP
        ret

Because the compiler knows that integer addition and multiplication are
associative and that multiplication distributes over addition – neither of
which is true of saturating arithmetic – it can optimize the entire loop down
to just a multiply and an add. Saturated arithmetic completely defeats this
kind of optimization since associativity and distributivity can fail at each
loop iteration, causing different outcomes depending on which iteration the
failure occurs in. The compiler can unroll the loop, but it cannot
algebraically reduce multiple operations into fewer equivalent operations.

The most reasonable alternative to having integer arithmetic silently overflow
is to do checked arithmetic everywhere, raising errors when adds, subtracts,
and multiplies overflow, producing values that are not value-correct. In this
`blog post <http://danluu.com/integer-overflow>`_, Dan Luu analyzes this and
finds that rather than the trivial cost that this approach should in theory
have, it ends up having a substantial cost due to compilers (LLVM and GCC)
not gracefully optimizing around the added overflow checks. If this improves
in the future, we could consider defaulting to checked integer arithmetic in
Julia, but for now, we have to live with the possibility of overflow.


.. _man-abstract-fields:

How do "abstract" or ambiguous fields in types interact with the compiler?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Types can be declared without specifying the types of their fields:

.. doctest::

    julia> type MyAmbiguousType
               a
           end

This allows ``a`` to be of any type. This can often be useful, but it
does have a downside: for objects of type ``MyAmbiguousType``, the
compiler will not be able to generate high-performance code.  The
reason is that the compiler uses the types of objects, not their
values, to determine how to build code. Unfortunately, very little can
be inferred about an object of type ``MyAmbiguousType``:

.. doctest::

    julia> b = MyAmbiguousType("Hello")
    MyAmbiguousType("Hello")

    julia> c = MyAmbiguousType(17)
    MyAmbiguousType(17)

    julia> typeof(b)
    MyAmbiguousType (constructor with 1 method)

    julia> typeof(c)
    MyAmbiguousType (constructor with 1 method)

``b`` and ``c`` have the same type, yet their underlying
representation of data in memory is very different. Even if you stored
just numeric values in field ``a``, the fact that the memory
representation of a ``UInt8`` differs from a ``Float64`` also means
that the CPU needs to handle them using two different kinds of
instructions.  Since the required information is not available in the
type, such decisions have to be made at run-time. This slows
performance.

You can do better by declaring the type of ``a``. Here, we are focused
on the case where ``a`` might be any one of several types, in which
case the natural solution is to use parameters. For example:

.. doctest::

    julia> type MyType{T<:FloatingPoint}
             a::T
           end

This is a better choice than

.. doctest::

    julia> type MyStillAmbiguousType
             a::FloatingPoint
           end

because the first version specifies the type of ``a`` from the type of
the wrapper object.  For example:

.. doctest::

    julia> m = MyType(3.2)
    MyType{Float64}(3.2)

    julia> t = MyStillAmbiguousType(3.2)
    MyStillAmbiguousType(3.2)

    julia> typeof(m)
    MyType{Float64} (constructor with 1 method)

    julia> typeof(t)
    MyStillAmbiguousType (constructor with 2 methods)

The type of field ``a`` can be readily determined from the type of
``m``, but not from the type of ``t``.  Indeed, in ``t`` it's possible
to change the type of field ``a``:

.. doctest::

    julia> typeof(t.a)
    Float64

    julia> t.a = 4.5f0
    4.5f0

    julia> typeof(t.a)
    Float32

In contrast, once ``m`` is constructed, the type of ``m.a`` cannot
change:

.. doctest::

    julia> m.a = 4.5f0
    4.5

    julia> typeof(m.a)
    Float64

The fact that the type of ``m.a`` is known from ``m``'s type---coupled
with the fact that its type cannot change mid-function---allows the
compiler to generate highly-optimized code for objects like ``m`` but
not for objects like ``t``.

Of course, all of this is true only if we construct ``m`` with a
concrete type.  We can break this by explicitly constructing it with
an abstract type:

.. doctest::

    julia> m = MyType{FloatingPoint}(3.2)
    MyType{FloatingPoint}(3.2)

    julia> typeof(m.a)
    Float64

    julia> m.a = 4.5f0
    4.5f0

    julia> typeof(m.a)
    Float32

For all practical purposes, such objects behave identically to those
of ``MyStillAmbiguousType``.

It's quite instructive to compare the sheer amount code generated for
a simple function
::

    func(m::MyType) = m.a+1

using
::

    code_llvm(func,(MyType{Float64},))
    code_llvm(func,(MyType{FloatingPoint},))
    code_llvm(func,(MyType,))

For reasons of length the results are not shown here, but you may wish
to try this yourself. Because the type is fully-specified in the first
case, the compiler doesn't need to generate any code to resolve the
type at run-time.  This results in shorter and faster code.


.. _man-abstract-container-type:

How should I declare "abstract container type" fields?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The same best practices that apply in the `previous section
<#man-abstract-fields>`_ also work for container types:

.. doctest::

    julia> type MySimpleContainer{A<:AbstractVector}
             a::A
           end

    julia> type MyAmbiguousContainer{T}
             a::AbstractVector{T}
           end

For example:

.. doctest::

    julia> c = MySimpleContainer(1:3);

    julia> typeof(c)
    MySimpleContainer{UnitRange{Int64}} (constructor with 1 method)

    julia> c = MySimpleContainer([1:3]);

    julia> typeof(c)
    MySimpleContainer{Array{Int64,1}} (constructor with 1 method)

    julia> b = MyAmbiguousContainer(1:3);

    julia> typeof(b)
    MyAmbiguousContainer{Int64} (constructor with 1 method)

    julia> b = MyAmbiguousContainer([1:3]);

    julia> typeof(b)
    MyAmbiguousContainer{Int64} (constructor with 1 method)

For ``MySimpleContainer``, the object is fully-specified by its type
and parameters, so the compiler can generate optimized functions. In
most instances, this will probably suffice.

While the compiler can now do its job perfectly well, there are cases
where *you* might wish that your code could do different things
depending on the *element type* of ``a``.  Usually the best way to
achieve this is to wrap your specific operation (here, ``foo``) in a
separate function::

    function sumfoo(c::MySimpleContainer)
        s = 0
	for x in c.a
	    s += foo(x)
	end
	s
    end

    foo(x::Integer) = x
    foo(x::FloatingPoint) = round(x)

This keeps things simple, while allowing the compiler to generate
optimized code in all cases.

However, there are cases where you may need to declare different
versions of the outer function for different element types of
``a``. You could do it like this::

    function myfun{T<:FloatingPoint}(c::MySimpleContainer{Vector{T}})
        ...
    end
    function myfun{T<:Integer}(c::MySimpleContainer{Vector{T}})
        ...
    end

This works fine for ``Vector{T}``, but we'd also have to write
explicit versions for ``UnitRange{T}`` or other abstract types. To
prevent such tedium, you can use two parameters in the declaration of
``MyContainer``::

    type MyContainer{T, A<:AbstractVector}
        a::A
    end
    MyContainer(v::AbstractVector) = MyContainer{eltype(v), typeof(v)}(v)

    julia> b = MyContainer(1.3:5);

    julia> typeof(b)
    MyContainer{Float64,UnitRange{Float64}}

Note the somewhat surprising fact that ``T`` doesn't appear in the
declaration of field ``a``, a point that we'll return to in a moment.
With this approach, one can write functions such as::

    function myfunc{T<:Integer, A<:AbstractArray}(c::MyContainer{T,A})
        return c.a[1]+1
    end
    # Note: because we can only define MyContainer for
    # A<:AbstractArray, and any unspecified parameters are arbitrary,
    # the previous could have been written more succinctly as
    #     function myfunc{T<:Integer}(c::MyContainer{T})

    function myfunc{T<:FloatingPoint}(c::MyContainer{T})
        return c.a[1]+2
    end

    function myfunc{T<:Integer}(c::MyContainer{T,Vector{T}})
        return c.a[1]+3
    end

    julia> myfunc(MyContainer(1:3))
    2

    julia> myfunc(MyContainer(1.0:3))
    3.0

    julia> myfunc(MyContainer([1:3]))
    4

As you can see, with this approach it's possible to specialize on both
the element type ``T`` and the array type ``A``.

However, there's one remaining hole: we haven't enforced that ``A``
has element type ``T``, so it's perfectly possible to construct an
object like this::

  julia> b = MyContainer{Int64, UnitRange{Float64}}(1.3:5);

  julia> typeof(b)
  MyContainer{Int64,UnitRange{Float64}}

To prevent this, we can add an inner constructor::

    type MyBetterContainer{T<:Real, A<:AbstractVector}
        a::A

        MyBetterContainer(v::AbstractVector{T}) = new(v)
    end
    MyBetterContainer(v::AbstractVector) = MyBetterContainer{eltype(v),typeof(v)}(v)


    julia> b = MyBetterContainer(1.3:5);

    julia> typeof(b)
    MyBetterContainer{Float64,UnitRange{Float64}}

    julia> b = MyBetterContainer{Int64, UnitRange{Float64}}(1.3:5);
    ERROR: no method MyBetterContainer(UnitRange{Float64},)

The inner constructor requires that the element type of ``A`` be ``T``.

Nothingness and missing values
------------------------------

How does "null" or "nothingness" work in Julia?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Unlike many languages (for example, C and Java), Julia does not have a
"null" value. When a reference (variable, object field, or array element)
is uninitialized, accessing it will immediately throw an error. This
situation can be detected using the ``isdefined`` function.

Some functions are used only for their side effects, and do not need to
return a value. In these cases, the convention is to return the value
``nothing``, which is just a singleton object of type ``Void``. This
is an ordinary type with no fields; there is nothing special about it
except for this convention, and that the REPL does not print anything
for it. Some language constructs that would not otherwise have a value
also yield ``nothing``, for example ``if false; end``.

For situations where a value exists only sometimes (for example, missing
statistical data), it is best to use the ``Nullable{T}`` type, which allows
specifying the type of a missing value.

The empty tuple (``()``) is another form of nothingness. But, it should not
really be thought of as nothing but rather a tuple of zero values.

In code written for Julia prior to version 0.4 you may occasionally see ``None``,
which is quite different. It is the empty (or "bottom") type, a type with no values
and no subtypes (except itself). This is now written as ``Union()`` (an empty union
type). You will generally not need to use this type.

Julia Releases
----------------

Do I want to use a release, beta, or nightly version of Julia?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You may prefer the release version of Julia if you are looking for a stable code base. Releases generally occur every 6 months, giving you a stable platform for writing code.

You may prefer the beta version of Julia if you don't mind being slightly behind the latest bugfixes and changes, but find the slightly faster rate of changes more appealing. Additionally, these binaries are tested before they are published to ensure they are fully functional.

You may prefer the nightly version of Julia if you want to take advantage of the latest updates to the language, and don't mind if the version available today occasionally doesn't actually work.

Finally, you may also consider building Julia from source for yourself. This option is mainly for those individuals who are comfortable at the command line, or interested in learning. If this describes you, you may also be interested in reading our `guidelines for contributing`__.

__ https://github.com/JuliaLang/julia/blob/master/CONTRIBUTING.md

Links to each of these download types can be found on the download page at http://julialang.org/downloads/. Note that not all versions of Julia are available for all platforms.

When are deprecated functions removed?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Deprecated functions are removed after the subsequent release. For example, functions marked as deprecated in the 0.1 release will not be available starting with the 0.2 release.

Developing Julia
----------------

How do I debug julia's C code? (running the julia REPL from within a debugger like gdb)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

First, you should build the debug version of julia with ``make
debug``.  Below, lines starting with ``(gdb)`` mean things you should
type at the gdb prompt.

From the shell
^^^^^^^^^^^^^^

The main challenge is that Julia and gdb each need to have their own
terminal, to allow you to interact with them both.  One approach is to
use gdb's ``attach`` functionality to debug an already-running julia
session.  However, on many systems you'll need root access to get this
to work. What follows is a method that can be implemented with just
user-level permissions.

The first time you do this, you'll need to define a script, here
called ``oterm``, containing the following lines::

    ps
    sleep 600000

Make it executable with ``chmod +x oterm``.

Now:

- From a shell (called shell 1), type ``xterm -e oterm &``. You'll see
  a new window pop up; this will be called terminal 2.

- From within shell 1, ``gdb julia-debug``. You can find this
  executable within ``julia/usr/bin``.

- From within shell 1, ``(gdb) tty /dev/pts/#`` where ``#`` is the
  number shown after ``pts/`` in terminal 2.

- From within shell 1, ``(gdb) run``

- From within terminal 2, issue any preparatory commands in Julia that
  you need to get to the step you want to debug

- From within shell 1, hit Ctrl-C

- From within shell 1, insert your breakpoint, e.g., ``(gdb) b codegen.cpp:2244``
- From within shell 1, ``(gdb) c`` to resume execution of julia

- From within terminal 2, issue the command that you want to
  debug. Shell 1 will stop at your breakpoint.


Within emacs
^^^^^^^^^^^^

- ``M-x gdb``, then enter ``julia-debug`` (this is easiest from
  within julia/usr/bin, or you can specify the full path)

- ``(gdb) run``

- Now you'll see the Julia prompt. Run any commands in Julia you need
  to get to the step you want to debug.

- Under emacs' "Signals" menu choose BREAK---this will return you to the ``(gdb)`` prompt

- Set a breakpoint, e.g., ``(gdb) b codegen.cpp:2244``

- Go back to the Julia prompt via ``(gdb) c``

- Execute the Julia command you want to see running.
