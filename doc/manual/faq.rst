.. _man-faq:

****************************
 Frequently-Asked Questions
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
collector runs; you can force this to happen with ``gc()``.

How can I modify the declaration of a type/immutable in my session?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Perhaps you've defined a type and and then realize you need to add a
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


Type declarations and constructors
----------------------------------
.. _man-abstract-fields:

How do "abstract" or ambigious fields in types interact with the compiler?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Types can be declared without specifying the types of their fields::

    type MyAmbiguousType
        a
    end

This allows ``a`` to be of any type. This can often be useful, but it
does have a downside: for objects of type ``MyAmbiguousType``, the
compiler will not be able to generate high-performance code.  The
reason is that the compiler uses the types of objects, not their
values, to determine how to build code. Unfortunately, very little can
be inferred about an object of type ``MyAmbiguousType``::

    julia> b = MyAmbiguousType("Hello")
    MyAmbiguousType("Hello")

    julia> c = MyAmbiguousType(17)
    MyAmbiguousType(17)

    julia> typeof(b)
    MyAmbiguousType

    julia> typeof(c)
    MyAmbiguousType

``b`` and ``c`` have the same type, yet their underlying
representation of data in memory is very different. Even if you stored
just numeric values in field ``a``, the fact that the memory
representation of a ``Uint8`` differs from a ``Float64`` also means
that the CPU needs to handle them using two different kinds of
instructions.  Since the required information is not available in the
type, such decisions have to be made at run-time. This slows
performance.

You can do better by declaring the type of ``a``. Here, we are focused
on the case where ``a`` might be any one of several types, in which
case the natural solution is to use parameters. For example::

    type MyType{T<:FloatingPoint}
        a::T
    end

This is a better choice than
::

    type MyStillAmbiguousType
        a::FloatingPoint
    end

because the first version specifies the type of ``a`` from the type of
the wrapper object.  For example::

    julia> m = MyType(3.2)
    MyType{Float64}(3.2)

    julia> t = MyStillAmbiguousType(3.2)
    MyStillAmbiguousType(3.2)

    julia> typeof(m)
    MyType{Float64}

    julia> typeof(t)
    MyStillAmbiguousType

The type of field ``a`` can be readily determined from the type of
``m``, but not from the type of ``t``.  Indeed, in ``t`` it's possible
to change the type of field ``a``::

    julia> typeof(t.a)
    Float64

    julia> t.a = 4.5f0
    4.5f0
    
    julia> typeof(t.a)
    Float32

In contrast, once ``m`` is constructed, the type of ``m.a`` cannot
change::

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
an abstract type::

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

    disassemble(func,(MyType{Float64},))
    disassemble(func,(MyType{FloatingPoint},))
    disassemble(func,(MyType,))

For reasons of length the results are not shown here, but you may wish
to try this yourself. Because the type is fully-specified in the first
case, the compiler doesn't need to generate any code to resolve the
type at run-time.  This results in shorter and faster code.

How should I declare "abstract container type" fields?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The same best practices that apply in the `previous section
<#man-abstract-fields>`_ also work for container types::

    type MySimpleContainer{A<:AbstractVector}
        a::A
    end

    type MyAmbiguousContainer{T}
        a::AbstractVector{T}
    end

For example::

    julia> c = MySimpleContainer(1:3);

    julia> typeof(c)
    MySimpleContainer{Range1{Int64}}

    julia> c = MySimpleContainer([1:3]);

    julia> typeof(c)
    MySimpleContainer{Array{Int64,1}}

    julia> b = MyAmbiguousContainer(1:3);

    julia> typeof(b)
    MyAmbiguousContainer{Int64}

    julia> b = MyAmbiguousContainer([1:3]);

    julia> typeof(b)
    MyAmbiguousContainer{Int64}

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
explicit versions for ``Range1{T}`` or other abstract types. To
prevent such tedium, you can use two parameters in the declaration of
``MyContainer``::

    type MyContainer{T, A<:AbstractVector}
        a::A
    end
    MyContainer(v::AbstractVector) = MyContainer{eltype(v), typeof(v)}(v)

    julia> b = MyContainer(1.3:5);

    julia> typeof(b)
    MyContainer{Float64,Range1{Float64}}

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

  julia> b = MyContainer{Int64, Range1{Float64}}(1.3:5);

  julia> typeof(b)
  MyContainer{Int64,Range1{Float64}}

To prevent this, we can add an inner constructor::

    type MyBetterContainer{T<:Real, A<:AbstractVector}
        a::A

        MyBetterContainer(v::AbstractVector{T}) = new(v)
    end
    MyBetterContainer(v::AbstractVector) = MyBetterContainer{eltype(v),typeof(v)}(v)


    julia> b = MyBetterContainer(1.3:5);

    julia> typeof(b)
    MyBetterContainer{Float64,Range1{Float64}}

    julia> b = MyBetterContainer{Int64, Range1{Float64}}(1.3:5);
    ERROR: no method MyBetterContainer(Range1{Float64},)

The inner constructor requires that the element type of ``A`` be ``T``.


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

- From within shell 1, ``gdb julia-debug-basic``. You can find this
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

- ``M-x gdb``, then enter ``julia-debug-basic`` (this is easiest from
  within julia/usr/bin, or you can specify the full path)

- ``(gdb) run``

- Now you'll see the Julia prompt. Run any commands in Julia you need
  to get to the step you want to debug.

- Under emacs' "Signals" menu choose BREAK---this will return you to the ``(gdb)`` prompt

- Set a breakpoint, e.g., ``(gdb) b codegen.cpp:2244``

- Go back to the Julia prompt via ``(gdb) c``

- Execute the Julia command you want to see running.
