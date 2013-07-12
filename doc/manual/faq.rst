.. _man-faq:

****************************
 Frequently-Asked Questions
****************************

.. contents::

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

Perhaps you've defined a type and and then realize you need to add a new field.
If you try this at the REPL, you get the error::

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

How do I handle "abstract" fields in types?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
You are not required to declare the type of any fields, so the following is acceptable::

    type MyType
        a
    end

This allows ``a`` to be of any type. However, very little will be known at compile-time about an object of type ``MyType``, and hence performance may suffer. You can do better by declaring the type of ``a``. When ``a`` might be any one of several types, you should probably use parameters. For example::

    type MyType{T<:FloatingPoint}
        a::T
    end

This is a better choice than::

    type MyWorseType
        a::FloatingPoint
    end

because, with the first version, you can write (parametric) functions that know the type of ``a`` at compile time.

This procedure also works fine for container types::

    type MySimpleContainer{A<:AbstractVector}
        a::A
    end

    julia> MySimpleContainer(1:3)
    MySimpleContainer{Range1{Int64}}(1:3)

Again, parametric functions can know the type of ``a`` at compile time::

    function myfunc{A}(c::MySimpleContainer{A})
        ...
    end

However, with this declaration of ``MySimpleContainer``, you can't write compile-time optimized functions that behave differently depending on whether (for example) the element type of ``a`` is integer or floating-point. For that, you'll want to use two parameters::

    type MyContainer{T, A<:AbstractVector}
        a::A
    end
    MyContainer(v::AbstractVector) = MyContainer{eltype(v), typeof(v)}(v)

Note the somewhat surprising fact that ``T`` doesn't appear in the declaration of field ``a``---we'll get back to this point in a moment.  With this approach, one can write functions such as::

    function myfunc{T<:Integer, A<:AbstractArray}(c::MyContainer{T,A})
        return c.a[1]+1
    end

    function myfunc{T<:FloatingPoint, A<:AbstractArray}(c::MyContainer{T,A})
        return c.a[1]+2
    end

    julia> myfunc(MyContainer(1:3))
    2
    
    julia> myfunc(MyContainer(1.0:1:3))
    3.0

However, there's one remaining hole: we haven't actually enforced that ``A`` has element type ``T``, so it's perfectly possible to construct an object like this::

    julia> MyContainer{Int64, Range{Float64}}(1.0:1:3)
    MyContainer{Int64,Range{Float64}}(1.0:1.0:3.0)

To prevent this, we can add an inner constructor::

    type MyBetterContainer{T<:Real, A<:AbstractVector}
        a::A

        MyBetterContainer(v::AbstractVector{T}) = new(v)
    end

This requires that the element type of the ``AbstractVector`` input matches the declared element type ``T``.


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

- From within shell 1, ``(gdb) tty /dev/pts/#`` where ``#`` is the number shown after ``pts/`` in terminal 2.

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
