.. _man-variables-and-scoping:

.. currentmodule:: Base

********************
 Scope of Variables
********************

The *scope* of a variable is the region of code within which a variable
is visible. Variable scoping helps avoid variable naming conflicts. The
concept is intuitive: two functions can both have arguments called ``x``
without the two ``x``'s referring to the same thing. Similarly there are
many other cases where different blocks of code can use the same name
without referring to the same thing. The rules for when the same
variable name does or doesn't refer to the same thing are called scope
rules; this section spells them out in detail.

Certain constructs in the language introduce *scope blocks*, which are
regions of code that are eligible to be the scope of some set of
variables. The scope of a variable cannot be an arbitrary set of source
lines; instead, it will always line up with one of these blocks.
The constructs introducing such blocks are:

-  ``function`` bodies (:ref:`either syntax <man-functions>`)
-  ``while`` loops
-  ``for`` loops
-  ``try`` blocks
-  ``catch`` blocks
-  ``let`` blocks
-  ``type`` blocks.

Notably missing from this list are
:ref:`begin blocks <man-compound-expressions>` and :ref:`if blocks <man-conditional-evaluation>`, which do
*not* introduce new scope blocks.

Certain constructs introduce new variables into the current innermost
scope. When a variable is introduced into a scope, it is also inherited
by all inner scopes unless one of those inner scopes explicitly
overrides it.

Julia uses `lexical scoping <http://en.wikipedia.org/wiki/Scope_%28computer_science%29#Lexical_scoping_vs._dynamic_scoping>`_,
meaning that a function's scope does not inherit from its caller's
scope, but from the scope in which the function was defined.
For example, in the following code the ``x`` inside ``foo`` is found
in the global scope (and if no global variable ``x`` existed, an
undefined variable error would be raised)::

    function foo()
      x
    end

    function bar()
      x = 1
      foo()
    end

    x = 2

    julia> bar()
    2

If ``foo`` is instead defined inside ``bar``, then it accesses
the local ``x`` present in that function::

    function bar()
      function foo()
        x
      end
      x = 1
      foo()
    end

    x = 2

    julia> bar()
    1

The constructs that introduce new variables into the current scope
are as follows:

-  A declaration ``local x`` or ``const x`` introduces a new local variable.
-  A declaration ``global x`` makes ``x`` in the current scope and inner
   scopes refer to the global variable of that name.
-  A function's arguments are introduced as new local variables into the
   function's body scope.
-  An assignment ``x = y`` introduces a new local variable ``x`` only if
   ``x`` is neither declared global nor introduced as local
   by any enclosing scope before *or after* the current line of code.

In the following example, there is only one ``x`` assigned both inside
and outside the ``for`` loop::

    function foo(n)
      x = 0
      for i = 1:n
        x = x + 1
      end
      x
    end

    julia> foo(10)
    10

In the next example, the loop has a separate ``x`` and the function
always returns zero::

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

In this example, an ``x`` exists only inside the loop, and the function
encounters an undefined variable error on its last line (unless there is
a global variable ``x``)::

    function foo(n)
      for i = 1:n
        x = i
      end
      x
    end

    julia> foo(10)
    in foo: x not defined

A variable that is not assigned to or otherwise introduced locally
defaults to global, so this function would return the value of the
global ``x`` if there were such a variable, or produce an error if no such
global existed. As a consequence, the only way to assign to a global
variable inside a non-top-level scope is to explicitly declare the
variable as global within some scope, since otherwise the assignment
would introduce a new local rather than assigning to the global. This
rule works out well in practice, since the vast majority of variables
assigned inside functions are intended to be local variables, and using
global variables should be the exception rather than the rule,
and assigning new values to them even more so.

One last example shows that an outer assignment introducing ``x`` need
not come before an inner usage::

    function foo(n)
      f = y -> n + x + y
      x = 1
      f(2)
    end

    julia> foo(10)
    13

This behavior may seem slightly odd for a normal variable, but allows
for named functions — which are just normal variables holding function
objects — to be used before they are defined. This allows functions to
be defined in whatever order is intuitive and convenient, rather than
forcing bottom up ordering or requiring forward declarations, both of
which one typically sees in C programs. As an example, here is an
inefficient, mutually recursive way to test if positive integers are
even or odd::

    even(n) = n == 0 ? true  :  odd(n-1)
    odd(n)  = n == 0 ? false : even(n-1)

    julia> even(3)
    false

    julia> odd(3)
    true

Julia provides built-in, efficient functions to test for oddness and evenness
called :func:`iseven` and :func:`isodd` so the above definitions should only be
taken as examples.

Since functions can be used before they are defined, as long as they are
defined by the time they are actually called, no syntax for forward
declarations is necessary, and definitions can be ordered arbitrarily.

At the interactive prompt, variable scope works the same way as anywhere
else. The prompt behaves as if there is scope block wrapped around
everything you type, except that this scope block is identified with the
global scope. This is especially evident in the case of assignments:

.. doctest::

    julia> for i = 1:1; y = 10; end

    julia> y
    ERROR: y not defined

    julia> y = 0
    0

    julia> for i = 1:1; y = 10; end

    julia> y
    10

In the former case, ``y`` only exists inside of the ``for`` loop. In the
latter case, an outer ``y`` has been introduced and so is inherited
within the loop. Due to the special identification of the prompt's scope
block with the global scope, it is not necessary to declare ``global y``
inside the loop. However, in code not entered into the interactive
prompt this declaration would be necessary in order to modify a global
variable.

Multiple variables can be declared global using the following syntax::

    function foo()
        global x=1, y="bar", z=3
    end
    
    julia> foo()
    3
    
    julia> x
    1
    
    julia> y
    "bar"
    
    julia> z
    3

The ``let`` statement provides a different way to introduce variables.
Unlike assignments to local variables, ``let`` statements allocate new
variable bindings each time they run. An assignment modifies an existing
value location, and ``let`` creates new locations. This difference is
usually not important, and is only detectable in the case of variables
that outlive their scope via closures. The ``let`` syntax accepts a
comma-separated series of assignments and variable names::

    let var1 = value1, var2, var3 = value3
        code
    end

The assignments are evaluated in order, with each right-hand side
evaluated in the scope before the new variable on the left-hand side
has been introduced. Therefore it makes sense to write something like
``let x = x`` since the two ``x`` variables are distinct and have separate
storage. Here is an example where the behavior of ``let`` is needed::

    Fs = cell(2)
    i = 1
    while i <= 2
      Fs[i] = ()->i
      i += 1
    end

    julia> Fs[1]()
    3

    julia> Fs[2]()
    3

Here we create and store two closures that return variable ``i``.
However, it is always the same variable ``i``, so the two closures
behave identically. We can use ``let`` to create a new binding for
``i``::

    Fs = cell(2)
    i = 1
    while i <= 2
      let i = i
        Fs[i] = ()->i
      end
      i += 1
    end

    julia> Fs[1]()
    1

    julia> Fs[2]()
    2

Since the ``begin`` construct does not introduce a new scope, it can be
useful to use a zero-argument ``let`` to just introduce a new scope
block without creating any new bindings:

.. doctest::

    julia> begin
             local x = 1
             begin
               local x = 2
             end
             x
           end
    ERROR: syntax: local "x" declared twice

    julia> begin
             local x = 1
             let
               local x = 2
             end
             x
           end
    1

The first example is invalid because you cannot declare the same
variable as local in the same scope twice. The second example is valid
since the ``let`` introduces a new scope block, so the inner local ``x``
is a different variable than the outer local ``x``.

For Loops and Comprehensions
----------------------------

``for`` loops and :ref:`comprehensions <comprehensions>` have a special
additional behavior: any new variables introduced in their body scopes are
freshly allocated for each loop iteration. Therefore these constructs are
similar to ``while`` loops with ``let`` blocks inside::

    Fs = cell(2)
    for i = 1:2
        Fs[i] = ()->i
    end

    julia> Fs[1]()
    1

    julia> Fs[2]()
    2

``for`` loops will reuse existing variables for iteration::

    i = 0
    for i = 1:3
    end
    i  # here equal to 3

However, comprehensions do not do this, and always freshly allocate their
iteration variables::

    x = 0
    [ x for x=1:3 ]
    x  # here still equal to 0

Constants
---------

A common use of variables is giving names to specific, unchanging
values. Such variables are only assigned once. This intent can be
conveyed to the compiler using the ``const`` keyword::

    const e  = 2.71828182845904523536
    const pi = 3.14159265358979323846

The ``const`` declaration is allowed on both global and local variables,
but is especially useful for globals. It is difficult for the compiler
to optimize code involving global variables, since their values (or even
their types) might change at almost any time. If a global variable will
not change, adding a ``const`` declaration solves this performance
problem.

Local constants are quite different. The compiler is able to determine
automatically when a local variable is constant, so local constant
declarations are not necessary for performance purposes.

Special top-level assignments, such as those performed by the
``function`` and ``type`` keywords, are constant by default.

Note that ``const`` only affects the variable binding; the variable may
be bound to a mutable object (such as an array), and that object may
still be modified.
