.. _man-variables-and-scoping:

.. currentmodule:: Base

********************
 Scope of Variables
********************

The *scope* of a variable is the region of code within which a
variable is visible. Variable scoping helps avoid variable naming
conflicts. The concept is intuitive: two functions can both have
arguments called ``x`` without the two ``x``'s referring to the same
thing. Similarly there are many other cases where different blocks of
code can use the same name without referring to the same thing. The
rules for when the same variable name does or doesn't refer to the
same thing are called scope rules; this section spells them out in
detail.

Certain constructs in the language introduce *scope blocks*, which are
regions of code that are eligible to be the scope of some set of
variables. The scope of a variable cannot be an arbitrary set of
source lines; instead, it will always line up with one of these
blocks.  There are two main types of scopes in Julia, *global scope*
and *local scope*, the latter can be nested.  The constructs
introducing scope blocks are:

.. _man-scope-table:

+--------------------------------+----------------------------------------------------------------------------------+
| Scope name                     | block/construct introducing this kind of scope                                   |
+================================+==================================================================================+
| :ref:`global <man-global>`     | | module, baremodule, at interactive prompt (REPL)                               |
+--------------------------------+------------------------------+---------------------------------------------------+
| :ref:`local <man-local-scope>` | :ref:`soft <man-soft-scope>` | | for, while, list-comprehensions,                |
|                                |                              |   try-catch-finally, let                          |
|                                +------------------------------+---------------------------------------------------+
|                                | :ref:`hard <man-hard-scope>` | | functions (either syntax, anonymous & do-blocks)|
|                                |                              | | type, immutable, macro                          |
+--------------------------------+------------------------------+---------------------------------------------------+

Notably missing from this table are :ref:`begin blocks
<man-compound-expressions>` and :ref:`if blocks
<man-conditional-evaluation>`, which do *not* introduce new scope
blocks.  All three types of scopes follow somewhat different rules
which will be explained below as well as some extra rules for
certain blocks.

Julia uses `lexical scoping <https://en.wikipedia.org/wiki/Scope_%28computer_science%29#Lexical_scoping_vs._dynamic_scoping>`_,
meaning that a function's scope does not inherit from its caller's
scope, but from the scope in which the function was defined.
For example, in the following code the ``x`` inside ``foo`` refers
to the ``x`` in the global scope of its module ``Bar``::

    module Bar
    x = 1
    foo() = x
    end

and not a ``x`` in the scope where ``foo`` is used::

    julia> import Bar

    julia> x = -1;

    julia> Bar.foo()
    1

Thus *lexical scope* means that the scope of variables can be inferred
from the source code alone.

.. _man-global:

Global Scope
------------

*Each module introduces a new global scope*, separate from the global
scope of all other modules; there is no all-encompassing global scope.
Modules can introduce variables of other modules into their scope
through the :ref:`using or import <man-modules>` statements or through
qualified access using the dot-notation, i.e. each module is a
so-called *namespace*.  Note that variable bindings can only be
changed within their global scope and not from an outside module. ::

    module A
    a = 1 # a global in A's scope
    end

    module B
    # b = a # would error as B's global scope is separate from A's
        module C
        c = 2
        end
    b = C.c # can access the namespace of a nested global scope
            # through a qualified access
    import A # makes module A available
    d = A.a
    # A.a = 2 # would error with: "ERROR: cannot assign variables in other modules"
    end

Note that the interactive prompt (aka REPL) is in the global scope of
the module ``Main``.

.. _man-local-scope:

Local Scope
-----------

A new local scope is introduced by most code-blocks, see above
:ref:`table <man-scope-table>` for a complete list.  A local scope
*usually* inherits all the variables from its parent scope, both for
reading and writing.  There are two subtypes of local scopes, hard and
soft, with slightly different rules concerning what variables are
inherited.  Unlike global scopes, local scopes are not namespaces,
thus variables in an inner scope cannot be retrieved from the parent
scope through some sort of qualified access.

The following rules and examples pertain to both hard and soft local
scopes.  A newly introduced variable in a local scope does not
back-propagate to its parent scope.  For example, here the ``z`` is not
introduced into the top-level scope::

    for i=1:10
        z = i
    end

    julia> z
    ERROR: UndefVarError: z not defined

(Note, in this and all following examples it is assumed that their
top-level is a global scope with a clean workspace, for instance a
newly started REPL.)

Inside a local scope a variable can be forced to be a local variable
using the ``local`` keyword::

    x = 0
    for i=1:10
        local x
        x = i + 1
    end

    julia> x
    0

Inside a local scope a new global variable can be defined using the
keyword ``global``::

    for i=1:10
        global z
        z = i
    end

    julia> z
    10

..
   However, there is no keyword to introduce a new local variable into a
   parent local scope.

The location of both the ``local`` and ``global`` keywords within the
scope block is irrelevant.  The following is equivalent to the last
example (although stylistically worse)::

    for i=1:10
        z = i
        global z
    end

    julia> z
    10

Multiple global or local definitions can be on one line and can also
be paired with assignments::

    for i=1:10
        global x=i, y, z
        local a=4, b , c=1
    end


.. _man-soft-scope:

Soft Local Scope
^^^^^^^^^^^^^^^^

  In a soft local scope, all variables are inherited from its parent
  scope unless a variable is specifically marked with the keyword
  ``local``.

Soft local scopes are introduced by for-loops, while-loops,
list-comprehensions, try-catch-finally-blocks, and let-blocks.  There
are some extra rules for :ref:`let-blocks <man-let-blocks>` and for
:ref:`for-loops and list-comprehensions <man-for-loops-scope>`.

In the following example the ``x`` and ``y`` refer always to the same
variables as the soft local scope inherits both read and write
variables::

    x,y = 0, 1
    for i = 1:10
        x = i + y + 1
    end

    julia> x
    11

Within soft scopes, the `global` keyword is never necessary, although
allowed.  The only case when it would change the semantics is
(currently) a syntax error::

    let
        local x = 2
        let
            global x = 3
        end
    end

    # ERROR: syntax: `global x`: x is local variable in the enclosing scope

.. _man-hard-scope:

Hard Local Scope
^^^^^^^^^^^^^^^^

Hard local scopes are introduced by function definitions (in all their
forms), type & immutable-blocks and macro-definitions.

   In a hard local scope, all variables are inherited from its parent
   scope unless:

   - an assignment would result in a modified *global* variable, or
   - a variable is specifically marked with the keyword ``local``.

Thus global variables are only inherited for reading but not for
writing::

    x,y = 1,2
    function foo()
        x = 2 # assignment introduces a new local
        return x + y # y refers to the global
    end

    julia> foo()
    4

    julia> x
    1

An explicit ``global`` is needed to assign to a global variable::

    x = 1
    function foo()
        global x = 2
    end
    foo()

    julia> x
    2

Note that *nested functions* can behave differently to functions
defined in the global scope as they can modify their parent scope's
*local* variables::

    x,y = 1,2
    function foo()
        x = 2 # introduces a new local
        function bar()
            x = 10 # modifies the parent's x
            return x+y # y is global
        end
        return bar() + x # 12 + 10 (x is modified in call of bar())
    end

    julia> foo()
    22  # (x,y unchanged)

The distinction between inheriting global and local variables for
assignment can lead to some slight differences between functions
defined in local vs. global scopes.  Consider the modification of the
last example by moving ``bar`` to the global scope::

    x,y = 1,2
    function bar()
        x = 10 # local
        return x+y
    end
    function foo()
        x = 2 # local
        return bar() + x # 12 + 2 (x is not modified)
    end

    julia> foo()
    14 # as x is not modified anymore.
       # (x,y unchanged)

Note that above subtlety does not pertain to type and macro
definitions as they can only appear at the global scope.
There are special scoping rules concerning the evaluation of default
and keyword function arguments which are described in the
:ref:`Function section <man-evaluation-scope-default-values>`.


An assignment introducing a variable used inside a function, type or
macro definition need not come before its inner usage:

.. doctest::

    julia> f = y -> x + y
    (anonymous function)

    julia> f(3)
    ERROR: UndefVarError: x not defined
     in anonymous at none:1

    julia> x = 1
    1

    julia> f(3)
    4

This behavior may seem slightly odd for a normal variable, but allows
for named functions — which are just normal variables holding function
objects — to be used before they are defined. This allows functions to
be defined in whatever order is intuitive and convenient, rather than
forcing bottom up ordering or requiring forward declarations, as long
as they are defined by the time they are actually called.  As an
example, here is an inefficient, mutually recursive way to test if
positive integers are even or odd::

    even(n) = n == 0 ? true  :  odd(n-1)
    odd(n)  = n == 0 ? false : even(n-1)

    julia> even(3)
    false

    julia> odd(3)
    true

Julia provides built-in, efficient functions to test for oddness and evenness
called :func:`iseven` and :func:`isodd` so the above definitions should only be
taken as examples.

Hard vs. Soft Local Scope
^^^^^^^^^^^^^^^^^^^^^^^^^

Blocks which introduce a soft local scope, such as loops, are
generally used to manipulate the variables in their parent scope.
Thus their default is to fully access all variables in their parent
scope.

Conversely, the code inside blocks which introduce a hard local scope
(function, type and macro definitions) can be executed at any place in
a program.  Remotely changing the state of global variables in other
modules should be done with care and thus this is an opt-in feature
requiring the ``global`` keyword.

The reason to allow *modifying local* variables of parent scopes in
nested functions is to allow constructing `closures
<https://en.wikipedia.org/wiki/Closure_%28computer_programming%29>`_
which have a private state, for instance the ``state`` variable in the
following example::

    let
        state = 0
        global counter
        counter() = state += 1
    end

    julia> counter()
    1

    julia> counter()
    2

See also the closures in the examples in the next two sections.

.. _man-let-blocks:

Let Blocks
^^^^^^^^^^

Unlike assignments to local variables, ``let`` statements allocate new
variable bindings each time they run. An assignment modifies an
existing value location, and ``let`` creates new locations. This
difference is usually not important, and is only detectable in the
case of variables that outlive their scope via closures. The ``let``
syntax accepts a comma-separated series of assignments and variable
names::

    let var1 = value1, var2, var3 = value3
        code
    end

The assignments are evaluated in order, with each right-hand side
evaluated in the scope before the new variable on the left-hand side
has been introduced. Therefore it makes sense to write something like
``let x = x`` since the two ``x`` variables are distinct and have separate
storage. Here is an example where the behavior of ``let`` is needed::

    Fs = Array(Any,2)
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

    Fs = Array(Any,2)
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

    julia> let
             local x = 1
             let
               local x = 2
             end
             x
           end
    1

Since ``let`` introduces a new scope block, the inner local ``x``
is a different variable than the outer local ``x``.


.. _man-for-loops-scope:

For Loops and Comprehensions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^


``for`` loops and :ref:`comprehensions <comprehensions>` have the
following behavior: any new variables introduced in their body scopes
are freshly allocated for each loop iteration.  This is in contrast to
``while`` loops which reuse the variables for all
iterations. Therefore these constructs are similar to ``while`` loops
with ``let`` blocks inside::

    Fs = Array(Any,2)
    for i = 1:2
        Fs[i] = ()->i
    end

    julia> Fs[1]()
    1

    julia> Fs[2]()
    2

``for`` loops will reuse existing variables for its iteration variable::

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
