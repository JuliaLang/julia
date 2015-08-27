*******************
Julia ASTs
*******************

.. currentmodule:: Base



Julia has two AST representations. First there is a surface syntax AST returned
by the parser (e.g. the :func:`parse` function), and manipulated by macros.
It is a structured representation of code as it is written,
constructed by ``julia-parser.scm`` from a character stream.
Next there is a lowered AST which is used by type inference and code generation.
In the lowered form, there are generally fewer types of nodes, all macros
are expanded, and all control flow is converted to explicit branches and sequences
of statements. The lowered form is constructed by ``julia-syntax.scm``.

First we will focus on the lowered form, since it is more important to the
compiler. It is also less obvious to the human, since it results from a
significant rearrangement of the input syntax.


Lowered form
------------

The following data types exist in lowered ASTs:

``Expr``
    has a node type indicated by the ``head`` field, and an ``args`` field
    which is a ``Vector{Any}`` of subexpressions.

``Symbol``
    used to name local variables and static parameters within a function.

``LambdaStaticData``
    wraps the AST of each function, including inner functions.

``LineNumberNode``
    line number metadata

``LabelNode``
    branch target, a consecutively-numbered integer starting at 0

``GotoNode``
    unconditional branch

``QuoteNode``
    wraps an arbitrary value to reference as data. For example, the function
    ``f() = :a`` contains a ``QuoteNode`` whose ``value`` field is the symbol
    ``a``, in order to return the symbol itself instead of evaluating it.

``GlobalRef``
    refers to global variable ``name`` in module ``mod``

``TopNode``
    forces a name to be resolved as a global in Base. This is now mostly
    redundant with ``GlobalRef(Base, :x)``.

``SymbolNode``
    used to annotate a local variable with a type

``GenSym``
    refers to a consecutively-numbered (starting at 0) static single assignment
    (SSA) variable inserted by the compiler.

``NewvarNode``
    marks a point where a closed variable needs to have a new box allocated.


Expr types
~~~~~~~~~~

These symbols appear in the ``head`` field of ``Expr``\s in lowered form.

``call``
    function call. ``args[1]`` is the function to call, ``args[2:end]`` are the
    arguments.

``line``
    line number and file name metadata. Unlike a ``LineNumberNode``, can also
    contain a file name.

``gotoifnot``
    conditional branch. If ``args[1]`` is false, goes to label identified in ``args[2]``.

``=``
    assignment

``method``
    adds a method to a generic function and assigns the result if necessary.

    ``args[1]`` - function name (symbol), or a ``GlobalRef``, or an ``Expr``
    with head ``kw``.  In the ``(kw f)`` case, the method is actually a keyword
    argument sorting function for ``f``. It will be stored instead in
    ``generic_function->env->kwsorter``.

    If ``method`` has only one argument, it corresponds to the form ``function
    foo end`` and only creates a function without adding any methods.

    ``args[2]`` - a ``SimpleVector`` of argument type data. ``args[2][1]`` is
    a ``Tuple type`` of the argument types, and ``args[2][2]`` is a
    ``SimpleVector`` of type variables corresponding to the method's static
    parameters.

    ``args[3]`` - a ``LambdaStaticData`` of the method itself.

    ``args[4]`` - ``true`` or ``false``, identifying whether the method is
    staged (``@generated function``)

``const``
    declares a (global) variable as constant

``null``
    has no arguments; simply yields the value ``nothing``

``static_typeof``
    a horrible misfeature used to determine the result type of array
    comprehensions. Planned to be removed.

``type_goto``
    a virtual control flow edge used to convey type data to ``static_typeof``,
    also to be removed.

``new``
    allocates a new struct-like object. First argument is the type. The ``new``
    pseudo-function is lowered to this, and the type is always inserted by the
    compiler.  This is very much an internal-only feature, and does no
    checking. Evaluating arbitrary ``new`` expressions can easily segfault.

``return``
    returns its argument as the value of the enclosing function.

``the_exception``
    yields the caught exception inside a ``catch`` block. This is the value of
    the run time system variable ``jl_exception_in_transit``.

``enter``
    enters an exception handler (``setjmp``). ``args[1]`` is the label of the
    catch block to jump to on error.

``leave``
    pop exception handlers. ``args[1]`` is the number of handlers to pop.

``boundscheck``
    controls turning bounds checks on or off. A stack is maintained; if the
    first argument of this expression is true or false (``true`` means bounds
    checks are enabled), it is pushed onto the stack. If the first argument is
    ``:pop``, the stack is popped.

``copyast``
    part of the implementation of quasi-quote. The argument is a surface syntax
    AST that is simply copied recursively and returned at run time.

``meta``
    metadata. Currently used for inlining hints, represented by the symbols
    ``:inline`` and ``:noinline``.


LambdaStaticData
~~~~~~~~~~~~~~~~

Has an ``->ast`` field pointing to an ``Expr`` with head ``lambda``. This
``Expr`` has the following layout:

``args[1]``
    ``Vector{Any}`` of argument name symbols. For varargs functions, the last
    element is actually an ``Expr`` with head ``...``. The argument of this
    ``Expr`` is an ``Expr`` with head ``::``. The first argument of ``::`` is a
    symbol (the argument name), and the second argument is a type declaration.

``args[2]``
    A ``Vector{Any}`` with variable information:

    ``args[2][1]`` - An array of 3-element ``varinfo`` arrays, one for each
    argument or local variable. A ``varinfo`` array has the form ``Any[:name,
    type, bits]``. The ``bits`` field is an integer
    describing variable properties as follows:
    - 1  - captured (closed over)
    - 2  - assigned (only false if there are *no* assignment statements with this var on the left)
    - 4  - assigned by an inner function
    - 8  - const (currently unused for local variables)
    - 16 - statically assigned once
    - 32 - might be used before assigned. This flag is only valid after type inference.

    ``args[2][2]`` - An array of ``varinfo`` triples for each outer variable
    this function captures.

    ``args[2][3]`` - The types of variables represented by ``GenSym`` objects.
    Given ``GenSym`` ``g``, its type will be at ``args[2][3][g.id+1]``.

    ``args[2][4]`` - The names (symbols) of static parameters.

``args[3]``
    an ``Expr`` with head ``body`` whose arguments are the statements
    comprising the function body.


Surface syntax AST
------------------

Front end ASTs consist entirely of ``Expr``\ s and atoms (e.g. symbols, numbers).
There is generally a different expression head for each visually distinct
syntactic form.
Examples will be given in s-expression syntax. Each parenthesized list corresponds
to an Expr, where the first element is the head.
For example ``(call f x)`` corresponds to ``Expr(:call, :f, :x)`` in julia.

Calls
~~~~~

=======================  ====================================
Input                    AST
=======================  ====================================
f(x)                     (call f x)
f(x, y=1, z=2)           (call f x (kw y 1) (kw z 2))
f(x; y=1)                (call f (parameters (kw y 1)) x)
f(x...)                  (call f (... x))
=======================  ====================================

``Do`` syntax::

    f(x) do a,b
        body
    end

parses as ``(call f (-> (tuple a b) (block body)) x)``.

Operators
~~~~~~~~~

Most uses of operators are just function calls, so they are parsed with the
head ``call``.
However some operators are special forms (not necessarily function calls),
and in those cases the operator itself is the expression head.
In julia-parser.scm these are referred to as "syntactic operators".
Some operators (``+`` and ``*``) use N-ary parsing; chained calls are parsed as
a single N-argument call.
Finally, chains of comparisons have their own special expression structure.

=======================  ====================================
Input                    AST
=======================  ====================================
x+y                      (call + x y)
a+b+c+d                  (call + a b c d)
2x                       (call * 2 x)
a&&b                     (&& a b)
x += 1                   (+= x 1)
a ? 1 : 2                (if a 1 2)
a:b                      (: a b)
a:b:c                    (: a b c)
a,b                      (tuple a b)
a==b                     (comparison a == b)
1<i<=n                   (comparison 1 < i <= n)
a.b                      (. a (quote b))
a.(b)                    (. a b)
=======================  ====================================

Bracketed forms
~~~~~~~~~~~~~~~

=======================  ====================================
Input                    AST
=======================  ====================================
a[i]                     (ref a i)
t[i;j]                   (typed_vcat t i j)
t[i j]                   (typed_hcat t i j)
t[a b; c d]              (typed_vcat t (row a b) (row c d))
a{b}                     (curly a b)
a{b;c}                   (curly a (parameters c) b)
[x]                      (vect x)
[x,y]                    (vect x y)
[x;y]                    (vcat x y)
[x y]                    (hcat x y)
[x y; z t]               (vcat (row x y) (row z t))
[x for y in z, a in b]   (comprehension x (= y z) (= a b))
T[x for y in z]          (typed_comprehension T x (= y z))
[a=>b for x in y]        (dict_comprehension (=> a b) (= x y))
(k=>v)[a=>b for x in y]  (typed_dict_comprehension (=> k v) (=> a b) (= x y))
(a, b, c)                (tuple a b c)
(a; b; c)                (block a (block b c))
=======================  ====================================

Macros
~~~~~~

=======================  ====================================
Input                    AST
=======================  ====================================
@m x y                   (macrocall @m x y)
Base.@m x y              (macrocall (. Base (quote @m)) x y)
@Base.m x y              (macrocall (. Base (quote @m)) x y)
=======================  ====================================

Strings
~~~~~~~

=======================  ====================================
Input                    AST
=======================  ====================================
"a"                      "a"
x"y"                     (macrocall @x_str "y")
x"y"z                    (macrocall @x_str "y" "z")
"x = $x"                 (string "x = " x)
\`a b c\`                (macrocall @cmd "a b c")
=======================  ====================================

Doc string syntax::

    "some docs"
    f(x) = x

parses as ``(macrocall (|.| Base '@doc) "some docs" (= (call f x) (block x)))``

Imports and such
~~~~~~~~~~~~~~~~

=======================  ====================================
Input                    AST
=======================  ====================================
import a                 (import a)
import a.b.c             (import a b c)
import ...a              (import . . . a)
import a.b, c.d          (toplevel (import a b) (import c d))
import Base: x           (import Base x)
import Base: x, y        (toplevel (import Base x) (import Base y))
export a, b              (export a b)
=======================  ====================================

Numbers
~~~~~~~

Julia supports more number types than many scheme implementations,
so not all numbers are represented directly as scheme numbers in the AST.

=======================  ====================================
Input                    AST
=======================  ====================================
11111111111111111111     (macrocall @int128_str "11111111111111111111")
0xfffffffffffffffff      (macrocall @uint128_str "0xfffffffffffffffff")
1111...many digits...    (macrocall @big_str "1111....")
=======================  ====================================

Block forms
~~~~~~~~~~~

A block of statements is parsed as ``(block stmt1 stmt2 ...)``.

If statement::

    if a
        b
    elseif c
        d
    else e
        f
    end

parses as::

    (if a (block (line 2) b)
        (block (line 3) (if c (block (line 4) d)
                            (block (line 5) e (line 6) f))))

A ``while`` loop parses as ``(while condition body)``.

A ``for`` loop parses as ``(for (= var iter) body)``.
If there is more than one iteration specification, they are parsed as a block:
``(for (block (= v1 iter1) (= v2 iter2)) body)``.

``break`` and ``continue`` are parsed as 0-argument expressions
``(break)`` and ``(continue)``.

``let`` is parsed as ``(let body (= var1 val1) (= var2 val2) ...)``.

A basic function definition is parsed as ``(function (call f x) body)``.
A more complex example::

    function f{T}(x::T; k = 1)
        return x+1
    end

parses as::

    (function (call (curly f T) (parameters (kw k 1))
                    (:: x T))
              (block (line 2 file.jl) (return (call + x 1))))

Type definition::

    type Foo{T<:S}
        x::T
    end

parses as::

    (type #t (curly Foo (<: T S))
          (block (line 2 none) (:: x T)))

The first argument is a boolean telling whether the type is mutable.

``try`` blocks parse as ``(try try_block var catch_block finally_block)``.
If no variable is present after ``catch``, ``var`` is ``#f``.
If there is no ``finally`` clause, then the last argument is not present.
