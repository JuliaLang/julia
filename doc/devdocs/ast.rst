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

Here we will focus on the lowered form, since it is more important to the
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
----------

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
----------------

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
