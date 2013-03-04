.. _man-metaprogramming:

*****************
 Metaprogramming  
*****************

The strongest legacy of Lisp in the Julia language is its
metaprogramming support. Like Lisp, Julia is
`homoiconic <http://en.wikipedia.org/wiki/Homoiconicity>`_: it
represents its own code as a data structure of the language itself.
Since code is represented by objects that can be created and manipulated
from within the language, it is possible for a program to transform and
generate its own code. This allows sophisticated code generation without
extra build steps, and also allows true Lisp-style macros, as compared
to preprocessor "macro" systems, like that of C and C++, that perform
superficial textual manipulation as a separate pass before any real
parsing or interpretation occurs. Another aspect of metaprogramming is
reflection: the ability of a running program to dynamically discover
properties of itself. Reflection emerges naturally from the fact that
all data types and code are represented by normal Julia data structures,
so the structure of the program and its types can be explored
programmatically just like any other data.

Expressions and Eval
--------------------

Julia code is represented as a syntax tree built out of Julia data
structures of type ``Expr``. This makes it easy to construct and
manipulate Julia code from within Julia, without generating or parsing
source text. Here is the definition of the ``Expr`` type::

    type Expr
      head::Symbol
      args::Array{Any,1}
      typ
    end

The ``head`` is a symbol identifying the kind of expression, and
``args`` is an array of subexpressions, which may be symbols referencing
the values of variables at evaluation time, may be nested ``Expr``
objects, or may be actual values of objects. The ``typ`` field is used
by type inference to store type annotations, and can generally be
ignored.

There is special syntax for "quoting" code (analogous to quoting
strings) that makes it easy to create expression objects without
explicitly constructing ``Expr`` objects. There are two forms: a short
form for inline expressions using ``:`` followed by a single expression,
and a long form for blocks of code, enclosed in ``quote ... end``. Here
is an example of the short form used to quote an arithmetic expression::

    julia> ex = :(a+b*c+1)
    +(a,*(b,c),1)

    julia> typeof(ex)
    Expr

    julia> ex.head
    call

    julia> typeof(ans)
    Symbol

    julia> ex.args
    4-element Any Array:
      +        
      a        
      :(*(b,c))
     1         

    julia> typeof(ex.args[1])
    Symbol

    julia> typeof(ex.args[2])
    Symbol

    julia> typeof(ex.args[3])
    Expr

    julia> typeof(ex.args[4])
    Int64

Expressions provided by the parser generally only have symbols, other
expressions, and literal values as their args, whereas expressions
constructed by Julia code can easily have arbitrary run-time values
without literal forms as args. In this specific example, ``+`` and ``a``
are symbols, ``*(b,c)`` is a subexpression, and ``1`` is a literal
64-bit signed integer. Here's an example of the longer expression
quoting form::

    julia> quote
         x = 1
         y = 2
         x + y
       end

    begin
      x = 1
      y = 2
      +(x,y)
    end

When the argument to ``:`` is just a symbol, a ``Symbol`` object results
instead of an ``Expr``::

    julia> :foo
    foo

    julia> typeof(ans)
    Symbol

In the context of an expression, symbols are used to indicate access to
variables, and when an expression is evaluated, a symbol evaluates to
the value bound to that symbol in the appropriate scope (see :ref:`man-variables-and-scoping` for further details).

Eval and Interpolation
~~~~~~~~~~~~~~~~~~~~~~

Given an expression object, one can cause Julia to evaluate (execute) it
at the *top level* scope — i.e. in effect like loading from a file or
typing at the interactive prompt — using the ``eval`` function::

    julia> :(1 + 2)
    +(1,2)

    julia> eval(ans)
    3

    julia> ex = :(a + b)
    +(a,b)

    julia> eval(ex)
    a not defined

    julia> a = 1; b = 2;

    julia> eval(ex)
    3

Expressions passed to ``eval`` are not limited to returning values
— they can also have side-effects that alter the state of the top-level
evaluation environment::

    julia> ex = :(x = 1)
    x = 1

    julia> x
    x not defined

    julia> eval(ex)
    1

    julia> x
    1

Here, the evaluation of an expression object causes a value to be
assigned to the top-level variable ``x``.

Since expressions are just ``Expr`` objects which can be constructed
programmatically and then evaluated, one can, from within Julia code,
dynamically generate arbitrary code which can then be run using
``eval``. Here is a simple example::

    julia> a = 1;

    julia> ex = Expr(:call, {:+,a,:b}, Any)
    :(+(1,b))

    julia> a = 0; b = 2;

    julia> eval(ex)
    3

The value of ``a`` is used to construct the expression ``ex`` which
applies the ``+`` function to the value 1 and the variable ``b``. Note
the important distinction between the way ``a`` and ``b`` are used:

-  The value of the *variable* ``a`` at expression construction time is
   used as an immediate value in the expression. Thus, the value of
   ``a`` when the expression is evaluated no longer matters: the value
   in the expression is already ``1``, independent of whatever the value
   of ``a`` might be.
-  On the other hand, the *symbol* ``:b`` is used in the expression
   construction, so the value of the variable ``b`` at that time is
   irrelevant — ``:b`` is just a symbol and the variable ``b`` need not
   even be defined. At expression evaluation time, however, the value of
   the symbol ``:b`` is resolved by looking up the value of the variable
   ``b``.

Constructing ``Expr`` objects like this is powerful, but somewhat
tedious and ugly. Since the Julia parser is already excellent at
producing expression objects, Julia allows "splicing" or interpolation
of expression objects, prefixed with ``$``, into quoted expressions,
written using normal syntax. The above example can be written more
clearly and concisely using interpolation::

    julia> a = 1;
    1

    julia> ex = :($a + b)
    :(+(1,b))

This syntax is automatically rewritten to the form above where we
explicitly called ``Expr``. The use of ``$`` for expression
interpolation is intentionally reminiscent of
:ref:`string interpolation <man-string-interpolation>` and
:ref:`command interpolation <man-command-interpolation>`.
Expression interpolation allows convenient, readable programmatic construction
of complex Julia expressions.

Code Generation
~~~~~~~~~~~~~~~

When a significant amount of repetitive boilerplate code is required, it
is common to generate it programmatically to avoid redundancy. In most
languages, this requires an extra build step, and a separate program to
generate the repetitive code. In Julia, expression interpolation and
eval allow such code generation to take place in the normal course of
program execution. For example, the following code defines a series of
operators on three arguments in terms of their 2-argument forms::

    for op = (:+, :*, :&, :|, :$)
      eval(quote
        ($op)(a,b,c) = ($op)(($op)(a,b),c)
      end)
    end

In this manner, Julia acts as its own preprocessor, and allows code
generation from inside the language. The above code could be written
slightly more tersely using the ``:`` prefix quoting form::

    for op = (:+, :*, :&, :|, :$)
      eval(:(($op)(a,b,c) = ($op)(($op)(a,b),c)))
    end

This sort of in-language code generation, however, using the
``eval(quote(...))`` pattern, is common enough that Julia comes with a
macro to abbreviate this pattern::

    for op = (:+, :*, :&, :|, :$)
      @eval ($op)(a,b,c) = ($op)(($op)(a,b),c)
    end

The ``@eval`` macro rewrites this call to be precisely equivalent to the
above longer versions. For longer blocks of generated code, the
expression argument given to ``@eval`` can be a block::

    @eval begin
      # multiple lines
    end

Interpolating into an unquoted expression is not supported and will
cause a compile-time error::

    julia> $a + b
    unsupported or misplaced expression $

.. _man-macros:

Macros
------

Macros are the analogue of functions for expression generation at
compile time: they allow the programmer to automatically generate
expressions by transforming zero or more argument expressions into a
single result expression, which then takes the place of the macro call
in the final syntax tree. Macros are invoked with the following general
syntax::

    @name expr1 expr2 ...

Note the distinguishing ``@`` before the macro name and the lack of
commas between the argument expressions. Before the program runs, this
statement will be replaced with the result of calling an expander
function for ``name`` on the expression arguments. Expanders are defined
with the ``macro`` keyword::

    macro name(expr1, expr2, ...)
        ...
    end

Here, for example, is the definition of Julia's ``@assert``
macro (see
`error.jl <https://github.com/JuliaLang/julia/blob/master/base/error.jl>`_)::

    macro assert(ex)
        :($ex ? nothing : error("Assertion failed: ", $(string(ex))))
    end

This macro can be used like this::

    julia> @assert 1==1.0

    julia> @assert 1==0
    Assertion failed: 1==0

Macro calls are expanded so that the above calls are precisely
equivalent to writing

::

    1==1.0 ? nothing : error("Assertion failed: ", "1==1.0")
    1==0 ? nothing : error("Assertion failed: ", "1==0")

That is, in the first call, the expression ``:(1==1.0)`` is spliced into
the test condition slot, while the value of ``string(:(1==1.0))`` is
spliced into the assertion message slot. The entire expression, thus
constructed, is placed into the syntax tree where the ``@assert`` macro
call occurs. Therefore, if the test expression is true when evaluated,
the entire expression evaluates to nothing, whereas if the test
expression is false, an error is raised indicating the asserted
expression that was false. Notice that it would not be possible to write
this as a function, since only the *value* of the condition and not the
expression that computed it would be available.

Hygiene
~~~~~~~

An issue that arises in more complex macros is that of
`hygiene <http://en.wikipedia.org/wiki/Hygienic_macro>`_. In short, Julia
must ensure that variables introduced and used by macros do not
accidentally clash with the variables used in code interpolated into
those macros. Another concern arises from the fact that a macro may be called
in a different module from where it was defined. In this case we need to
ensure that all global variables are resolved to the correct module.

To demonstrate these issues,
let us consider writing a ``@time`` macro that takes an expression as
its argument, records the time, evaluates the expression, records the
time again, prints the difference between the before and after times,
and then has the value of the expression as its final value.
The macro might look like this::

    macro time(ex)
      quote
        local t0 = time()
        local val = $ex
        local t1 = time()
        println("elapsed time: ", t1-t0, " seconds")
        val
      end
    end

Here, we want ``t0``, ``t1``, and ``val`` to be private temporary variables,
and we want ``time`` to refer to the ``time`` function in the standard library,
not to any ``time`` variable the user might have (the same applies to
``println``). Imagine the problems that could occur if the user expression
``ex`` also contained assignments to a variable called ``t0``, or defined
its own ``time`` variable. We might get errors, or mysteriously incorrect
behavior.

Julia's macro expander solves these problems in the following way. First,
variables within a macro result are classified as either local or global.
A variable is considered local if it is assigned to (and not declared
global), declared local, or used as a function argument name. Otherwise,
it is considered global. Local variables are then renamed to be unique
(using the ``gensym`` function, which generates new symbols), and global
variables are resolved within the macro definition environment. Therefore
both of the above concerns are handled; the macro's locals will not conflict
with any user variables, and ``time`` and ``println`` will refer to the
standard library definitions.

One problem remains however. Consider the following use of this macro::

    module MyModule
    import Base.@time

    time() = ... # compute something

    @time time()
    end

Here the user expression ``ex`` is a call to ``time``, but not the same
``time`` function that the macro uses. It clearly refers to ``MyModule.time``.
Therefore we must arrange for the code in ``ex`` to be resolved in the
macro call environment. This is done by "escaping" the expression with
the ``esc`` function::

    macro time(ex)
        ...
        local val = $(esc(ex))
        ...
    end

An expression wrapped in this manner is left alone by the macro expander
and simply pasted into the output verbatim. Therefore it will be
resolved in the macro call environment.

This escaping mechanism can be used to "violate" hygiene when necessary,
in order to introduce or manipulate user variables. For example, the
following macro sets ``x`` to zero in the call environment::

    macro zerox()
      esc(:(x = 0))
    end

    function foo()
      x = 1
      @zerox
      x  # is zero
    end

This kind of manipulation of variables should be used judiciously, but
is occasionally quite handy.

Non-Standard String Literals
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Recall from :ref:`Strings <man-non-standard-string-literals>` that
string literals prefixed by an identifier are called non-standard string
literals, and can have different semantics than un-prefixed string
literals. For example:

-  ``E"$100\n"`` interprets escape sequences but does no string
   interpolation
-  ``r"^\s*(?:#|$)"`` produces a regular expression object rather than a
   string
-  ``b"DATA\xff\u2200"`` is a byte array literal for
   ``[68,65,84,65,255,226,136,128]``.

Perhaps surprisingly, these behaviors are not hard-coded into the Julia
parser or compiler. Instead, they are custom behaviors provided by a
general mechanism that anyone can use: prefixed string literals are
parsed as calls to specially-named macros. For example, the regular
expression macros is just the following::

    macro r_str(p)
      Regex(p)
    end

That's all. This macro says that the literal contents of the string
literal ``r"^\s*(?:#|$)"`` should be passed to the ``@r_str`` macro and
the result of that expansion should be placed in the syntax tree where
the string literal occurs. In other words, the expression
``r"^\s*(?:#|$)"`` is equivalent to placing the following object
directly into the syntax tree::

    Regex("^\\s*(?:#|\$)")

Not only is the string literal form shorter and far more convenient, but
it is also more efficient: since the regular expression is compiled and
the ``Regex`` object is actually created *when the code is compiled*,
the compilation occurs only once, rather than every time the code is
executed. Consider if the regular expression occurs in a loop::

    for line = lines
      m = match(r"^\s*(?:#|$)", line)
      if m.match == nothing
        # non-comment
      else
        # comment
      end
    end

Since the regular expression ``r"^\s*(?:#|$)"`` is compiled and inserted
into the syntax tree when this code is parsed, the expression is only
compiled once instead of each time the loop is executed. In order to
accomplish this without macros, one would have to write this loop like
this::

    re = Regex("^\\s*(?:#|\$)")
    for line = lines
      m = match(re, line)
      if m.match == nothing
        # non-comment
      else
        # comment
      end
    end

Moreover, if the compiler could not determine that the regex object was
constant over all loops, certain optimizations might not be possible,
making this version still less efficient than the more convenient
literal form above. Of course, there are still situations where the
non-literal form is more convenient: if one needs to interpolate a
variable into the regular expression, has to take this more verbose
approach; in cases where the regular expression pattern itself is
dynamic, potentially changing upon each loop iteration, a new regular
expression object must be constructed on each iteration. The vast
majority of use cases, however, one does not construct regular
expressions dynamically, depending on run-time data. In this majority of
cases, the ability to write regular expressions as compile-time values
is, well, invaluable.

The mechanism for user-defined string literals is deeply, profoundly
powerful. Not only are Julia's non-standard literals implemented using
it, but also the command literal syntax (```echo "Hello, $person"```)
is implemented with the following innocuous-looking macro::

    macro cmd(str)
      :(cmd_gen($shell_parse(str)))
    end

Of course, a large amount of complexity is hidden in the functions used
in this macro definition, but they are just functions, written
entirely in Julia. You can read their source and see precisely what they
do — and all they do is construct expression objects to be inserted into
your program's syntax tree.

Reflection
----------

