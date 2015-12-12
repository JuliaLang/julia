.. _man-metaprogramming:

.. currentmodule:: Base

*****************
 Metaprogramming
*****************

The strongest legacy of Lisp in the Julia language is its metaprogramming
support. Like Lisp, Julia represents its own code as a data structure of
the language itself.
Since code is represented by objects that can be created and manipulated
from within the language, it is possible for a program to transform and
generate its own code. This allows sophisticated code generation
without extra build steps, and also allows true Lisp-style macros operating at
the level of `abstract syntax trees <https://en.wikipedia.org/wiki/Abstract_syntax_tree>`_.
In contrast, preprocessor "macro" systems, like that of C and C++, perform
textual manipulation and substitution before any actual parsing or
interpretation occurs. Because all data types and code in Julia
are represented by Julia data structures, powerful
`reflection <https://en.wikipedia.org/wiki/Reflection_%28computer_programming%29>`_
capabilities are available to explore the internals of a program and its types
just like any other data.

Program representation
----------------------

Every Julia program starts life as a string:

.. doctest::

    julia> prog = "1 + 1"
    "1 + 1"

**What happens next?**

The next step is to `parse <https://en.wikipedia.org/wiki/Parsing#Computer_languages>`_
each string into an object called an expression, represented by the Julia type
:obj:`Expr`:

.. doctest::

    julia> ex1 = parse(prog)
    :(1 + 1)

    julia> typeof(ex1)
    Expr

:obj:`Expr` objects contain three parts:

- a ``Symbol`` identifying the kind of expression. A symbol is an
  `interned string <https://en.wikipedia.org/wiki/String_interning>`_
  identifier (more discussion below).

.. doctest::

    julia> ex1.head
    :call

- the expression arguments, which may be symbols, other expressions, or literal values:

.. doctest::

    julia> ex1.args
    3-element Array{Any,1}:
      :+
     1
     1

- finally, the expression result type, which may be annotated by the user or inferred
  by the compiler (and may be ignored completely for the purposes of this chapter):

.. doctest::

    julia> ex1.typ
    Any

Expressions may also be constructed directly in
`prefix notation <https://en.wikipedia.org/wiki/Polish_notation>`_:

.. doctest::

    julia> ex2 = Expr(:call, :+, 1, 1)
    :(1 + 1)

The two expressions constructed above -- by parsing and by direct
construction -- are equivalent:

.. doctest::

    julia> ex1 == ex2
    true

**The key point here is that Julia code is internally represented
as a data structure that is accessible from the language itself.**

The :func:`dump` function provides indented and annotated display of :obj:`Expr`
objects:

.. doctest::

    julia> dump(ex2)
    Expr
      head: Symbol call
      args: Array(Any,(3,))
        1: Symbol +
        2: Int64 1
        3: Int64 1
      typ: Any

:obj:`Expr` objects may also be nested:

.. doctest::

    julia> ex3 = parse("(4 + 4) / 2")
    :((4 + 4) / 2)

Another way to view expressions is with Meta.show_sexpr, which displays the
`S-expression <https://en.wikipedia.org/wiki/S-expression>`_ form of a given
:obj:`Expr`, which may look very familiar to users of Lisp. Here's an example
illustrating the display on a nested :obj:`Expr`::

    julia> Meta.show_sexpr(ex3)
    (:call, :/, (:call, :+, 4, 4), 2)

Symbols
~~~~~~~

The ``:`` character has two syntactic purposes in Julia. The first form creates a
:obj:`Symbol`, an `interned string <https://en.wikipedia.org/wiki/String_interning>`_
used as one building-block of expressions:

.. doctest::

    julia> :foo
    :foo

    julia> typeof(ans)
    Symbol

:obj:`Symbol`\ s can also be created using :func:`symbol`, which takes any
number of arguments and creates a new symbol by concatenating their string
representations together:

.. doctest::

    julia> :foo == symbol("foo")
    true

    julia> symbol("func",10)
    :func10

    julia> symbol(:var,'_',"sym")
    :var_sym

In the context of an expression, symbols are used to indicate access to
variables; when an expression is evaluated, a symbol is replaced with
the value bound to that symbol in the appropriate :ref:`scope
<man-variables-and-scoping>`.

Sometimes extra parentheses around the argument to ``:`` are needed to avoid
ambiguity in parsing.:

.. doctest::

    julia> :(:)
    :(:)

    julia> :(::)
    :(::)

Expressions and evaluation
--------------------------

Quoting
~~~~~~~

The second syntactic purpose of the ``:`` character is to create expression
objects without using the explicit :obj:`Expr` constructor. This is referred
to as *quoting*. The ``:`` character, followed by paired parentheses around
a single statement of Julia code, produces an :obj:`Expr` object based on the
enclosed code. Here is example of the short form used to quote an arithmetic
expression:

.. doctest::

    julia> ex = :(a+b*c+1)
    :(a + b * c + 1)

    julia> typeof(ex)
    Expr

(to view the structure of this expression, try ``ex.head`` and ``ex.args``,
or use :func:`dump` as above)

Note that equivalent expressions may be constructed using :func:`parse` or
the direct :obj:`Expr` form:

.. doctest::

   julia>      :(a + b*c + 1)  ==
          parse("a + b*c + 1") ==
          Expr(:call, :+, :a, Expr(:call, :*, :b, :c), 1)
   true

Expressions provided by the parser generally only have symbols, other
expressions, and literal values as their args, whereas expressions
constructed by Julia code can have arbitrary run-time values
without literal forms as args. In this specific example, ``+`` and ``a``
are symbols, ``*(b,c)`` is a subexpression, and ``1`` is a literal
64-bit signed integer.

There is a second syntactic form of quoting for multiple expressions:
blocks of code enclosed in ``quote ... end``. Note that this form
introduces :obj:`QuoteNode` elements to the expression tree, which
must be considered when directly manipulating an expression tree
generated from ``quote`` blocks. For other purposes, ``:( ... )``
and ``quote .. end`` blocks are treated identically.

.. doctest::

    julia> ex = quote
               x = 1
               y = 2
               x + y
           end
    quote  # none, line 2:
        x = 1 # none, line 3:
        y = 2 # none, line 4:
        x + y
    end

    julia> typeof(ex)
    Expr

Interpolation
~~~~~~~~~~~~~

Direct construction of :obj:`Expr` objects with value arguments is
powerful, but :obj:`Expr` constructors can be tedious compared to "normal"
Julia syntax. As an alternative, Julia allows "splicing" or interpolation
of literals or expressions into quoted expressions. Interpolation is
indicated by the ``$`` prefix.

In this example, the literal value of ``a`` is interpolated:

.. doctest::

    julia> a = 1;

    julia> ex = :($a + b)
    :(1 + b)

Interpolating into an unquoted expression is not supported and will
cause a compile-time error:

.. doctest::

    julia> $a + b
    ERROR: unsupported or misplaced expression $

In this example, the tuple ``(1,2,3)`` is interpolated as an
expression into a conditional test:

.. doctest::

    julia> ex = :(a in $:((1,2,3)) )
    :($(Expr(:in, :a, :((1,2,3)))))

Interpolating symbols into a nested expression requires enclosing each
symbol in an enclosing quote block::

    julia> :( :a in $( :(:a + :b) ) )
                       ^^^^^^^^^^
                       quoted inner expression

The use of ``$`` for expression interpolation is intentionally reminiscent
of :ref:`string interpolation <man-string-interpolation>` and :ref:`command
interpolation <man-command-interpolation>`. Expression interpolation allows
convenient, readable programmatic construction of complex Julia expressions.

:func:`eval` and effects
~~~~~~~~~~~~~~~~~~~~~~~~

Given an expression object, one can cause Julia to evaluate (execute) it
at global scope using :func:`eval`:

.. doctest::

    julia> :(1 + 2)
    :(1 + 2)

    julia> eval(ans)
    3

    julia> ex = :(a + b)
    :(a + b)

    julia> eval(ex)
    ERROR: UndefVarError: b not defined

    julia> a = 1; b = 2;

    julia> eval(ex)
    3

Every :ref:`module <man-modules>` has its own :func:`eval` function that
evaluates expressions in its global scope.
Expressions passed to :func:`eval` are not limited to returning values
— they can also have side-effects that alter the state of the enclosing
module's environment:

.. doctest::

    julia> ex = :(x = 1)
    :(x = 1)

    julia> x
    ERROR: UndefVarError: x not defined

    julia> eval(ex)
    1

    julia> x
    1

Here, the evaluation of an expression object causes a value to be
assigned to the global variable ``x``.

Since expressions are just :obj:`Expr` objects which can be constructed
programmatically and then evaluated, it is possible to dynamically generate
arbitrary code which can then be run using :func:`eval`. Here is a simple example:

.. doctest::

    julia> a = 1;

    julia> ex = Expr(:call, :+, a, :b)
    :(1 + b)

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

Functions on :obj:`Expr`\ essions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As hinted above, one extremely useful feature of Julia is the capability to
generate and manipulate Julia code within Julia itself. We have already
seen one example of a function returning :obj:`Expr` objects: the :func:`parse`
function, which takes a string of Julia code and returns the corresponding
:obj:`Expr`. A function can also take one or more :obj:`Expr` objects as
arguments, and return another :obj:`Expr`. Here is a simple, motivating example::

   julia> function math_expr(op, op1, op2)
            expr = Expr(:call, op, op1, op2)
            return expr
          end

    julia>  ex = math_expr(:+, 1, Expr(:call, :*, 4, 5))
    :(1 + 4*5)

    julia> eval(ex)
    21

As another example, here is a function that doubles any numeric argument,
but leaves expressions alone::

    julia> function make_expr2(op, opr1, opr2)
             opr1f, opr2f = map(x -> isa(x, Number) ? 2*x : x, (opr1, opr2))
             retexpr = Expr(:call, op, opr1f, opr2f)

             return retexpr
       end
    make_expr2 (generic function with 1 method)

    julia> make_expr2(:+, 1, 2)
    :(2 + 4)

    julia> ex = make_expr2(:+, 1, Expr(:call, :*, 5, 8))
    :(2 + 5 * 8)

    julia> eval(ex)
    42

.. _man-macros:

Macros
------

Macros provide a method to include generated code in the final body
of a program. A macro maps a tuple of arguments to a returned
*expression*, and the resulting expression is compiled directly rather
than requiring a runtime :func:`eval` call. Macro arguments may include
expressions, literal values, and symbols.

Basics
~~~~~~

Here is an extraordinarily simple macro:

.. doctest::

    julia> macro sayhello()
               return :( println("Hello, world!") )
           end

Macros have a dedicated character in Julia's syntax: the ``@`` (at-sign),
followed by the unique name declared in a ``macro NAME ... end`` block.
In this example, the compiler will replace all instances of ``@sayhello``
with::

    :( println("Hello, world!") )

When ``@sayhello`` is given at the REPL, the expression executes
immediately, thus we only see the evaluation result::

    julia> @sayhello()
    "Hello, world!"

Now, consider a slightly more complex macro::

    julia> macro sayhello(name)
               return :( println("Hello, ", $name) )
           end

This macro takes one argument: ``name``. When ``@sayhello`` is
encountered, the quoted expression is *expanded* to interpolate
the value of the argument into the final expression::

    julia> @sayhello("human")
    Hello, human

We can view the quoted return expression using the function :func:`macroexpand`
(**important note:** this is an extremely useful tool for debugging macros)::

    julia> ex = macroexpand( :(@sayhello("human")) )
    :(println("Hello, ","human"))
                        ^^^^^^^
                        interpolated: now a literal string

    julia> typeof(ex)
    Expr

Hold up: why macros?
~~~~~~~~~~~~~~~~~~~~

We have already seen a function ``f(::Expr...) -> Expr`` in a previous section.
In fact, :func:`macroexpand` is also such a function. So, why do macros
exist?

Macros are necessary because they execute when code is parsed, therefore,
macros allow the programmer to generate and include fragments of customized
code *before* the full program is run. To illustrate the difference,
consider the following example::

    julia> macro twostep(arg)
               println("I execute at parse time. The argument is: ", arg)

               return :(println("I execute at runtime. The argument is: ", $arg))
           end

    julia> ex = macroexpand( :(@twostep :(1, 2, 3)) );
    I execute at parse time. The argument is: :((1,2,3))

The first call to :func:`println` is executed when :func:`macroexpand`
is called. The resulting expression contains *only* the second ``println``::

    julia> typeof(ex)
    Expr

    julia> ex
    :(println("I execute at runtime. The argument is: ",$(Expr(:copyast, :(:((1,2,3)))))))

    julia> eval(ex)
    I execute at runtime. The argument is: (1,2,3)

Macro invocation
~~~~~~~~~~~~~~~~

Macros are invoked with the following general syntax::

    @name expr1 expr2 ...
    @name(expr1, expr2, ...)

Note the distinguishing ``@`` before the macro name and the lack of
commas between the argument expressions in the first form, and the
lack of whitespace after ``@name`` in the second form. The two styles
should not be mixed. For example, the following syntax is different
from the examples above; it passes the tuple ``(expr1, expr2, ...)`` as
one argument to the macro::

    @name (expr1, expr2, ...)

It is important to emphasize that macros receive their arguments as
expressions, literals, or symbols. One way to explore macro arguments
is to call the :func:`show` function within the macro body::

    julia> macro showarg(x)
       show(x)
       # ... remainder of macro, returning an expression
    end


    julia> @showarg(a)
    (:a,)

    julia> @showarg(1+1)
    :(1 + 1)

    julia> @showarg(println("Yo!"))
    :(println("Yo!"))


Building an advanced macro
~~~~~~~~~~~~~~~~~~~~~~~~~~

Here is a simplified definition of Julia's :obj:`@assert` macro::

    macro assert(ex)
        return :( $ex ? nothing : throw(AssertionError($(string(ex)))) )
    end

This macro can be used like this:

.. doctest::

    julia> @assert 1==1.0

    julia> @assert 1==0
    ERROR: AssertionError: 1 == 0

In place of the written syntax, the macro call is expanded at parse time to
its returned result. This is equivalent to writing::

    1==1.0 ? nothing : throw(AssertionError("1==1.0"))
    1==0 ? nothing : throw(AssertionError("1==0"))

That is, in the first call, the expression ``:(1==1.0)`` is spliced into
the test condition slot, while the value of ``string(:(1==1.0))`` is
spliced into the assertion message slot. The entire expression, thus
constructed, is placed into the syntax tree where the :obj:`@assert` macro
call occurs. Then at execution time, if the test expression evaluates to
true, then ``nothing`` is returned, whereas if the test is false, an error
is raised indicating the asserted expression that was false. Notice that
it would not be possible to write this as a function, since only the
*value* of the condition is available and it would be impossible to
display the expression that computed it in the error message.

The actual definition of :obj:`@assert` in the standard library is more
complicated. It allows the user to optionally specify their own error
message, instead of just printing the failed expression. Just like in
functions with a variable number of arguments, this is specified with an
ellipses following the last argument::

    macro assert(ex, msgs...)
        msg_body = isempty(msgs) ? ex : msgs[1]
        msg = string(msg_body)
        return :($ex ? nothing : throw(AssertionError($msg)))
    end

Now :obj:`@assert` has two modes of operation, depending upon the number of
arguments it receives! If there's only one argument, the tuple of expressions
captured by ``msgs`` will be empty and it will behave the same as the simpler
definition above. But now if the user specifies a second argument, it is
printed in the message body instead of the failing expression. You can inspect
the result of a macro expansion with the aptly named :func:`macroexpand`
function:

.. doctest::

    julia> macroexpand(:(@assert a==b))
    :(if a == b
            nothing
        else
            Base.throw(Base.Main.Base.AssertionError("a == b"))
        end)

    julia> macroexpand(:(@assert a==b "a should equal b!"))
    :(if a == b
            nothing
        else
            Base.throw(Base.Main.Base.AssertionError("a should equal b!"))
        end)

There is yet another case that the actual :obj:`@assert` macro handles: what
if, in addition to printing "a should equal b," we wanted to print their
values? One might naively try to use string interpolation in the custom
message, e.g., ``@assert a==b "a ($a) should equal b ($b)!"``, but this
won't work as expected with the above macro. Can you see why? Recall
from :ref:`string interpolation <man-string-interpolation>` that an
interpolated string is rewritten to a call to :func:`string`.
Compare:

.. doctest::

    julia> typeof(:("a should equal b"))
    String

    julia> typeof(:("a ($a) should equal b ($b)!"))
    Expr

    julia> dump(:("a ($a) should equal b ($b)!"))
    Expr
      head: Symbol string
      args: Array(Any,(5,))
        1: String "a ("
        2: Symbol a
        3: String ") should equal b ("
        4: Symbol b
        5: String ")!"
      typ: Any

So now instead of getting a plain string in ``msg_body``, the macro is
receiving a full expression that will need to be evaluated in order to
display as expected. This can be spliced directly into the returned expression
as an argument to the :func:`string` call; see `error.jl
<https://github.com/JuliaLang/julia/blob/master/base/error.jl>`_ for
the complete implementation.

The :obj:`@assert` macro makes great use of splicing into quoted expressions
to simplify the manipulation of expressions inside the macro body.


Hygiene
~~~~~~~

An issue that arises in more complex macros is that of
`hygiene <https://en.wikipedia.org/wiki/Hygienic_macro>`_. In short, macros must
ensure that the variables they introduce in their returned expressions do not
accidentally clash with existing variables in the surrounding code they expand
into. Conversely, the expressions that are passed into a macro as arguments are
often *expected* to evaluate in the context of the surrounding code,
interacting with and modifying the existing variables. Another concern arises
from the fact that a macro may be called in a different module from where it
was defined. In this case we need to ensure that all global variables are
resolved to the correct module. Julia already has a major advantage over
languages with textual macro expansion (like C) in that it only needs to
consider the returned expression. All the other variables (such as ``msg`` in
:obj:`@assert` above) follow the :ref:`normal scoping block behavior
<man-variables-and-scoping>`.

To demonstrate these issues,
let us consider writing a ``@time`` macro that takes an expression as
its argument, records the time, evaluates the expression, records the
time again, prints the difference between the before and after times,
and then has the value of the expression as its final value.
The macro might look like this::

    macro time(ex)
      return quote
        local t0 = time()
        local val = $ex
        local t1 = time()
        println("elapsed time: ", t1-t0, " seconds")
        val
      end
    end

Here, we want ``t0``, ``t1``, and ``val`` to be private temporary variables,
and we want ``time`` to refer to the :func:`time` function in the standard library,
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
(using the :func:`gensym` function, which generates new symbols), and global
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
:func:`esc`::

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
      return esc(:(x = 0))
    end

    function foo()
      x = 1
      @zerox
      x  # is zero
    end

This kind of manipulation of variables should be used judiciously, but
is occasionally quite handy.

Code Generation
---------------

When a significant amount of repetitive boilerplate code is required, it
is common to generate it programmatically to avoid redundancy. In most
languages, this requires an extra build step, and a separate program to
generate the repetitive code. In Julia, expression interpolation and
:func:`eval` allow such code generation to take place in the normal course of
program execution. For example, the following code defines a series of
operators on three arguments in terms of their 2-argument forms::

    for op = (:+, :*, :&, :|, :$)
      eval(quote
        ($op)(a,b,c) = ($op)(($op)(a,b),c)
      end)
    end

In this manner, Julia acts as its own `preprocessor
<https://en.wikipedia.org/wiki/Preprocessor>`_, and allows code
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

The :obj:`@eval` macro rewrites this call to be precisely equivalent to the
above longer versions. For longer blocks of generated code, the
expression argument given to :obj:`@eval` can be a block::

    @eval begin
      # multiple lines
    end

.. _man-non-standard-string-literals2:

Non-Standard String Literals
----------------------------

Recall from :ref:`Strings <man-non-standard-string-literals>` that
string literals prefixed by an identifier are called non-standard string
literals, and can have different semantics than un-prefixed string
literals. For example:

-  ``r"^\s*(?:#|$)"`` produces a regular expression object rather than a
   string
-  ``b"DATA\xff\u2200"`` is a byte array literal for
   ``[68,65,84,65,255,226,136,128]``.

Perhaps surprisingly, these behaviors are not hard-coded into the Julia
parser or compiler. Instead, they are custom behaviors provided by a
general mechanism that anyone can use: prefixed string literals are
parsed as calls to specially-named macros. For example, the regular
expression macro is just the following::

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
the :obj:`Regex` object is actually created *when the code is compiled*,
the compilation occurs only once, rather than every time the code is
executed. Consider if the regular expression occurs in a loop::

    for line = lines
      m = match(r"^\s*(?:#|$)", line)
      if m == nothing
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
      if m == nothing
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
variable into the regular expression, one must take this more verbose
approach; in cases where the regular expression pattern itself is
dynamic, potentially changing upon each loop iteration, a new regular
expression object must be constructed on each iteration. In the vast
majority of use cases, however, regular expressions are not constructed
based on run-time data. In this majority of cases, the ability to write
regular expressions as compile-time values is invaluable.

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
do — and all they do is construct expression objects to be inserted into
your program's syntax tree.

Generated functions
-------------------

A very special macro is ``@generated``, which allows you to define so-called
*generated functions*. These have the capability to generate specialized
code depending on the types of their arguments with more flexibility and/or
less code than what can be achieved with multiple dispatch. While macros
work with expressions at parsing-time and cannot access the types of their
inputs, a generated function gets expanded at a time when the types of
the arguments are known, but the function is not yet compiled.

Instead of performing some calculation or action, a generated function
declaration returns a quoted expression which then forms the body for the
method corresponding to the types of the arguments. When called, the body
expression is compiled (or fetched from a cache, on subsequent calls) and
only the returned expression - not the code that generated it - is evaluated.
Thus, generated functions provide a flexible framework to move work from
run-time to compile-time.

When defining generated functions, there are three main differences to
ordinary functions:

1. You annotate the function declaration with the ``@generated`` macro.
   This adds some information to the AST that lets the compiler know that
   this is a generated function.

2. In the body of the generated function you only have access to the
   *types* of the arguments, not their values.

3. Instead of calculating something or performing some action, you return
   a *quoted expression* which, when evaluated, does what you want.

It's easiest to illustrate this with an example. We can declare a generated
function ``foo`` as

.. doctest::

    julia> @generated function foo(x)
               println(x)
               return :(x*x)
           end
    foo (generic function with 1 method)

Note that the body returns a quoted expression, namely ``:(x*x)``, rather
than just the value of ``x*x``.

From the caller's perspective, they are very similar to regular functions;
in fact, you don't have to know if you're calling a regular or generated
function - the syntax and result of the call is just the same.
Let's see how ``foo`` behaves:

.. doctest::

    # note: output is from println() statement in the body
    julia> x = foo(2);
    Int64

    julia> x           # now we print x
    4

    julia> y = foo("bar");
    String

    julia> y
    "barbar"

So, we see that in the body of the generated function, ``x`` is the
*type* of the passed argument, and the value returned by the generated
function, is the result of evaluating the quoted expression we returned
from the definition, now with the *value* of ``x``.

What happens if we evaluate ``foo`` again with a type that we have already
used?

.. doctest::

    julia> foo(4)
    16

Note that there is no printout of :obj:`Int64`. The body of the generated
function is only executed *once* (not entirely true, see note below) when
the method for that specific set of argument types is compiled. After that,
the expression returned from the generated function on the first invocation
is re-used as the method body.

The reason for the disclaimer above is that the number of times a generated
function is generated is really an implementation detail; it *might* be only
once, but it *might* also be more often. As a consequence, you should
*never* write a generated function with side effects - when, and how often,
the side effects occur is undefined. (This is true for macros too - and just
like for macros, the use of :func:`eval` in a generated function is a sign that
you're doing something the wrong way.)

The example generated function ``foo`` above did not do anything a normal
function ``foo(x)=x*x`` could not do, except printing the type on the
first invocation (and incurring a higher compile-time cost). However, the
power of a generated function lies in its ability to compute different quoted
expression depending on the types passed to it:

.. doctest::

   julia> @generated function bar(x)
              if x <: Integer
                  return :(x^2)
              else
                  return :(x)
              end
          end
   bar (generic function with 1 method)

   julia> bar(4)
   16
   julia> bar("baz")
   "baz"

(although of course this contrived example is easily implemented using
multiple dispatch...)

We can, of course, abuse this to produce some interesting behavior:

.. doctest::

   julia> @generated function baz(x)
              if rand() < .9
                  return :(x^2)
              else
                  return :("boo!")
              end
          end
   baz (generic function with 1 method)


Since the body of the generated function is non-deterministic, its behavior
is undefined; the expression returned on the *first* invocation will be
used for *all* subsequent invocations with the same type (again, with the
exception covered by the disclaimer above). When we call the generated
function with ``x`` of a new type, :func:`rand` will be called again to
see which method body to use for the new type. In this case, for one
*type* out of ten, ``baz(x)`` will return the string ``"boo!"``.

*Don't copy these examples!*

These examples are hopefully helpful to illustrate how generated functions
work, both in the definition end and at the call site; however, *don't
copy them*, for the following reasons:

* the ``foo`` function has side-effects, and it is undefined exactly when,
  how often or how many times these side-effects will occur
* the ``bar`` function solves a problem that is better solved with multiple
  dispatch - defining ``bar(x) = x`` and ``bar(x::Integer) = x^2`` will do
  the same thing, but it is both simpler and faster.
* the ``baz`` function is pathologically insane

Instead, now that we have a better understanding for how generated functions
work, let's use them to build some more advanced functionality...

An advanced example
~~~~~~~~~~~~~~~~~~~

Julia's base library has a :func:`sub2ind` function to calculate a
linear index into an n-dimensional array, based on a set of n multilinear
indices - in other words, to calculate the index ``i`` that can be used to
index into an array ``A`` using ``A[i]``, instead of ``A[x,y,z,...]``. One
possible implementation is the following::

    function sub2ind_loop{N}(dims::NTuple{N}, I::Integer...)
        ind = I[N] - 1
        for i = N-1:-1:1
            ind = I[i]-1 + dims[i]*ind
        end
        return ind + 1
    end

The same thing can be done using recursion::

    sub2ind_rec(dims::Tuple{}) = 1
    sub2ind_rec(dims::Tuple{},i1::Integer, I::Integer...) =
        i1==1 ? sub2ind_rec(dims,I...) : throw(BoundsError())
    sub2ind_rec(dims::Tuple{Integer,Vararg{Integer}}, i1::Integer) = i1
    sub2ind_rec(dims::Tuple{Integer,Vararg{Integer}}, i1::Integer, I::Integer...) =
        i1 + dims[1]*(sub2ind_rec(tail(dims),I...)-1)

Both these implementations, although different, do essentially the same
thing: a runtime loop over the dimensions of the array, collecting the
offset in each dimension into the final index.

However, all the information we need for the loop is embedded in the type
information of the arguments. Thus, we can utilize generated functions to
move the iteration to compile-time; in compiler parlance, we use generated
functions to manually unroll the loop. The body becomes almost identical,
but instead of calculating the linear index, we build up an *expression*
that calculates the index:

.. code-block:: julia

    @generated function sub2ind_gen{N}(dims::NTuple{N}, I::Integer...)
        ex = :(I[$N] - 1)
        for i = N-1:-1:1
            ex = :(I[$i] - 1 + dims[$i]*$ex)
        end
        return :($ex + 1)
    end

**What code will this generate?**

An easy way to find out, is to extract the body into another (regular)
function:

.. doctest::

   julia> @generated function sub2ind_gen{N}(dims::NTuple{N}, I::Integer...)
              sub2ind_gen_impl(dims, I...)
          end
   sub2ind_gen (generic function with 1 method)

   julia> function sub2ind_gen_impl{N}(dims::Type{NTuple{N}}, I...)
              length(I) == N || return :(error("partial indexing is unsupported"))
              ex = :(I[$N] - 1)
              for i = N-1:-1:1
                  ex = :(I[$i] - 1 + dims[$i]*$ex)
              end
              return :($ex + 1)
          end
   sub2ind_gen_impl (generic function with 1 method)

We can now execute ``sub2ind_gen_impl`` and examine the expression it
returns:

.. doctest::

   julia> sub2ind_gen_impl(Tuple{Int,Int}, Int, Int)
   :(((I[1] - 1) + dims[1] * (I[2] - 1)) + 1)

So, the method body that will be used here doesn't include a loop at all
- just indexing into the two tuples, multiplication and addition/subtraction.
All the looping is performed compile-time, and we avoid looping during
execution entirely. Thus, we only loop *once per type*, in this case once
per ``N`` (except in edge cases where the function is generated more than
once - see disclaimer above).
