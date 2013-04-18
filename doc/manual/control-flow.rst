.. _man-control-flow:

**************
 Control Flow  
**************

Julia provides a variety of control flow constructs:

-  :ref:`man-compound-expressions`: ``begin`` and ``(;)``.
-  :ref:`man-conditional-evaluation`:
   ``if``-``elseif``-``else`` and ``?:`` (ternary operator).
-  :ref:`man-short-circuit-evaluation`:
   ``&&``, ``||`` and chained comparisons.
-  :ref:`man-loops`: ``while`` and ``for``.
-  :ref:`man-exception-handling`:
   ``try``-``catch``, ``error`` and ``throw``.
-  :ref:`man-tasks`: ``yieldto``.

The first five control flow mechanisms are standard to high-level
programming languages. Tasks are not so standard: they provide non-local
control flow, making it possible to switch between temporarily-suspended
computations. This is a powerful construct: both exception handling and
cooperative multitasking are implemented in Julia using tasks. Everyday
programming requires no direct usage of tasks, but certain problems can
be solved much more easily by using tasks.

.. _man-compound-expressions:

Compound Expressions
--------------------

Sometimes it is convenient to have a single expression which evaluates
several subexpressions in order, returning the value of the last
subexpression as its value. There are two Julia constructs that
accomplish this: ``begin`` blocks and ``(;)`` chains. The value of both
compound expression constructs is that of the last subexpression. Here's
an example of a ``begin`` block::

    julia> z = begin
             x = 1
             y = 2
             x + y
           end
    3

Since these are fairly small, simple expressions, they could easily be
placed onto a single line, which is where the ``(;)`` chain syntax comes
in handy::

    julia> z = (x = 1; y = 2; x + y)
    3

This syntax is particularly useful with the terse single-line function
definition form introduced in :ref:`man-functions`. Although it
is typical, there is no requirement that ``begin`` blocks be multiline
or that ``(;)`` chains be single-line::

    julia> begin x = 1; y = 2; x + y end
    3

    julia> (x = 1;
            y = 2;
            x + y)
    3

.. _man-conditional-evaluation:

Conditional Evaluation
----------------------

Conditional evaluation allows portions of code to be evaluated or not
evaluated depending on the value of a boolean expression. Here is the
anatomy of the ``if``-``elseif``-``else`` conditional syntax::

    if x < y
      println("x is less than y")
    elseif x > y
      println("x is greater than y")
    else
      println("x is equal to y")
    end

If the condition expression ``x < y`` is ``true``, then the corresponding block
is evaluated; otherwise the condition expression ``x > y`` is evaluated, and if
it is ``true``, the corresponding block is evaluated; if neither expression is
true, the ``else`` block is evaluated. Here it is in action::

    julia> function test(x, y)
             if x < y
               println("x is less than y")
             elseif x > y
               println("x is greater than y")
             else
               println("x is equal to y")
             end
           end

    julia> test(1, 2)
    x is less than y

    julia> test(2, 1)
    x is greater than y

    julia> test(1, 1)
    x is equal to y

The ``elseif`` and ``else`` blocks are optional, and as many ``elseif``
blocks as desired can be used. The condition expressions in the
``if``-``elseif``-``else`` construct are evaluated until the first one
evaluates to ``true``, after which the associated block is evaluated,
and no further condition expressions or blocks are evaluated.

Unlike C, MATLAB, Perl, Python, and Ruby — but like Java, and a few
other stricter, typed languages — it is an error if the value of a
conditional expression is anything but ``true`` or ``false``::

    julia> if 1
             println("true")
           end
    type error: lambda: in if, expected Bool, got Int64

This error indicates that the conditional was of the wrong type:
``Int64`` rather than the required ``Bool``.

The so-called "ternary operator", ``?:``, is closely related to the
``if``-``elseif``-``else`` syntax, but is used where a conditional
choice between single expression values is required, as opposed to
conditional execution of longer blocks of code. It gets its name from
being the only operator in most languages taking three operands::

    a ? b : c

The expression ``a``, before the ``?``, is a condition expression, and
the ternary operation evaluates the expression ``b``, before the ``:``,
if the condition ``a`` is ``true`` or the expression ``c``, after the
``:``, if it is ``false``.

The easiest way to understand this behavior is to see an example. In the
previous example, the ``println`` call is shared by all three branches:
the only real choice is which literal string to print. This could be
written more concisely using the ternary operator. For the sake of
clarity, let's try a two-way version first::

    julia> x = 1; y = 2;

    julia> println(x < y ? "less than" : "not less than")
    less than

    julia> x = 1; y = 0;

    julia> println(x < y ? "less than" : "not less than")
    not less than

If the expression ``x < y`` is true, the entire ternary operator
expression evaluates to the string ``"less than"`` and otherwise it
evaluates to the string ``"not less than"``. The original three-way
example requires chaining multiple uses of the ternary operator
together::

    julia> test(x, y) = println(x < y ? "x is less than y"    :
                                x > y ? "x is greater than y" : "x is equal to y")

    julia> test(1, 2)
    x is less than y

    julia> test(2, 1)
    x is greater than y

    julia> test(1, 1)
    x is equal to y

To facilitate chaining, the operator associates from right to left.

It is significant that like ``if``-``elseif``-``else``, the expressions
before and after the ``:`` are only evaluated if the condition
expression evaluates to ``true`` or ``false``, respectively::

    v(x) = (println(x); x)

    julia> 1 < 2 ? v("yes") : v("no")
    yes
    "yes"

    julia> 1 > 2 ? v("yes") : v("no")
    no
    "no"

.. _man-short-circuit-evaluation:

Short-Circuit Evaluation
------------------------

Short-circuit evaluation is quite similar to conditional evaluation. The
behavior is found in most imperative programming languages having the
``&&`` and ``||`` boolean operators: in a series of boolean expressions
connected by these operators, only the minimum number of expressions are
evaluated as are necessary to determine the final boolean value of the
entire chain. Explicitly, this means that:

-  In the expression ``a && b``, the subexpression ``b`` is only
   evaluated if ``a`` evaluates to ``true``.
-  In the expression ``a || b``, the subexpression ``b`` is only
   evaluated if ``a`` evaluates to ``false``.

The reasoning is that ``a && b`` must be ``false`` if ``a`` is
``false``, regardless of the value of ``b``, and likewise, the value of
``a || b`` must be true if ``a`` is ``true``, regardless of the value of
``b``. Both ``&&`` and ``||`` associate to the right, but ``&&`` has
higher precedence than than ``||`` does. It's easy to experiment with
this behavior::

    t(x) = (println(x); true)
    f(x) = (println(x); false)

    julia> t(1) && t(2)
    1
    2
    true

    julia> t(1) && f(2)
    1
    2
    false

    julia> f(1) && t(2)
    1
    false

    julia> f(1) && f(2)
    1
    false

    julia> t(1) || t(2)
    1
    true

    julia> t(1) || f(2)
    1
    true

    julia> f(1) || t(2)
    1
    2
    true

    julia> f(1) || f(2)
    1
    2
    false

You can easily experiment in the same way with the associativity and
precedence of various combinations of ``&&`` and ``||`` operators.

Boolean operations *without* short-circuit evaluation can be done with the
bitwise boolean operators introduced in :ref:`man-mathematical-operations`:
``&`` and ``|``. These are normal functions, which happen to support
infix operator syntax, but always evaluate their arguments::

    julia> f(1) & t(2)
    1
    2
    false

    julia> t(1) | t(2)
    1
    2
    true

Just like condition expressions used in ``if``, ``elseif`` or the
ternary operator, the operands of ``&&`` or ``||`` must be boolean
values (``true`` or ``false``). Using a non-boolean value is an error::

    julia> 1 && 2
    type error: lambda: in if, expected Bool, got Int64

.. _man-loops:

Repeated Evaluation: Loops
--------------------------

There are two constructs for repeated evaluation of expressions: the
``while`` loop and the ``for`` loop. Here is an example of a ``while``
loop::

    julia> i = 1;

    julia> while i <= 5
             println(i)
             i += 1
           end
    1
    2
    3
    4
    5

The ``while`` loop evaluates the condition expression (``i < n`` in this
case), and as long it remains ``true``, keeps also evaluating the body
of the ``while`` loop. If the condition expression is ``false`` when the
``while`` loop is first reached, the body is never evaluated.

The ``for`` loop makes common repeated evaluation idioms easier to
write. Since counting up and down like the above ``while`` loop does is
so common, it can be expressed more concisely with a ``for`` loop::

    julia> for i = 1:5
             println(i)
           end
    1
    2
    3
    4
    5

Here the ``1:5`` is a ``Range`` object, representing the sequence of
numbers 1, 2, 3, 4, 5. The ``for`` loop iterates through these values,
assigning each one in turn to the variable ``i``. One rather important
distinction between the previous ``while`` loop form and the ``for``
loop form is the scope during which the variable is visible. If the
variable ``i`` has not been introduced in an other scope, in the ``for``
loop form, it is visible only inside of the ``for`` loop, and not
afterwards. You'll either need a new interactive session instance or a
different variable name to test this::

    julia> for j = 1:5
             println(j)
           end
    1
    2
    3
    4
    5

    julia> j
    j not defined

See :ref:`man-variables-and-scoping` for a detailed
explanation of variable scope and how it works in Julia.

In general, the ``for`` loop construct can iterate over any container.
In these cases, the alternative (but fully equivalent) keyword ``in`` is
typically used instead of ``=``, since it makes the code read more
clearly::

    julia> for i in [1,4,0]
             println(i)
           end
    1
    4
    0

    julia> for s in ["foo","bar","baz"]
             println(s)
           end
    foo
    bar
    baz

Various types of iterable containers will be introduced and discussed in
later sections of the manual (see, e.g., :ref:`man-arrays`).

It is sometimes convenient to terminate the repetition of a ``while``
before the test condition is falsified or stop iterating in a ``for``
loop before the end of the iterable object is reached. This can be
accomplished with the ``break`` keyword::

    julia> i = 1;

    julia> while true
             println(i)
             if i >= 5
               break
             end
             i += 1
           end
    1
    2
    3
    4
    5

    julia> for i = 1:1000
             println(i)
             if i >= 5
               break
             end
           end
    1
    2
    3
    4
    5

The above ``while`` loop would never terminate on its own, and the
``for`` loop would iterate up to 1000. These loops are both exited early
by using the ``break`` keyword.

In other circumstances, it is handy to be able to stop an iteration and
move on to the next one immediately. The ``continue`` keyword
accomplishes this::

    julia> for i = 1:10
             if i % 3 != 0
               continue
             end
             println(i)
           end
    3
    6
    9

This is a somewhat contrived example since we could produce the same
behavior more clearly by negating the condition and placing the
``println`` call inside the ``if`` block. In realistic usage there is
more code to be evaluated after the ``continue``, and often there are
multiple points from which one calls ``continue``.

Multiple nested ``for`` loops can be combined into a single outer loop,
forming the cartesian product of its iterables::

    julia> for i = 1:2, j = 3:4
             println((i, j))
           end
    (1,3)
    (1,4)
    (2,3)
    (2,4)

.. _man-exception-handling:

Exception Handling
------------------

When an unexpected condition occurs, a function may be unable to return
a reasonable value to its caller. In such cases, it may be best for the
exceptional condition to either terminate the program, printing a
diagnostic error message, or if the programmer has provided code to
handle such exceptional circumstances, allow that code to take the
appropriate action.

Built-in ``Exception``\ s
~~~~~~~~~~~~~~~~~~~~~~~~~

``Exception``\ s are thrown when an unexpected condition has occurred. The
built-in ``Exception``\ s listed below all interrupt the normal flow of control.

======================
``Exception``         
----------------------  
``ArgumentError``
``BoundsError``
``DivideError``
``DomainError``
``EOFError``
``ErrorException``
``InexactError``
``InterruptException``
``KeyError``
``LoadError``
``MemoryError``
``MethodError``
``OverflowError``
``ParseError``
``SystemError``
``TypeError``
``UndefRefError``
======================

For example, the ``sqrt`` function returns a ``DomainError()`` if applied to a
negative real value::

    julia> sqrt(-1)
    ERROR: DomainError()
     in sqrt at math.jl:117

The ``throw`` function
~~~~~~~~~~~~~~~~~~~~~~

Exceptions can be created explicitly with ``throw``. For example, a function
defined only for nonnegative numbers could be written to ``throw`` a ``DomainError``
if the argument is negative. ::

    julia> f(x) = x>=0 ? exp(-x) : throw(DomainError())
    # methods for generic function f
    f(x) at none:1
    
    julia> f(1)
    0.36787944117144233
    
    julia> f(-1)
    ERROR: DomainError()
     in f at none:1

Note that the ``DomainError`` should be called with the parentheses lest the
throw statement return something other than an exception. ::

    julia> typeof(DomainError()) <: Exception
    true
    
    julia> typeof(DomainError) <: Exception
    false

Errors
~~~~~~

The ``error`` function is used to produce an ``ErrorException`` that
interrupts the normal flow of control.

Suppose we want to stop execution immediately if the square root of a
negative number is taken. To do this, we can define a fussy version of
the ``sqrt`` function that raises an error if its argument is negative::

    fussy_sqrt(x) = x >= 0 ? sqrt(x) : error("negative x not allowed")

    julia> fussy_sqrt(2)
    1.4142135623730951

    julia> fussy_sqrt(-1)
    negative x not allowed

If ``fussy_sqrt`` is called with a negative value from another function,
instead of trying to continue execution of the calling function, it
returns immediately, displaying the error message in the interactive
session::

    function verbose_fussy_sqrt(x)
      println("before fussy_sqrt")
      r = fussy_sqrt(x)
      println("after fussy_sqrt")
      return r
    end

    julia> verbose_fussy_sqrt(2)
    before fussy_sqrt
    after fussy_sqrt
    1.4142135623730951

    julia> verbose_fussy_sqrt(-1)
    before fussy_sqrt
    negative x not allowed

Warnings and informational messages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Julia also provides other functions that write messages to the standard error
I/O, but do not throw any ``Exception``\ s and hence do not interrupt
execution.::

    julia> info("Hi"); 1+1
    MESSAGE: Hi
    2
    
    julia> warn("Hi"); 1+1
    WARNING: Hi
    2
    
    julia> error("Hi"); 1+1
    ERROR: Hi
     in error at error.jl:21

The ``try/catch`` statement
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``try/catch`` statement allows for ``Exception``\ s to be tested for. For
example, a customized square root function can be written to automatically
call either the real or complex square root method on demand using
``Exception`` \s ::

    julia> f(x) = try
             sqrt(x)
           catch
             sqrt(complex(x, 0))
           end
    # methods for generic function f
    f(x) at none:1
    
    julia> f(1)
    1.0
    
    julia> f(-1)
    0.0 + 1.0im

``try/catch`` statements also allow the ``Exception`` to be saved in a
variable. In this contrived example, the following example calculates the
square root of the second element of ``x`` if ``x`` is indexable, otherwise
assumes ``x`` is a real number and returns its square root::

    julia> sqrt_second(x) = try
             sqrt(x[2])
           catch y
             if y == DomainError() 
               sqrt(complex(x[2], 0))
             elseif y == BoundsError()
               sqrt(x)
             end  
           end

    # methods for generic function sqrt_second
    sqrt_second(x) at none:1
    
    julia> sqrt_second([1 4])
    2.0
    
    julia> sqrt_second([1 -4])
    0.0 + 2.0im
    
    julia> sqrt_second(9)
    3.0
    
    julia> sqrt_second(-9)
    ERROR: DomainError()
     in sqrt at math.jl:117
     in sqrt_second at none:7
     
This next example, shows an example when ``DivideByZeroError`` is thrown::

    julia> div(1,0)
    error: integer divide by zero

    julia> try
             div(1,0)
           catch x
             println(typeof(x))
           end
    DivideByZeroError

``DivideByZeroError`` is a concrete subtype of ``Exception``, thrown to
indicate that an integer division by zero has occurred. Floating-point
functions, on the other hand, can simply return ``NaN`` rather than
throwing an exception.

The power of the ``try/catch`` construct lies in the ability to unwind a deeply
nested computation immediately to a much higher level in the stack of calling
functions. There are situations where no error has occurred, but the ability to
unwind the stack and pass a value to a higher level is desirable. Julia
provides the ``rethrow``, ``backtrace`` and ``catch_backtrace`` functions for
more advanced error handling.

finally Clauses
~~~~~~~~~~~~~~~

In code that performs state changes or uses resources like files, there is
typically clean-up work (such as closing files) that needs to be done when the
code is finished. Exceptions potentially complicate this task, since they can
cause a block of code to exit before reaching its normal end. The ``finally``
keyword provides a way to run some code when a given block of code exits,
regardless of how it exits.

For example, here is how we can guarantee that an opened file is closed::

    f = open("file")
    try
        # operate on file f
    finally
        close(f)
    end

When control leaves the ``try`` block (for example due to a ``return``, or
just finishing normally), ``close(f)`` will be executed. If
the ``try`` block exits due to an exception, the exception will continue
propagating. A ``catch`` block may be combined with ``try`` and ``finally``
as well. In this case the ``finally`` block will run after ``catch`` has
handled the error.

.. _man-tasks:

Tasks (aka Coroutines)
----------------------

Tasks are a control flow feature that allows computations to be
suspended and resumed in a flexible manner. This feature is sometimes
called by other names, such as symmetric coroutines, lightweight
threads, cooperative multitasking, or one-shot continuations.

When a piece of computing work (in practice, executing a particular
function) is designated as a ``Task``, it becomes possible to interrupt
it by switching to another ``Task``. The original ``Task`` can later be
resumed, at which point it will pick up right where it left off. At
first, this may seem similar to a function call. However there are two
key differences. First, switching tasks does not use any space, so any
number of task switches can occur without consuming the call stack.
Second, switching among tasks can occur in any order, unlike function calls,
where the called function must finish executing before control returns
to the calling function.

This kind of control flow can make it much easier to solve certain
problems. In some problems, the various pieces of required work are not
naturally related by function calls; there is no obvious "caller" or
"callee" among the jobs that need to be done. An example is the
producer-consumer problem, where one complex procedure is generating
values and another complex procedure is consuming them. The consumer
cannot simply call a producer function to get a value, because the
producer may have more values to generate and so might not yet be ready
to return. With tasks, the producer and consumer can both run as long as
they need to, passing values back and forth as necessary.

Julia provides the functions ``produce`` and ``consume`` for solving
this problem. A producer is a function that calls ``produce`` on each
value it needs to produce::

    function producer()
      produce("start")
      for n=1:4
        produce(2n)
      end
      produce("stop")
    end

To consume values, first the producer is wrapped in a ``Task``, then
``consume`` is called repeatedly on that object::

    julia> p = Task(producer)
    Task

    julia> consume(p)
    "start"

    julia> consume(p)
    2

    julia> consume(p)
    4

    julia> consume(p)
    6

    julia> consume(p)
    8

    julia> consume(p)
    "stop"

One way to think of this behavior is that ``producer`` was able to
return multiple times. Between calls to ``produce``, the producer's
execution is suspended and the consumer has control.

A Task can be used as an iterable object in a ``for`` loop, in which
case the loop variable takes on all the produced values::

    julia> for x in Task(producer)
             println(x)
           end
    start
    2
    4
    6
    8
    stop

Note that the ``Task()`` constructor expects a 0-argument function. A
common pattern is for the producer to be parameterized, in which case a
partial function application is needed to create a 0-argument :ref:`anonymous
function <man-anonymous-functions>`. This can be done either
directly or by use of a convenience macro::

    function mytask(myarg)
        ...
    end

    taskHdl = Task(() -> mytask(7))
    # or, equivalently
    taskHdl = @task mytask(7)

``produce`` and ``consume`` are intended for multitasking, and do not
launch threads that can run on separate CPUs. True kernel threads are
discussed under the topic of :ref:`man-parallel-computing`.
