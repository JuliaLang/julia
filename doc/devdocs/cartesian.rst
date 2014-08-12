.. module:: Base.Cartesian

.. _devdocs-cartesian:

Base.Cartesian
==============

The (non-exported) Cartesian module provides macros that facilitate
writing multidimensional algorithms. It is hoped that Cartesian will
not, in the long term, be necessary; however, at present it is one of
the few ways to write compact and performant multidimensional code.


Principles of usage
-------------------

A simple example of usage is::

    @nloops 3 i A begin
        s += @nref 3 A i
    end

which generates the following code::

    for i_3 = 1:size(A,3)
        for i_2 = 1:size(A,2)
	    for i_1 = 1:size(A,1)
                s += A[i_1,i_2,i_3]
	    end
	end
    end

In general, Cartesian allows you to write generic code that contains
repetitive elements, like the nested loops in this example.  Other
applications include repeated expressions (e.g., loop unwinding) or
creating function calls with variable numbers of arguments without using
the "splat" construct (``i...``).

Basic syntax
------------

The (basic) syntax of ``@nloops`` is as follows:

-  The first argument must be an integer (*not* a variable) specifying
   the number of loops.
-  The second argument is the symbol-prefix used for the iterator
   variable. Here we used ``i``, and variables ``i_1, i_2, i_3`` were
   generated.
-  The third argument specifies the range for each iterator variable. If
   you use a variable (symbol) here, it's taken as ``1:size(A,dim)``.
   More flexibly, you can use the anonymous-function expression syntax
   described below.
-  The last argument is the body of the loop. Here, that's what appears
   between the ``begin...end``.

There are some additional features of ``@nloops`` described in the
:ref:`reference section <devdoc-cartesian-reference>`.

``@nref`` follows a similar pattern, generating ``A[i_1,i_2,i_3]`` from
``@nref 3 A i``. The general practice is to read from left to right,
which is why ``@nloops`` is ``@nloops 3 i A expr`` (as in
``for i_2 = 1:size(A,2)``, where ``i_2`` is to the left and the range is
to the right) whereas ``@nref`` is ``@nref 3 A i`` (as in
``A[i_1,i_2,i_3]``, where the array comes first).

If you're developing code with Cartesian, you may find that debugging is
easier when you examine the generated code, using ``macroexpand``::

    julia> macroexpand(:(@nref 2 A i))
    :(A[i_1,i_2])


Supplying the number of expressions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The first argument to both of these macros is the number of
expressions, which must be an integer. When you're writing a function
that you intend to work in multiple dimensions, this may not be
something you want to hard-code.  Perhaps the most straightforward
approach is to use the ``@ngenerate`` macro.

Perhaps the easiest way to understand ``@ngenerate`` is to see it in
action.  Here's a slightly cleaned up example::

    julia> macroexpand(:(@ngenerate N typeof(A) function mysum{T,N}(A::Array{T,N})
            s = zero(T)
            @nloops N i A begin
                s += @nref N A i
            end
            s
        end))
    :(begin
        function mysum{T}(A::Array{T,1}) # none, line 2:
            s = zero(T) # line 3:
            for i_1 = 1:size(A,1) # line 293:
                s += A[i_1]
            end # line 295:
            s
        end
        function mysum{T}(A::Array{T,2}) # none, line 2:
            s = zero(T) # line 3:
            for i_2 = 1:size(A,2) # line 293:
                for i_1 = 1:size(A,1) # line 293:
                    s += A[i_1,i_2]
                end # line 295:
            end # line 295:
            s
        end
        function mysum{T}(A::Array{T,3}) # none, line 2:
            s = zero(T) # line 3:
            for i_3 = 1:size(A,3) # line 293:
                for i_2 = 1:size(A,2) # line 293:
                    for i_1 = 1:size(A,1) # line 293:
                        s += A[i_1,i_2,i_3]
                    end # line 295:
                end # line 295:
            end # line 295:
            s
        end
        function mysum{T}(A::Array{T,4}) # none, line 2:
            ...
        end
        let mysum_cache = Dict{Int,Function}() # line 113:
            function mysum{T,N}(A::Array{T,N}) # cartesian.jl, line 100:
                if !(haskey(mysum_cache,N)) # line 102:
                    localfunc = quote
		        function _F_{T}(A::Array{T,$N})
                            s = zero(T)
                            @nloops $N i A begin
                                s += @nref $N A i
                            end
                            s
                        end
                    end
		    mysum_cache[N] = eval(quote
                        local _F_
                        $localfunc
                        _F_
                    end)
                end
                mysum_cache[N](A)::typeof(A)
            end
        end
    end)

You can see that ``@ngenerate`` causes explicit versions to be
generated for dimensions 1 to 4 (a setting controlled by the constant
``CARTESIAN_DIMS``).  To allow arbitrary-dimensional arrays to be
handled, it also generates a version in which different methods are
cached in a dictionary.  If a given method has not yet been generated,
it creates a version specific to that dimensionality and then stores
it in the dictionary.  Creating the method is slow---it involves
generating expressions and then evaluating them---but once created the
function can be looked up from the cache, and is reasonably efficient
(but still less efficient than the versions generated for explicit
dimensionality).

The arguments to ``@ngenerate`` are:

- The symbol of the variable that will be used for generating
  different versions (in the example, ``N``)
- The return type of the function (in the example,
  ``typeof(A)``). This is not used for the versions that are generated
  for specific ``N``, but is needed for the dictionary-backed
  version.  Julia cannot infer the return type of the function looked
  up from the dictionary.
- The actual function declaration.  Use ``N`` as you would a normal
  parameter.


Anonymous-function expressions as macro arguments
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Perhaps the single most powerful feature in ``Cartesian`` is the
ability to supply anonymous-function expressions that get evaluated at
parsing time.  Let's consider a simple example::

    @nexprs 2 j->(i_j = 1)

``@nexprs`` generates ``n`` expressions that follow a pattern. This
code would generate the following statements::

    i_1 = 1
    i_2 = 1

In each generated statement, an "isolated" ``j`` (the variable of the
anonymous function) gets replaced by values in the range ``1:2``.
Generally speaking, Cartesian employs a LaTeX-like syntax.  This
allows you to do math on the index ``j``.  Here's an example computing
the strides of an array::

    s_1 = 1
    @nexprs 3 j->(s_{j+1} = s_j * size(A, j))

would generate expressions
::

    s_1 = 1
    s_2 = s_1 * size(A, 1)
    s_3 = s_2 * size(A, 2)
    s_4 = s_3 * size(A, 3)

Anonymous-function expressions have many uses in practice.

.. _devdoc-cartesian-reference:

Macro reference
~~~~~~~~~~~~~~~

Macros for creating functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: @ngenerate Nsym returntypeexpr functiondeclexpr

    Generate versions of a function for different values of ``Nsym``.

.. function:: @nsplat Nsym functiondeclexpr
              @nsplat Nsym dimrange functiondeclexpr

    Generate explicit versions of a function for different numbers of
    arguments.  For example::

        @nsplat N 2:3 absgetindex(A, I::NTuple{N,Real}...) = abs(getindex(A, I...))

    generates::

        absgetindex(A, I_1::Real, I_2::Real) = abs(getindex(A, I_1, I_2))
        absgetindex(A, I_1::Real, I_2::Real, I_3::Real) = abs(getindex(A, I_1, I_2, I_3))


Macros for function bodies
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: @nloops N itersym rangeexpr bodyexpr
              @nloops N itersym rangeexpr preexpr bodyexpr
              @nloops N itersym rangeexpr preexpr postexpr bodyexpr

    Generate ``N`` nested loops, using ``itersym`` as the prefix for
    the iteration variables. ``rangeexpr`` may be an
    anonymous-function expression, or a simple symbol ``var`` in which
    case the range is ``1:size(var,d)`` for dimension ``d``.

    Optionally, you can provide "pre" and "post" expressions. These
    get executed first and last, respectively, in the body of each
    loop. For example,
    ::

        @nloops 2 i A d->j_d=min(i_d,5) begin
            s += @nref 2 A j
        end

    would generate
    ::

        for i_2 = 1:size(A, 2)
            j_2 = min(i_2, 5)
            for i_1 = 1:size(A, 1)
                j_1 = min(i_1, 5)
                s += A[j_1,j_2]
            end
        end

    If you want just a post-expression, supply
    ``nothing`` for the pre-expression. Using parenthesis and
    semicolons, you can supply multi-statement expressions.

.. function:: @nref N A indexexpr

    Generate expressions like ``A[i_1,i_2,...]``.  ``indexexpr`` can
    either be an iteration-symbol prefix, or an anonymous-function
    expression.

.. function:: @nexprs N expr

    Generate ``N`` expressions. ``expr`` should be an
    anonymous-function expression.

.. function:: @ntuple N expr

    Generates an ``N``-tuple.  ``@ntuple 2 i`` would generate ``(i_1, i_2)``, and ``@ntuple 2 k->k+1`` would generate ``(2,3)``.

.. function:: @nall N expr

    ``@nall 3 d->(i_d > 1)`` would generate the expression
    ``(i_1 > 1 && i_2 > 1 && i_3 > 1)``. This can be convenient for
    bounds-checking.

.. function:: @nif N conditionexpr expr
              @nif N conditionexpr expr elseexpr

    Generates a sequence of ``if ... elseif ... else ... end`` statements. For example::

        @nif 3 d->(i_d >= size(A,d)) d->(error("Dimension ", d, " too big")) d->println("All OK")

    would generate::

        if i_1 > size(A, 1)
	    error("Dimension ", 1, " too big")
        elseif i_2 > size(A, 2)
	    error("Dimension ", 2, " too big")
        else
	    println("All OK")
	end


Frequently asked questions
^^^^^^^^^^^^^^^^^^^^^^^^^^


I got an error ``ERROR: N not defined`` when using ``@ngenerate``. Why?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Most likely you forgot to define your function with ``N`` as a type parameter, e.g., ``@ngenerate N returntype myfunc{N}(...)``.
