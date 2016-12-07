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
something you want to hard-code. If you're writing code that
you need to work with older Julia versions, currently you
should use the ``@ngenerate`` macro described in `an older version of this documentation <http://docs.julialang.org/en/release-0.3/devdocs/cartesian/#supplying-the-number-of-expressions>`_.

Starting in Julia 0.4-pre, the recommended approach is to use
a ``@generated function``.  Here's an example::

  @generated function mysum{T,N}(A::Array{T,N})
      quote
          s = zero(T)
          @nloops $N i A begin
              s += @nref $N A i
          end
          s
      end
  end

Naturally, you can also prepare expressions or perform calculations
before the ``quote`` block.


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

.. function:: @nloops N itersym rangeexpr bodyexpr
              @nloops N itersym rangeexpr preexpr bodyexpr
              @nloops N itersym rangeexpr preexpr postexpr bodyexpr

   .. Docstring generated from Julia source

   Generate ``N`` nested loops, using ``itersym`` as the prefix for the iteration variables. ``rangeexpr`` may be an anonymous-function expression, or a simple symbol ``var`` in which case the range is ``1:size(var,d)`` for dimension ``d``\ .

   Optionally, you can provide "pre" and "post" expressions. These get executed first and last, respectively, in the body of each loop. For example:

   .. code-block:: julia

       @nloops 2 i A d->j_d=min(i_d,5) begin
           s += @nref 2 A j
       end

   would generate:

   .. code-block:: julia

       for i_2 = 1:size(A, 2)
           j_2 = min(i_2, 5)
           for i_1 = 1:size(A, 1)
               j_1 = min(i_1, 5)
               s += A[j_1,j_2]
           end
       end

   If you want just a post-expression, supply ``nothing`` for the pre-expression. Using parentheses and semicolons, you can supply multi-statement expressions.

.. function:: @nref N A indexexpr

   .. Docstring generated from Julia source

   Generate expressions like ``A[i_1,i_2,...]``\ . ``indexexpr`` can either be an iteration-symbol prefix, or an anonymous-function expression.

.. function:: @nextract N esym isym

   .. Docstring generated from Julia source

   Generate ``N`` variables ``esym_1``\ , ``esym_2``\ , ..., ``esym_N`` to extract values from ``isym``\ . ``isym`` can be either a ``Symbol`` or anonymous-function expression.

   ``@nextract 2 x y`` would generate

   .. code-block:: julia

       x_1 = y[1]
       x_2 = y[2]

   while ``@nextract 3 x d->y[2d-1]`` yields

   .. code-block:: julia

       x_1 = y[1]
       x_2 = y[3]
       x_3 = y[5]

.. function:: @nexprs N expr

   .. Docstring generated from Julia source

   Generate ``N`` expressions. ``expr`` should be an anonymous-function expression.

.. function:: @ncall N f sym...

   .. Docstring generated from Julia source

   Generate a function call expression. ``sym`` represents any number of function arguments, the last of which may be an anonymous-function expression and is expanded into ``N`` arguments.

   For example ``@ncall 3 func a`` generates

   .. code-block:: julia

       func(a_1, a_2, a_3)

   while ``@ncall 2 func a b i->c[i]`` yields

   .. code-block:: julia

       func(a, b, c[1], c[2])

.. function:: @ntuple N expr

   .. Docstring generated from Julia source

   Generates an ``N``\ -tuple. ``@ntuple 2 i`` would generate ``(i_1, i_2)``\ , and ``@ntuple 2 k->k+1`` would generate ``(2,3)``\ .

.. function:: @nall N expr

   .. Docstring generated from Julia source

   Check whether all of the expressions generated by the anonymous-function expression ``expr`` evaluate to ``true``\ .

   ``@nall 3 d->(i_d > 1)`` would generate the expression ``(i_1 > 1 && i_2 > 1 && i_3 > 1)``\ . This can be convenient for bounds-checking.

.. function:: @nany N expr

   .. Docstring generated from Julia source

   Check whether any of the expressions generated by the anonymous-function expression ``expr`` evaluate to ``true``\ .

   ``@nany 3 d->(i_d > 1)`` would generate the expression ``(i_1 > 1 || i_2 > 1 || i_3 > 1)``\ .

.. function:: @nif N conditionexpr expr
              @nif N conditionexpr expr elseexpr

   .. Docstring generated from Julia source

   Generates a sequence of ``if ... elseif ... else ... end`` statements. For example:

   .. code-block:: julia

       @nif 3 d->(i_d >= size(A,d)) d->(error("Dimension ", d, " too big")) d->println("All OK")

   would generate:

   .. code-block:: julia

       if i_1 > size(A, 1)
           error("Dimension ", 1, " too big")
       elseif i_2 > size(A, 2)
           error("Dimension ", 2, " too big")
       else
           println("All OK")
       end

