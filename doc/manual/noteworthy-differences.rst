.. _man-noteworthy-differences:

*******************************************
Noteworthy Differences from other Languages
*******************************************

Noteworthy differences from MATLAB
----------------------------------

Although MATLAB users may find Julia's syntax familiar,
Julia is in no way a MATLAB clone. There are major syntactic and
functional differences. The following are some noteworthy
differences that may trip up Julia users accustomed to MATLAB:

-  Arrays are indexed with square brackets, ``A[i,j]``.
-  Arrays are assigned by reference. After ``A=B``, assigning into ``B``
   will modify ``A`` as well.
-  Values are passed and assigned by reference. If a function modifies
   an array, the changes will be visible in the caller.
-  The imaginary unit ``sqrt(-1)`` is represented in julia with ``im``.
-  Multiple values are returned and assigned with parentheses,
   ``return (a, b)`` and ``(a, b) = f(x)``.
-  Julia has 1-dimensional arrays. Column vectors are of size ``N``, not
   ``Nx1``. For example, ``rand(N)`` makes a 1-dimensional array.
-  Concatenating scalars and arrays with the syntax ``[x,y,z]``
   concatenates in the first dimension ("vertically"). For the second
   dimension ("horizontally"), use spaces as in ``[x y z]``. To
   construct block matrices (concatenating in the first two dimensions),
   the syntax ``[a b; c d]`` is used to avoid confusion.
-  Colons ``a:b`` and ``a:b:c`` construct ``Range`` objects. To
   construct a full vector, use ``linspace``, or "concatenate" the range
   by enclosing it in brackets, ``[a:b]``.
-  Functions return values using the ``return`` keyword, instead of by
   listing their names in the function definition (see
   :ref:`man-return-keyword` for details).
-  A file may contain any number of functions, and all definitions will
   be externally visible when the file is loaded.
-  Reductions such as ``sum``, ``prod``, and ``max`` are performed over
   every element of an array when called with a single argument as in
   ``sum(A)``.
-  Functions such as ``sort`` that operate column-wise by default
   (``sort(A)`` is equivalent to ``sort(A,1)``) do not have special
   behavior for 1xN arrays; the argument is returned unmodified since it
   still performs ``sort(A,1)``. To sort a 1xN matrix like a vector, use
   ``sort(A,2)``.
-  Parentheses must be used to call a function with zero arguments, as
   in ``tic()`` and ``toc()``.
-  Do not use semicolons to end statements. The results of statements are
   not automatically printed (except at the interactive prompt), and
   lines of code do not need to end with semicolons. The function
   ``println`` can be used to print a value followed by a newline.
-  If ``A`` and ``B`` are arrays, ``A == B`` doesn't return an array of
   booleans. Use ``A .== B`` instead. Likewise for the other boolean
   operators, ``<``, ``>``, ``!=``, etc.
-  The elements of a collection can be passed as arguments to a function
   using ``...``, as in ``xs=[1,2]; f(xs...)``.
-  Julia's ``svd`` returns singular values as a vector instead of as a
   full diagonal matrix.
-  In Julia, ``...`` is not used to continue lines of code.
-  The variable ``ans`` is set to the value of the last expression issued
   in an interactive session, but not set when Julia code is run in other
   ways.

Noteworthy differences from R
-----------------------------

One of Julia's goals is to provide an effective language for data analysis and statistical programming. For users coming to Julia from R, these are some noteworthy differences:

- Julia uses ``=`` for assignment. Julia does not provide any operator like ``<-`` or ``<<-``.
- Julia constructs vectors using brackets. Julia's ``[1, 2, 3]`` is the equivalent of R's ``c(1, 2, 3)``.
- Julia's matrix operations are more like traditional mathematical notation than R's. If ``A`` and ``B`` are matrices, then ``A * B`` defines a matrix multiplication in Julia equivalent to R's ``A %*% B``. In R, this same notation would perform an elementwise Hadamard product. To get the elementwise multiplication operation, you need to write ``A .* B`` in Julia.
- Julia performs matrix transposition using the ``'`` operator. Julia's ``A'`` is therefore equivalent to R's ``t(A)``.
- Julia does not require parentheses when writing ``if`` statements or ``for`` loops: use ``for i in [1, 2, 3]`` instead of ``for (i in c(1, 2, 3))`` and ``if i == 1`` instead of ``if (i == 1)``.
- Julia does not treat the numbers ``0`` and ``1`` as Booleans. You cannot write ``if (1)`` in Julia, because ``if`` statements accept only booleans. Instead, you can write ``if true``.
- Julia does not provide ``nrow`` and ``ncol``. Instead, use ``size(M, 1)`` for ``nrow(M)`` and ``size(M, 2)`` for ``ncol(M)``.
- Julia's SVD is not thinned by default, unlike R. To get results like R's, you will often want to call ``svd(X, true)`` on a matrix ``X``.
- Julia is very careful to distinguish scalars, vectors and matrices. In R, ``1`` and ``c(1)`` are the same. In Julia, they can not be used interchangeably. One potentially confusing result of this is that ``x' * y`` for vectors ``x`` and ``y`` is a 1-element vector, not a scalar. To get a scalar, use ``dot(x, y)``.
- Julia's ``diag()`` and ``diagm()`` are not like R's.
- Julia cannot assign to the results of function calls on the left-hand of an assignment operation: you cannot write ``diag(M) = ones(n)``.
- Julia discourages populating the main namespace with functions. Most statistical functionality for Julia is found in `packages <http://docs.julialang.org/en/latest/packages/packagelist/>`_ like the DataFrames and Distributions packages:
	- Distributions functions are found in the `Distributions package <https://github.com/JuliaStats/Distributions.jl>`_
	- The `DataFrames package <https://github.com/HarlanH/DataFrames.jl>`_ provides data frames.
	- Formulas for GLM's must be escaped: use ``:(y ~ x)`` instead of ``y ~ x``.
- Julia provides tuples and real hash tables, but not R's lists. When returning multiple items, you should typically use a tuple: instead of ``list(a = 1, b = 2)``, use ``(1, 2)``. 
- Julia encourages all users to write their own types. Julia's types are much easier to use than S3 or S4 objects in R. Julia's multiple dispatch system means that ``table(x::TypeA)`` and ``table(x::TypeB)`` act like R's ``table.TypeA(x)`` and ``table.TypeB(x)``.
- In Julia, values are passed and assigned by reference. If a function modifies an array, the changes will be visible in the caller. This is very different from R and allows new functions to operate on large data structures much more efficiently.
- Concatenation of vectors and matrices is done using ``hcat`` and ``vcat``, not ``c``, ``rbind`` and ``cbind``.
- A Julia range object like ``a:b`` is not shorthand for a vector like in R, but is a specialized type of object that is used for iteration without high memory overhead. To convert a range into a vector, you need to wrap the range with brackets ``[a:b]``.
- Julia has several functions that can mutate their arguments. For example, it has ``sort(v)`` and ``sort!(v)``.
- ``colMeans()`` and ``rowMeans()``, ``size(m, 1)`` and ``size(m, 2)``
- In R, performance requires vectorization. In Julia, almost the opposite is true: the best performing code is often achieved by using devectorized loops.
- Unlike R, there is no delayed evaluation in Julia. For most users, this means that there are very few unquoted expressions or column names.
- Julia does not support the ``NULL`` type.
- There is no equivalent of R's ``assign`` or ``get`` in Julia.

Noteworthy differences from Python
----------------------------------

- Indexing of arrays, strings, etc. in Julia is 1-based not 0-based.
- The last element of a list or array is indexed with ``end`` in Julia, not -1 as in Python.
- Comprehensions in Julia do not (yet) have the optional if clause found in Python.
- For, if, while, etc. blocks in Julia are terminated by ``end``; indentation is not significant.
- Julia has no line continuation syntax: if, at the end of a line, the input so far is a complete
  expression, it is considered done; otherwise the input continues. One way to force an expression
  to continue is to wrap it in parentheses.
