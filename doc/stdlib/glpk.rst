:mod:`glpk.jl` --- Wrapper for the GNU Linear Programming Kit (GLPK)
======================================================================

.. module:: glpk.jl
   :synopsis: GLPK wrapper

This module provides a wrapper for the GNU Linear Programming Kit (`GLPK <http://www.gnu.org/software/glpk>`_),
which is a C library, in Julia.
It is designed for making it easy to port C code to Julia, while at the same time having the
benefits of the higher level language features of Julia, like the automatic management of memory, the possibility
of returning tuples/strings/vectors etc.

It's currently based on GLPK version 4.47.

--------
Preamble
--------

Almost all GLPK functions can be called in Julia with the same syntax as in the original C library,
with some simple translation rules (with very :ref:`few exceptions <glpk-different-than-C>`).
Some functionality is still missing (see :ref:`this list <glpk-not-available>`); most of it will be
added in the future.

Let's start with an example. This is an excerpt from the beginning of the :file:`sample.c` example program
which ships with GLPK:

.. code-block:: c

    /* C code */
    glp_prob *lp = glp_create_prob();
    glp_set_prob_name(lp, "sample");
    glp_set_obj_dir(lp, GLP_MAX);
    glp_add_rows(lp, 3);
    glp_set_row_name(lp, 1, "p");
    glp_set_row_bnds(lp, 1, GLP_UP, 0.0, 100.0);

This is the Julia translation of the above::

    # Julia code
    lp = GLPProb()
    glp_set_prob_name(lp, "sample")
    glp_set_obj_dir(lp, GLP_MAX)
    glp_add_rows(lp, 3)
    glp_set_row_name(lp, 1, "p")
    glp_set_row_bnds(lp, 1, GLP_UP, 0.0, 100.0)

Apart from the first line, which is different, all the rest is pretty much unchanged.
In particular, note that the GLPK constants, such as ``GLP_MAX`` and ``GLP_UP``, are available, unchanged,
within Julia (at the moment they are integer values, like in C, but this may change in the future).

Because of the strict adherence of the Julia functions to their C counterparts, and since the GLPK
documentation is extremely well written and complete, this manual page is not going to document
the whole GLPK library in detail, but rather provide :ref:`the rules <glpk-translation-rules>` needed to translate
from C to Julia, detail the :ref:`few exceptions <glpk-different-than-C>` to these rules and then
:ref:`list all the available functions <glpk-function-list>` with a brief description of their
usage.

Please, refer to the original GLPK manual (available at http://www.gnu.org/software/glpk) for a detailed
description of the library API.

.. _glpk-translation-rules:

--------------------------------------
GLPK translation rules from C to Julia
--------------------------------------

1) from C stucts to Julia objects
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

All structs in the original GLPK are wrapped up in composite types, which initialize and destroy themselves
as needed. For example, the ``glp_prob`` C struct becomes the ``GLPProb`` Julia type.
Whenever in C you would pass a pointer to a struct, in Julia you pass a corresponding composite object.
This is the table relating C structs with Julia types:

+---------------+--------------------------+
|  C            |  Julia                   |
+===============+==========================+
| ``glp_prob``  | ``GLPProb``              |
+---------------+--------------------------+
| ``glp_smcp``  | ``GLPSimplexParam``      |
+---------------+--------------------------+
| ``glp_iptcp`` | ``GLPInteriorParam``     |
+---------------+--------------------------+
| ``glp_iocp``  | ``GLPIntoptParam``       |
+---------------+--------------------------+
| ``glp_bfcp``  | ``GLPBasisFactParam``    |
+---------------+--------------------------+
| ``glp_tran``  | ``GLPMathProgWorkspace`` |
+---------------+--------------------------+
| ``glp_data``  | ``GLPData``              |
+---------------+--------------------------+

Therefore, the original C GLPK API:

.. code-block:: c

    int glp_simplex(glp_prob * lp, glp_smpc * param)

becomes::

    glp_simplex(lp::GLPProb, param::GLPSimplexParam)

In the C GLPK API, objects are created by functions, such as:

.. code-block:: c

    glp_prob * lp = glp_create_prob();
    glp_smcp * param = glp_smcp_init();

and need to be destroyed when the program is finished:

.. code-block:: c

    glp_delete_prob(lp);
    glp_smcp_delete(smcp);

In Julia, objects are created by calling the object constructor (without parameters)::

    lp = GLPProb()
    param = GLPSimplexParam()

and they are automatically destroyed by the garbage collector when no longer needed.


2) setting the parameters to the solvers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In all GLPK solver functions, like ``glp_simplex``, options are passed via structs. As stated before, these become
composite object types in Julia; but instead of setting a field, like in C:

.. code-block:: c

    param = glp_smcp_init();
    param.msg_lev = GLP_MSG_ERR;
    param.presolve = GLP_ON;

in Julia one uses an array-like referencing syntax::

    param = GLPSimplexParam()
    param["msg_lev"]= GLP_MSG_ERR
    param["presolve"] = GLP_ON

Note that the field names are passed as strings, and that all GLPK constants are available in Julia.
Also note that no test is currently performed at assignment to check that the provided values are valid.

This part of the API may change in the future.


3) scalar and array types translate in a natural way
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following C-to-Julia type conversion rules apply:

+--------------+-------------+
| C            | Julia       |
+==============+=============+
| ``int``      | ``Int32``   |
+--------------+-------------+
| ``double``   | ``Float64`` |
+--------------+-------------+
| ``char[]``   | ``String``  |
+--------------+-------------+
| ``glp_long`` | ``Int64``   |
+--------------+-------------+

On output, these rules apply exactly. On input, on the other hand, Julia requirements are more relaxed:

+--------------+-------------+
| C            | Julia       |
+==============+=============+
| ``int``      | ``Integer`` |
+--------------+-------------+
| ``glp_long`` | ``Integer`` |
+--------------+-------------+
| ``double``   | ``Real``    |
+--------------+-------------+

Whenever the C version expects a pointer to an array, a Julia Array can be passed. In the GLPK API, all indexing
starts from 1 even in the C version, so no special care is required on that side (in C, you would leave an
unused element at the beginning of each array; in Julia you don't).

The relaxed requirements for inputs are also valid for arrays (e.g. one can pass an ``Array{Int64}`` when an array
of ``int`` is expected, and it will be converted automatically). The only exception is for functions which
return an array of values by filling out an allocated array whose pointer is provided by the user.
In that case, the strict version of the rules applies (i.e. you can only pass an ``Array{Int32}`` if an
array of ``int`` is expected). Those functions almost always have an alternative, more convenient formulation
as well, though.


4) optional arguments
^^^^^^^^^^^^^^^^^^^^^

Whenever the C version accepts the value ``NULL`` to indicate an optional pointer argument, the Julia version
accepts the constant ``nothing``. In case the optional pointer argument is an array, an empty array is
also accepted (it can be of the expected type, e.g. ``Int32[]``, or even just ``[]``)
Most of the time, alternative ways to call the function are also provided.


5) fatal errors become exceptions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Whenever an invalid condition is detected (e.g. if you pass an invalid parameter, such as a negative length),
the Julia GLPK wrapper throws a ``GLPError`` exception with some message detailing what went wrong.
Ideally, all invalid input combinations should be captured by Julia before being passed
over to the library, so that all errors could be catched via a ``try ... catch`` block;
in practice, it is likely that some conditions exist which will leak to the C API and break Julia: this should be
considered as a bug (and reported as such).

.. _glpk-not-available:

---------------------------------------------------
GLPK functions which are not avaliable yet in Julia
---------------------------------------------------

In general, all parts of the GLPK API which rely on callback functions are not avaliable in Julia.
In particular, you should not set the callback fields (``cb_func`` and ``cb_info``) in the ``GLPIntoptParam``
type, unless you *really* know what you're doing.

There are 5 groups of functions which are not wrapped:

1. The branch & cut API function for mixed integer programming, because they are supposed to be called from
   within a callback (see chapter 5 in the GLPK manual); they all start with this prefix:

   * ``glp_ios_*``

2. All graph and network routines (anything involving ``glp_graph`` objects); these will be added in the future)

3. Some misc functions which either have a variable argument list or involve callbacks (see section 6.1 in the GLPK
   manual):

   * ``glp_printf``
   * ``glp_vprintf``
   * ``glp_term_hook``
   * ``glp_error``
   * ``glp_assert``
   * ``glp_error_hook``

4. Some plain data file reading routines which involve long jumps / varargs (see section 6.2 in the GLPK manual):

   * ``glp_sdf_set_jump``
   * ``glp_sdf_error``
   * ``glp_sdf_warning``


5. One additional routine, which may be included in the future:

   * ``lpx_check_kkt``

.. _glpk-different-than-C:

------------------------------------------------
Functions which differ from their C counterparts
------------------------------------------------

Some library functions return multiple values; as C cannot do this directly, this is obtained via some "pointer gymnastics".
In Julia, on the other hand, this is not necessary, and providing an exact counterpart to the C version would be awkward and
pointless. There are 3 such functions:

* ``glp_analyze_bound``
* ``glp_analyze_coef``
* ``glp_mem_usage``

For example the C declaration for ``glp_analyze_bound`` is:

.. code-block:: c

    void glp_analyze_bound(glp_prob *lp, int k, int *limit1, int *var1, int *limit2, int *var2)

In Julia, this becomes::

    glp_analyze_bound(glp_prob::GLPProb, k::Integer)

which returns a tuple::

    julia> (limit1, var1, limit2, var2) = glp_analyze_bound(glp_prob, k)
    
The other 2 functions work in the same way, by just returning the values which in C you would pass
as pointers.

Some other functions have both a strictly-compatible calling form, for simplifying C code porting,
and some more convenient Julia counterparts. See :ref:`the list below <glpk-function-list>` for more details.

One function has a different return value: ``glp_version`` returns a tuple of integer with the major and minor
version numbers, rather then a string.

.. _glpk-function-list:

-------------------------------
List of GLPK functions in Julia
-------------------------------

As stated above, this list only offers a brief explanation of what each function does and presents alternative
calling forms when available. Refer to the GLPK manual for a complete description.

.. function:: glp_set_prob_name(glp_prob, name)

    Assigns a name to the problem object (or deletes it if ``name`` is empty or ``nothing``).

.. function:: glp_set_obj_name(glp_prob, name)

    Assigns a name to the objective function (or deletes it if ``name`` is empty or ``nothing``).

.. function:: glp_set_obj_dir(glp_prob, dir)

    Sets the optimization direction, ``GLP_MIN`` (minimization) or ``GLP_MAX`` (maximization).

.. function:: glp_add_rows(glp_prob, rows)

    Adds the given number of rows (constraints) to the problem object; returns the number of
    the first new row added.

.. function:: glp_add_cols(glp_prob, cols)

    Adds the given number of columns (structural variables) to the problem object; returns the number of
    the first new column added.

.. function:: glp_set_row_name(glp_prob, row, name)

    Assigns a name to the specified row (or deletes it if ``name`` is empty or ``nothing``).

.. function:: glp_set_col_name(glp_prob, col, name)

    Assigns a name to the specified column (or deletes it if ``name`` is empty or ``nothing``).

.. function:: glp_set_row_bnds(glp_prob, row, bounds_type, lb, ub)

    Sets the type and bounds on a row. ``type`` must be one of ``GLP_FR`` (free), ``GLP_LO`` (lower bounded),
    ``GLP_UP`` (upper bounded), ``GLP_DB`` (double bounded), ``GLP_FX`` (fixed).

    At initialization, each row is free.

.. function:: glp_set_col_bnds(glp_prob, col, bounds_type, lb, ub)

    Sets the type and bounds on a column. ``type`` must be one of ``GLP_FR`` (free), ``GLP_LO`` (lower bounded),
    ``GLP_UP`` (upper bounded), ``GLP_DB`` (double bounded), ``GLP_FX`` (fixed).

    At initialization, each column is fixed at 0.

.. function:: glp_set_obj_coef(glp_prob, col, coef)

    Sets the objective coefficient to a column (``col`` can be 0 to indicate the constant term of the objective function).

.. function:: glp_set_mat_row(glp_prob, row, [len,] ind, val)

    Sets (replaces) the content of a row. The content is specified in sparse format: ``ind`` is a vector of indices,
    ``val`` is the vector of corresponding values. ``len`` is the number of vector elements which will be considered,
    and must be less or equal to the length of both ``ind`` and ``val``.  If ``len`` is 0, ``ind`` and/or ``val`` can be ``nothing``.

    In Julia, ``len`` can be omitted, and then it is inferred from ``ind`` and ``val`` (which need to have the same length
    in such case).

.. function:: glp_set_mat_col(glp_prob, col, [len,] ind, val)

    Sets (replaces) the content of a column. Everything else is like ``glp_set_mat_row``.

.. function:: glp_load_matrix(glp_prob, [numel,] ia, ja, ar)
              glp_load_matrix(glp_prob, A)

    Sets (replaces) the content matrix (i.e. sets all  rows/coluns at once). The matrix is passed in sparse
    format.

    In the first form (original C API), it's passed via 3 vectors: ``ia`` and ``ja`` are for rows/columns
    indices, ``ar`` is for values. ``numel`` is the number of elements which will be read and must be less or
    equal to the length of any of the 3 vectors. If ``numel`` is 0, any of the vectors can be passed as ``nothing``.

    In Julia, ``numel`` can be omitted, and then it is inferred from ``ia``, ``ja`` and ``ar`` (which need to have the same length
    in such case).

    Also, in Julia there's a second, simpler calling form, in which the matrix is passed as a ``SparseMatrixCSC`` object.

.. function:: glp_check_dup(rows, cols, [numel,] ia, ja)

    Check for duplicates in the indices vectors ``ia`` and ``ja``. ``numel`` has the same meaning and (optional) use as in
    ``glp_load_matrix``. Returns 0 if no duplicates/out-of-range indices are found, or a positive number indicating where a duplicate
    occurs, or a negative number indicating an out-of-bounds index.

.. function:: glp_sort_matrix(glp_prob)

    Sorts the elements of the problem object's matrix.

.. function:: glp_del_rows(glp_prob, [num_rows,] rows_ids)

    Deletes rows from the problem object. Rows are specified in the ``rows_ids`` vector. ``num_rows`` is the number of elements
    of ``rows_ids`` which will be considered, and must be less or equal to the length id ``rows_ids``. If ``num_rows`` is 0, ``rows_ids``
    can be ``nothing``. In Julia, ``num_rows`` is optional (it's inferred from ``rows_ids`` if not given).

.. function:: glp_del_cols(glp_prob, cols_ids)

    Deletes columns from the problem object. See ``glp_del_rows``.

.. function:: glp_copy_prob(glp_prob_dest, glp_prob, copy_names)

    Makes a copy of the problem object. The flag ``copy_names`` determines if names are copied, and must be either ``GLP_ON`` or ``GLP_OFF``.

.. function:: glp_erase_prob(glp_prob)

    Resets the problem object.

.. function:: glp_get_prob_name(glp_prob)

    Returns the problem object's name. Unlike the C version, if the problem has no assigned name, returns an empty string.

.. function:: glp_get_obj_name(glp_prob)

    Returns the objective function's name. Unlike the C version, if the objective has no assigned name, returns an empty string.

.. function:: glp_get_obj_dir(glp_prob)

    Returns the optimization direction, ``GLP_MIN`` (minimization) or ``GLP_MAX`` (maximization).

.. function:: glp_get_num_rows(glp_prob)

    Returns the current number of rows.

.. function:: glp_get_num_cols(glp_prob)

    Returns the current number of columns.

.. function:: glp_get_row_name(glp_prob, row)

    Returns the name of the specified row. Unlike the C version, if the row has no assigned name, returns an empty string.

.. function:: glp_get_col_name(glp_prob, col)

    Returns the name of the specified column. Unlike the C version, if the column has no assigned name, returns an empty string.

.. function:: glp_get_row_type(glp_prob, row)

    Returns the type of the specified row: ``GLP_FR`` (free), ``GLP_LO`` (lower bounded),
    ``GLP_UP`` (upper bounded), ``GLP_DB`` (double bounded), ``GLP_FX`` (fixed).

.. function:: glp_get_row_lb(glp_prob, row)

    Returns the lower bound of the specified row, ``-DBL_MAX`` if unbounded.

.. function:: glp_get_row_ub(glp_prob, row)

    Returns the upper bound of the specified row, ``+DBL_MAX`` if unbounded.

.. function:: glp_get_col_type(glp_prob, col)

    Returns the type of the specified column: ``GLP_FR`` (free), ``GLP_LO`` (lower bounded),
    ``GLP_UP`` (upper bounded), ``GLP_DB`` (double bounded), ``GLP_FX`` (fixed).

.. function:: glp_get_col_lb(glp_prob, col)

    Returns the lower bound of the specified column, ``-DBL_MAX`` if unbounded.

.. function:: glp_get_col_ub(glp_prob, col)

    Returns the upper bound of the specified column, ``+DBL_MAX`` if unbounded.

.. function:: glp_get_obj_coef(glp_prob, col)

    Return the objective coefficient to a column (``col`` can be 0 to indicate the constant term of the objective function).

.. function:: glp_get_num_nz(glp_prob)

    Return the number of non-zero elements in the constraint matrix.

.. function:: glp_get_mat_row(glp_prob, row, ind, val)
              glp_get_mat_row(glp_prob, row)

    Returns the contents of a row. In the first form (original C API), it fills the ``ind`` and ``val`` vectors provided,
    which must be of type ``Vector{Int32}`` and ``Vector{Float64}`` respectively, and have a sufficient length to hold the result
    (or they can be empty or ``nothing``, and then they're not filled). It returns the length of the result.

    In Julia, there's a second, simpler calling form which allocates and returns the two vectors as ``(ind, val)``.

.. function:: glp_get_mat_col(glp_prob, col, ind, val)
              glp_get_mat_col(glp_prob, col)

    Returns the contents of a column. See ``glp_get_mat_row``.

.. function:: glp_create_index(glp_prob)

    Creates the name index (used by ``glp_find_row``, ``glp_find_col``) for the problem object.

.. function:: glp_find_row(glp_prob, name)

    Finds the numeric id of a row by name. Returns 0 if no row with the given name is found.

.. function:: glp_find_col(glp_prob, name)

    Finds the numeric id of a column by name. Returns 0 if no column with the given name is found.

.. function:: glp_delete_index(glp_prob)

    Deletes the name index for the problem object.

.. function:: glp_set_rii(glp_prob, row, rii)

    Sets the rii scale factor for the specified row.

.. function:: glp_set_sjj(glp_prob, col, sjj)

    Sets the sjj scale factor for the specified column.

.. function:: glp_get_rii(glp_prob, row)

    Returns the rii scale factor for the specified row.

.. function:: glp_get_sjj(glp_prob, col)

    Returns the sjj scale factor for the specified column.

.. function:: glp_scale_prob(glp_prob, flags)

    Performs automatic scaling of problem data for the problem object. The parameter ``flags`` can be ``GLP_SF_AUTO`` (automatic)
    or a bitwise OR of the forllowing: ``GLP_SF_GM`` (geometric mean), ``GLP_SF_EQ`` (equilibration), ``GLP_SF_2N`` (nearest power of 2),
    ``GLP_SF_SKIP`` (skip if well scaled).

.. function:: glp_unscale_prob(glp_prob)

    Unscale the problem data (cancels the scaling effect).

.. function:: glp_set_row_stat(glp_prob, row, stat)

    Sets the status of the specified row. ``stat`` must be one of: ``GLP_BS`` (basic), ``GLP_NL`` (non-basic lower bounded),
    ``GLP_NU`` (non-basic upper-bounded), ``GLP_NF`` (non-basic free), ``GLP_NS`` (non-basic fixed).

.. function:: glp_set_col_stat(glp_prob, col, stat)

    Sets the status of the specified column. ``stat`` must be one of: ``GLP_BS`` (basic), ``GLP_NL`` (non-basic lower bounded),
    ``GLP_NU`` (non-basic upper-bounded), ``GLP_NF`` (non-basic free), ``GLP_NS`` (non-basic fixed).

.. function:: glp_std_basis(glp_prob)

    Constructs the standard (trivial) initial LP basis for the problem object.

.. function:: glp_adv_basis(glp_prob[, flags])

    Constructs an advanced initial LP basis for the problem object. The flag ``flags`` is optional; it must be 0 if given.

.. function:: glp_cpx_basis(glp_prob)

    Constructs an initial LP basis for the problem object with the algorithm proposed by R. Bixby.

.. function:: glp_simplex(glp_prob, [glp_param])

    The routine ``glp_simplex`` is a driver to the LP solver based on the simplex
    method. This routine retrieves problem data from the specified problem
    object, calls the solver to solve the problem instance, and stores results of
    computations back into the problem object.

    The parameters are specified via the optional ``glp_param`` argument, which is of type ``GLPSimplexParam``
    (or ``nothing`` to use the default settings).

    Returns 0 in case of success, or a non-zero flag specifying the reason for failure: ``GLP_EBADB`` (invalid base),
    ``GLP_ESING`` (singular matrix), ``GLP_ECOND`` (ill-conditioned matrix), ``GLP_EBOUND`` (incorrect bounds),
    ``GLP_EFAIL`` (solver failure), ``GLP_EOBJLL`` (lower limit reached), ``GLP_EOBJUL`` (upper limit reached),
    ``GLP_ITLIM`` (iterations limit exceeded), ``GLP_ETLIM`` (time limit exceeded), ``GLP_ENOPFS`` (no primal feasible
    solution), ``GLP_ENODFS`` (no dual feasible solution).

.. function:: glp_exact(glp_prob, [glp_param])

    A tentative implementation of the primal two-phase simplex method based on exact (rational) arithmetic. Similar to
    ``glp_simplex``. The optional glp_param is of type ``GLPSimplexParam``.

    The possible return values are 0 (success) or ``GLP_EBADB``, ``GLP_ESING``, ``GLP_EBOUND``,
    ``GLP_EFAIL``, ``GLP_ITLIM``, ``GLP_ETLIM`` (see ``glp_simplex``).

.. function:: glp_init_smcp(glp_param)

    Initializes a ``GLPSimplexParam`` object with the default values. In Julia, this is done at object creation time; this
    function can be used to reset the object.

.. function:: glp_get_status(glp_prob)

    Returns the generic status of the current basic solution: ``GLP_OPT`` (optimal),
    ``GLP_FEAS`` (feasible), ``GLP_INFEAS`` (infeasible), ``GLP_NOFEAS`` (no feasible solution), ``GLP_UNBND``
    (unbounded solution), ``GLP_UNDEF`` (undefined).

.. function:: glp_get_prim_stat(glp_prob)

    Returns the status of the primal basic solution: ``GLP_FEAS``, ``GLP_INFEAS``, ``GLP_NOFEAS``,
    ``GLP_UNDEF`` (see ``glp_get_status``).

.. function:: glp_get_dual_stat(glp_prob)

    Returns the status of the dual basic solution: ``GLP_FEAS``, ``GLP_INFEAS``, ``GLP_NOFEAS``,
    ``GLP_UNDEF`` (see ``glp_get_status``).

.. function:: glp_get_obj_val(glp_prob)

    Returns the current value of the objective function.

.. function:: glp_get_row_stat(glp_prob, row)

    Returns the status of the specified row: ``GLP_BS``, ``GLP_NL``, ``GLP_NU``, ``GLP_NF``,
    ``GLP_NS`` (see ``glp_set_row_stat``).

.. function:: glp_get_row_prim(glp_prob, row)

    Returns the primal value of the specified row.

.. function:: glp_get_row_dual(glp_prob, row)

    Returns the dual value (reduced cost) of the specified row.

.. function:: glp_get_col_stat(glp_prob, col)

    Returns the status of the specified column: ``GLP_BS``, ``GLP_NL``, ``GLP_NU``, ``GLP_NF``,
    ``GLP_NS`` (see ``glp_set_row_stat``).

.. function:: glp_get_col_prim(glp_prob, col)

    Returns the primal value of the specified column.

.. function:: glp_get_col_dual(glp_prob, col)

    Returns the dual value (reduced cost) of the specified column.

.. function:: glp_get_unbnd_ray(glp_prob)

    Returns the number k of a variable, which causes primal or dual unboundedness (if 1 <= k <= rows
    it's row k; if rows+1 <= k <= rows+cols it's column k-rows, if k=0 such variable is not defined).

.. function:: glp_interior(glp_prob, [glp_param])

    The routine ``glp_interior`` is a driver to the LP solver based on the primal-dual
    interior-point method. This routine retrieves problem data from the
    specified problem object, calls the solver to solve the problem instance, and
    stores results of computations back into the problem object.

    The parameters are specified via the optional ``glp_param`` argument, which is of type ``GLPInteriorParam``
    (or ``nothing`` to use the default settings).

    Returns 0 in case of success, or a non-zero flag specifying the reason for failure: ``GLP_EFAIL`` (solver failure),
    ``GLP_ENOCVG`` (very slow convergence, or divergence), ``GLP_ITLIM`` (iterations limit exceeded),
    ``GLP_EINSTAB`` (numerical instability).

.. function:: glp_init_iptcp(glp_param)

    Initializes a ``GLPInteriorParam`` object with the default values. In Julia, this is done at object creation time; this
    function can be used to reset the object.

.. function:: glp_ipt_status(glp_prob)

    Returns the status of the interior-point solution: ``GLP_OPT`` (optimal),
    ``GLP_INFEAS`` (infeasible), ``GLP_NOFEAS`` (no feasible solution), ``GLP_UNDEF`` (undefined).

.. function:: glp_ipt_obj_val(glp_prob)

    Returns the current value of the objective function for the interior-point solution.

.. function:: glp_ipt_row_prim(glp_prob, row)

    Returns the primal value of the specified row for the interior-point solution.

.. function:: glp_ipt_row_dual(glp_prob, row)

    Returns the dual value (reduced cost) of the specified row for the interior-point solution.

.. function:: glp_ipt_col_prim(glp_prob, col)

    Returns the primal value of the specified column for the interior-point solution.

.. function:: glp_ipt_col_dual(glp_prob, col)

    Returns the dual value (reduced cost) of the specified column for the interior-point solution.

.. function:: glp_set_col_kind(glp_prob, col, kind)

    Sets the kind for the specified column (for mixed-integer programming). ``kind`` must be one of:
    ``GLP_CV`` (continuous), ``GLP_IV`` (integer), ``GLP_BV`` (binary, 0/1).

.. function:: glp_get_col_kind(glp_prob, col)

    Returns the kind for the specified column (see `glp_set_col_kind`).

.. function:: glp_get_num_int(glp_prob)

    Returns the number of columns marked as integer (including binary).

.. function:: glp_get_num_bin(glp_prob)

    Returns the number of columns marked binary.

.. function:: glp_intopt(glp_prob, [glp_param])

    The routine ``glp_intopt`` is a driver to the mixed-integer-programming (MIP) solver
    based on the branch- and-cut method, which is a hybrid of branch-and-bound
    and cutting plane methods.

    The parameters are specified via the optional ``glp_param`` argument, which is of type ``GLPIntoptParam``
    (or ``nothing`` to use the default settings).

    Returns 0 in case of success, or a non-zero flag specifying the reason for failure: ``GLP_EBOUND`` (incorrect bounds),
    ``GLP_EROOT`` (no optimal LP basis given), ``GLP_ENOPFS`` (no primal feasible LP solution), ``GLP_ENODFS`` (no dual
    feasible LP solution), ``GLP_EFAIL`` (solver failure), ``GLP_EMIPGAP`` (mip gap tolearance reached), ``GLP_ETLIM``
    (time limit exceeded), ``GLP_ESTOP`` (terminated by application).

.. function:: glp_init_iocp(glp_param)

    Initializes a ``GLPIntoptParam`` object with the default values. In Julia, this is done at object creation time; this
    function can be used to reset the object.

.. function:: glp_mip_status(glp_prob)

    Returns the generic status of the MIP solution: ``GLP_OPT`` (optimal),
    ``GLP_FEAS`` (feasible), ``GLP_NOFEAS`` (no feasible solution), ``GLP_UNDEF`` (undefined).

.. function:: glp_mip_obj_val(glp_prob)

    Returns the current value of the objective function for the MIP solution.

.. function:: glp_mip_row_val(glp_prob, row)

    Returns the value of the specified row for the MIP solution.

.. function:: glp_mip_col_val(glp_prob, col)

    Returns the value of the specified column for the MIP solution.

.. function:: glp_read_mps(glp_prob, format, [param,] filename)

    Reads problem data in MPS format from a text file. ``format`` must be one of ``GLP_MPS_DECK`` (fixed, old) or ``GLP_MPS_FILE``
    (free, modern). ``param`` is optional; if given it must be ``nothing``.

    Returns 0 upon success; throws an error in case of failure.

.. function:: glp_write_mps(glp_prob, format, [param,] filename)

    Writes problem data in MPS format from a text file. See ``glp_read_mps``.

    Returns 0 upon success; throws an error in case of failure.

.. function:: glp_read_lp(glp_prob, [param,] filename)

    Reads problem data in CPLEX LP format from a text file. ``param`` is optional; if given it must be ``nothing``.

    Returns 0 upon success; throws an error in case of failure.

.. function:: glp_write_lp(glp_prob, [param,] filename)

    Writes problem data in CPLEX LP format from a text file. See ``glp_read_lp``.

    Returns 0 upon success; throws an error in case of failure.

.. function:: glp_read_prob(glp_prob, [flags,] filename)

    Reads problem data in GLPK LP/MIP format from a text file. ``flags`` is optional; if given it must be 0.

    Returns 0 upon success; throws an error in case of failure.

.. function:: glp_write_prob(glp_prob, [flags,] filename)

    Writes problem data in GLPK LP/MIP format from a text file. See ``glp_read_prob``.

    Returns 0 upon success; throws an error in case of failure.

.. function:: glp_mpl_read_model(glp_tran, filename, skip)

    Reads the model section and, optionally, the data section, from a text file in MathProg format, and stores it
    in ``glp_tran``, which is a ``GLPMathProgWorkspace`` object. If ``skip`` is nonzero, the data section is skipped
    if present.

    Returns 0 upon success; throws an error in case of failure.

.. function:: glp_mpl_read_data(glp_tran, filename)

    Reads data section from a text file in MathProg format and stores it in ``glp_tran``, which is a
    ``GLPMathProgWorkspace`` object. May be called more than once.

    Returns 0 upon success; throws an error in case of failure.

.. function:: glp_mpl_generate(glp_tran, [filename])

    Generates the model using its description stored in the ``GLPMathProgWorkspace`` translator workspace ``glp_tran``.
    The optional ``filename`` specifies an output file; if not given or ``nothing``, the terminal is used.

    Returns 0 upon success; throws an error in case of failure.

.. function:: glp_mpl_build_prob(glp_tran, glp_prob)

    Transfer information from the ``GLPMathProgWorkspace`` translator workspace ``glp_tran`` to the ``GLPProb`` problem
    object ``glp_prob``.

.. function:: glp_mpl_postsolve(glp_tran, glp_prob, sol)

    Copies the solution from the ``GLPProb`` problem object ``glp_prob`` to the ``GLPMathProgWorkspace`` translator workspace
    ``glp_tran`` and then executes all the remaining model statements, which follow the solve statement.

    The parameter ``sol`` specifies which solution should be copied from the problem object to the workspace: ``GLP_SOL`` (basic),
    ``GLP_IPT`` (interior-point), ``GLP_MIP`` (MIP).

    Returns 0 upon success; throws an error in case of failure.

.. function:: glp_print_sol(glp_prob, filename)

    Writes the current basic solution to a text file, in printable format.

    Returns 0 upon success; throws an error in case of failure.

.. function:: glp_read_sol(glp_prob, filename)

    Reads the current basic solution from a text file, in the format used by ``glp_write_sol``.

    Returns 0 upon success; throws an error in case of failure.

.. function:: glp_write_sol(glp_prob, filename)

    Writes the current basic solution from a text file, in a format which can be read by ``glp_read_sol``.

    Returns 0 upon success; throws an error in case of failure.

.. function:: glp_print_ipt(glp_prob, filename)

    Writes the current interior-point solution to a text file, in printable format.

    Returns 0 upon success; throws an error in case of failure.

.. function:: glp_read_ipt(glp_prob, filename)

    Reads the current interior-point solution from a text file, in the format used by ``glp_write_ipt``.

    Returns 0 upon success; throws an error in case of failure.

.. function:: glp_write_ipt(glp_prob, filename)

    Writes the current interior-point solution from a text file, in a format which can be read by ``glp_read_ipt``.

    Returns 0 upon success; throws an error in case of failure.

.. function:: glp_print_mip(glp_prob, filename)

    Writes the current MIP solution to a text file, in printable format.

    Returns 0 upon success; throws an error in case of failure.

.. function:: glp_read_mip(glp_prob, filename)

    Reads the current MIP solution from a text file, in the format used by ``glp_write_mip``.

    Returns 0 upon success; throws an error in case of failure.

.. function:: glp_write_mip(glp_prob, filename)

    Writes the current MIP solution from a text file, in a format which can be read by ``glp_read_mip``.

    Returns 0 upon success; throws an error in case of failure.

.. function:: glp_print_ranges(glp_prob, [[len,] list,] [flags,] filename)

    Performs sensitivity analysis of current optimal basic solution and writes the analysis report
    in human-readable format to a text file. ``list`` is a vector specifying the rows/columns to analyze
    (if 1 <= list[i] <= rows, analyzes row list[i]; if rows+1 <= list[i] <= rows+cols, analyzes column
    list[i]-rows). ``len`` is the number of elements of ``list`` which will be consideres, and must be smaller
    or equal to the length of the list. In Julia, ``len`` is optional (it's inferred from ``len`` if not given).
    ``list`` can be empty of ``nothing`` or not given at all, implying all indices will be analyzed. ``flags`` is
    optional, and must be 0 if given.

    To call this function, the current basic solution must be optimal, and the basis factorization must exist.

    Returns 0 upon success, non-zero otherwise.

.. function:: glp_bf_exists(glp_prob)

    Returns non-zero if the basis fatorization for the current basis exists, 0 otherwise.

.. function:: glp_factorize(glp_prob)

    Computes the basis factorization for the current basis.

    Returns 0 if successful, otherwise: ``GLP_EBADB`` (invalid matrix), ``GLP_ESING`` (singluar matrix),
    ``GLP_ECOND`` (ill-conditioned matrix).

.. function:: glp_bf_updated(glp_prob)

    Returns 0 if the basis factorization was computed from scratch, non-zero otherwise.

.. function:: glp_get_bfcp(glp_prob, glp_param)

    Retrieves control parameters, which are used on computing and updating the basis factorization
    associated with the problem object, and stores them in the ``GLPBasisFactParam`` object ``glp_param``.

.. function:: glp_set_bfcp(glp_prob[, glp_param])

    Sets the control parameters stored in the ``GLPBasisFactParam`` object ``glp_param`` into the problem
    object. If ``glp_param`` is ``nothing`` or is omitted, resets the parameters to their defaults.

    The ``glp_param`` should always be retreived via ``glp_get_bfcp`` before changing its values and calling
    this function.

.. function:: glp_get_bhead(glp_prob, k)

    Returns the basis header information for the current basis. ``k`` is a row index.
    
    Returns either i such that 1 <= i <= rows, if ``k`` corresponds to i-th auxiliary variable,
    or rows+j such that 1 <= j <= columns, if ``k`` corresponds to the j-th structural variable.

.. function:: glp_get_row_bind(glp_prob, row)

    Returns the index of the basic variable ``k`` which is associated with the specified row, or 0 if
    the variable is non-basic. If ``glp_get_bhead(glp_prob, k) = row``, then ``glp_get_bind(glp_prob, row) = k``.

.. function:: glp_get_col_bind(glp_prob, col)

    Returns the index of the basic variable ``k`` which is associated with the specified column, or 0 if
    the variable is non-basic. If ``glp_get_bhead(glp_prob, k) = rows+col``, then ``glp_get_bind(glp_prob, col) = k``.

.. function:: glp_ftran(glp_prob, v)

    Performs forward transformation (FTRAN), i.e. it solves the system Bx = b, where B is the basis matrix,
    x is the vector of unknowns to be computed, b is the vector of right-hand sides. At input, ``v`` represents the
    vector b; at output, it contains the vector x. ``v`` must be a ``Vector{Float64}`` whose length is the number of rows.

.. function:: glp_btran(glp_prob, v)

    Performs backward transformation (BTRAN), i.e. it solves the system B'x = b, where B is the transposed of the basis
    matrix, x is the vector of unknowns to be computed, b is the vector of right-hand sides. At input, ``v`` represents the
    vector b; at output, it contains the vector x. ``v`` must be a ``Vector{Float64}`` whose length is the number of rows.

.. function:: glp_warm_up(glp_prob)

    "Warms up" the LP basis using current statuses assigned to rows and columns, i.e. computes factorization of the basis
    matrix (if it does not exist), computes primal and dual components of basic solution, and determines the solution status.

    Returns 0 if successful, otherwise: ``GLP_EBADB`` (invalid matrix), ``GLP_ESING`` (singluar matrix),
    ``GLP_ECOND`` (ill-conditioned matrix).

.. function:: glp_eval_tab_row(glp_prob, k, ind, val)
              glp_eval_tab_row(glp_prob, k)

    Computes a row of the current simplex tableau which corresponds to some basic variable specified by the parameter ``k``.
    If 1 <= ``k`` <= rows, uses ``k``-th auxiliary variable; if rows+1 <= ``k`` <= rows+cols, uses (``k``-rows)-th structural
    variable. The basis factorization must exist.

    In the first form, stores the result in the provided vectors ``ind`` and ``val``, which must be of type ``Vector{Int32}`` and
    ``Vector{Float64}``, respectively, and returns the length of the outcome; in Julia, the vectors will be resized as needed to hold
    the result.

    In the second, simpler form, ``ind`` and ``val`` are returned in a tuple as the output of the function.

.. function:: glp_eval_tab_col(glp_prob, k, ind, val)
              glp_eval_tab_col(glp_prob, k)

    Computes a column of the current simplex tableau which corresponds to some non-basic variable specified by the parameter ``k``.
    See ``glp_eval_tab_row``.

.. function:: glp_transform_row(glp_prob, [len,] ind, val)

    Performs the same operation as ``glp_eval_tab_row`` with the exception that the row to be transformed is specified
    explicitly as a sparse vector. The parameter ``len`` is the number of elements of ``ind`` and ``val`` which will be used,
    and must be smaller or equal to the length of both vectors; in Julia it is optional (and the ``ind`` and ``val`` must have the
    same length). The vectors ``int`` and ``val`` must be of type ``Vector{Int32}`` and ``Vector{Float64}``, respectively, since
    they will also hold the result; in Julia, they will be resized to the resulting required length.

    Returns the length if the resulting vectors ``ind`` and ``val``.

.. function:: glp_transform_col(glp_prob, [len,] ind, val)

    Performs the same operation as ``glp_eval_tab_col`` with the exception that the row to be transformed is specified
    explicitly as a sparse vector. See ``glp_transform_row``.

.. function:: glp_prim_rtest(glp_prob, [len,] ind, val, dir, eps)

    Performs the primal ratio test using an explicitly specified column of the simplex table.
    The current basic solution must be primal feasible.
    The column is specified in sparse format by ``len`` (length of the vector), ``ind`` and ``val`` (indices and values of
    the vector). ``len`` is the number of elements which will be considered and must be smaller or equal to the length of
    both ``ind`` and ``val``; in Julia, it can be omitted (and then ``ind`` and ``val`` must have the same length).
    The indices in ``ind`` must be between 1 and rows+cols; they must correspond to basic variables.
    ``dir`` is a direction parameter which must be either +1 (increasing) or -1 (decreasing).
    ``eps`` is a tolerance parameter and must be positive.
    See the GLPK manual for a detailed explanation.

    Returns the position in ``ind`` and ``val`` which corresponds to the pivot element, or 0 if the choice cannot be made.

.. function:: glp_dual_rtest(glp_prob, [len,] ind, val, dir, eps)

    Performs the dual ratio test using an explicitly specified row of the simplex table.
    The current basic solution must be dual feasible.
    The indices in ``ind`` must correspond to non-basic variables.
    Everything else is like in ``glp_prim_rtest``.

.. function:: glp_analyze_bound(glp_prob, k)

    Analyzes the effect of varying the active bound of specified non-basic variable. See the GLPK manual for a
    detailed explanation.
    In Julia, this function has a different API then C. It returns ``(limit1, var1, limit2, var2)`` rather
    then taking them as pointers in the argument list.

.. function:: glp_analyze_coef(glp_prob, k)

    Analyzes the effect of varying the objective coefficient at specified basic variable. See the GLPK manual for a
    detailed explanation.
    In Julia, this function has a different API then C. It returns
    ``(coef1, var1, value1, coef2, var2, value2)`` rather then taking them as pointers in the argument list.

.. function:: glp_init_env()

    Initializes the GLPK environment. Not normally needed.

    Returns 0 (initilization successful), 1 (environment already initialized), 2 (failed, insufficient memory) or
    3 (failed, unsupported programming model).

.. function:: glp_version()

    Returns the GLPK version number. In Julia, instead of returning a string as in C, it returns a tuple of integer
    values, containing the major and the minor number.  

.. function:: glp_free_env()

    Frees all resources used by GLPK routines (memory blocks, etc.) which are currently still in use. Not normally needed.

    Returns 0 if successful, 1 if envirnoment is inactive.

.. function:: glp_term_out(flag)

    Enables/disables the terminal output of glpk routines. ``flag`` is either ``GLP_ON`` (output enabled) or ``GLP_OFF``
    (output disabled).

    Returns the previous status of the terminal output.

.. function:: glp_open_tee(filename)

    Starts copying all the terminal output to an output text file.

    Returns 0 if successful, 1 if already active, 2 if it fails creating the output file.

.. function:: glp_close_tee()

    Stops copying the terminal output to the output text file previously open by the ``glp_open_tee``.

    Return 0 if successful, 1 if copying terminal output was not started.

.. function:: glp_malloc(size)

    Replacement of standard C ``malloc``. Allocates uninitialized memeory which must freed with ``glp_free``.

    Returns a pointer to the allocated memory.

.. function:: glp_calloc(n, size)

    Replacement of standard C ``calloc``, but does not initialize the memeory.
    Allocates uninitialized memeory which must freed with ``glp_free``.

    Returns a pointer to the allocated memory.

.. function:: glp_free(ptr)

    Deallocates a memory block previously allocated by ``glp_malloc`` or ``glp_calloc``.

.. function:: glp_mem_usage()

    Reports some information about utilization of the memory by the routines ``glp_malloc``, ``glp_calloc``,
    and ``glp_free``.
    In Julia, this function has a different API then C. It returns ``(count, cpeak, total, tpeak)`` rather
    then taking them as pointers in the argument list.

.. function:: glp_mem_limit(limit)

    Limits the amount of memory avaliable for dynamic allocation to a value in megabyes given by the integer
    parameter ``limit``.

.. function:: glp_time()

    Returns the current universal time (UTC), in milliseconds.

.. function:: glp_difftime(t1, t0)

    Returns the difference between two time values ``t1`` and ``t0``, expressed in seconds.

.. function:: glp_sdf_open_file(filename)

    Opens a plain data file.

    If successful, returns a GLPData() object, otherwise throws an error.

.. function:: glp_sdf_read_int(glp_data)

    Reads an integer number from the plain data file specified by the ``GLPData`` parameter ``glp_data``, skipping initial
    whitespace.

.. function:: glp_sdf_read_num(glp_data)

    Reads a floating point number from the plain data file specified by the ``GLPData`` parameter ``glp_data``, skipping initial
    whitespace.

.. function:: glp_sdf_read_item(glp_data)

    Reads a data item (a String) from the plain data file specified by the ``GLPData`` parameter ``glp_data``, skipping initial
    whitespace.

.. function:: glp_sdf_read_text(glp_data)

    Reads a line of text from the plain data file specified by the ``GLPData`` parameter ``glp_data``, skipping initial and final
    whitespace.

.. function:: glp_sdf_line(glp_data)

    Returns the current line in the GLPData object ``glp_data``

.. function:: glp_sdf_close_file(glp_data)

    Closes the file associated to ``glp_data`` and frees the resources.

.. function:: glp_read_cnfsat(glp_prob, filename)

    Reads the CNF-SAT problem data in DIMACS format from a text file.

    Returns 0 upon success; throws an error in case of failure.

.. function:: glp_check_cnfsat(glp_prob)

    Checks if the problem object encodes a CNF-SAT problem instance, in which case it returns 0,
    otherwise returns non-zero.

.. function:: glp_write_cnfsat(glp_prob, filename)

    Writes the CNF-SAT problem data in DIMACS format into a text file.

    Returns 0 upon success; throws an error in case of failure.

.. function:: glp_minisat1(glp_prob)

    The routine `glp_minisat1` is a driver to MiniSat, a CNF-SAT solver developed by
    Niklas Eén and Niklas Sörensson, Chalmers University of Technology, Sweden.

    Returns 0 in case of success, or a non-zero flag specifying the reason for failure: ``GLP_EDATA``
    (problem is not CNF-SAT), ``GLP_EFAIL`` (solver failure).

.. function:: glp_intfeas1(glp_prob, use_bound, obj_bound)

    The routine glp_intfeas1 is a tentative implementation of an integer feasibility solver
    based on a CNF-SAT solver (currently MiniSat). ``use_bound`` is a flag: if zero, any feasible solution
    is seeked, otherwise seraches for an integer feasible solution. ``obj_bound`` is used only if
    ``use_bound`` is non-zero, and specifies an upper/lower bound (for maximization/minimazion respectively)
    to the objective function.

    All variables (columns) must either be binary or fixed. All constraint and objective coeffient
    must be integer.

    Returns 0 in case of success, or a non-zero flag specifying the reason for failure: ``GLP_EDATA``
    (problem data is not valid), ``GLP_ERANGE`` (integer overflow occurred), ``GLP_EFAIL`` (solver failure).
