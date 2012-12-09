.. _man-sparse-matrices:

******************
 Sparse Matrices
******************

`Sparse matrices <http://en.wikipedia.org/wiki/Sparse_matrix>`_ are
matrices that are primarily populated with zeros. Sparse matrices may
be used when operations on the sparse representation of a matrix lead
to considerable gains in either time or space when compared to
performing the same operations on a dense matrix.

Compressed Sparse Column (CSC) Storage
--------------------------------------

In julia, sparse matrices are stored in the `Compressed Sparse Column
(CSC) format
<http://en.wikipedia.org/wiki/Sparse_matrix#Compressed_sparse_column_.28CSC_or_CCS.29>`_. Julia
sparse matrices have the type ``SparseMatrixCSC{Tv,Ti}``, where ``Tv``
is the type of the nonzero values, and ``Ti`` is the integer type for
storing column pointers and row indices. 
::

    type SparseMatrixCSC{Tv,Ti<:Integer} <: AbstractSparseMatrix{Tv,Ti}
        m::Int                  # Number of rows
        n::Int                  # Number of columns
        colptr::Vector{Ti}      # Column i is in colptr[i]:(colptr[i+1]-1)
        rowval::Vector{Ti}      # Row values of nonzeros
        nzval::Vector{Tv}       # Nonzero values
    end

The compressed sparse column storage makes it easy and quick to access
the elements in the column of a sparse matrix, whereas accessing the
sparse matrix by rows is considerably slower. Operations such as
insertion of nonzero values one at a time in the CSC structure tend to
be slow. This is because all elements of the sparse matrix that are
beyond the point of insertion have to be moved one place over.

All operations on sparse matrices are carefully implemented to exploit
the CSC data structure for performance, and to avoid expensive operations.

Sparse matrix constructors
--------------------------

The simplest way to create sparse matrices are using functions
equivalent to the ``zeros`` and ``eye`` functions that Julia provides
for working with dense matrices. To produce sparse matrices instead,
you can use the same names with an ``sp`` prefix:

::

    julia> spzeros(3,5)
    3x5 sparse matrix with 0 nonzeros:

    julia> speye(3,5)
    3x5 sparse matrix with 3 nonzeros:
        [1, 1]  =  1.0
        [2, 2]  =  1.0
        [3, 3]  =  1.0

The ``sparse`` function is often a handy way to construct sparse
matrices. It takes as its input a vector ``I`` of row indices, a
vector ``J`` of column indices, and a vector ``V`` of nonzero
values. ``sparse(I,J,V)`` constructs a sparse matrix such that
``S[I[k], J[k]] = V[k]``.

::

    julia> I = [1, 4, 3, 5]; J = [4, 7, 18, 9]; V = [1, 2, -5, 3];

    julia> sparse(I,J,V)
    5x18 sparse matrix with 4 nonzeros:
         [1 ,  4]  =  1
         [4 ,  7]  =  2
         [5 ,  9]  =  3
         [3 , 18]  =  -5

The inverse of the ``sparse`` function is ``findn``, which
retrieves the inputs used to create the sparse matrix.

::

    julia> findn(S)
    ([1, 4, 5, 3],[4, 7, 9, 18])

    julia> findn_nzs(S)
    ([1, 4, 5, 3],[4, 7, 9, 18],[1, 2, 3, -5])

Another way to create sparse matrices is to convert a dense matrix
into a sparse matrix using the ``sparse`` function:

::

    julia> sparse(eye(5))
    5x5 sparse matrix with 5 nonzeros:
        [1, 1]  =  1.0
        [2, 2]  =  1.0
        [3, 3]  =  1.0
        [4, 4]  =  1.0
        [5, 5]  =  1.0

You can go in the other direction using the ``dense`` or the ``full``
function. The ``issparse`` function can be used to query if a matrix
is sparse.

::

    julia> issparse(speye(5))
    true

Sparse matrix operations
------------------------

Arithmetic operations on sparse matrices also work as they do on dense
matrices. Indexing of, assignment into, and concatenation of sparse
matrices work in the same way as dense matrices. Indexing operations,
especially assignment, are expensive, when carried out one element at
a time. In many cases it may be better to convert the sparse matrix
into ``(I,J,V)`` format using ``find_nzs``, manipulate the nonzeros or
the structure in the dense vectors ``(I,J,V)``, and then reconstruct
the sparse matrix.
