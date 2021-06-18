# Sparse Linear Algebra

```@meta
DocTestSetup = :(using LinearAlgebra, SparseArrays, SuiteSparse)
```

Sparse matrix solvers call functions from [SuiteSparse](http://suitesparse.com). The following factorizations are available:

| Type                              | Description                                   |
|:--------------------------------- |:--------------------------------------------- |
| `SuiteSparse.CHOLMOD.Factor`      | Cholesky factorization                        |
| `SuiteSparse.UMFPACK.UmfpackLU`   | LU factorization                              |
| `SuiteSparse.SPQR.QRSparse`       | QR factorization                              |

Other solvers such as [Pardiso.jl](https://github.com/JuliaSparse/Pardiso.jl/) are as external packages. [Arpack.jl](https://julialinearalgebra.github.io/Arpack.jl/stable/) provides `eigs` and `svds` for iterative solution of eigensystems and singular value decompositions.

These factorizations are described in the [`Linear Algebra`](@ref man-linalg) section of the manual:
1. [`cholesky`](@ref)
2. [`ldlt`](@ref)
3. [`lu`](@ref)
4. [`qr`](@ref)

```@docs
SuiteSparse.CHOLMOD.lowrankupdate
SuiteSparse.CHOLMOD.lowrankupdate!
SuiteSparse.CHOLMOD.lowrankdowndate
SuiteSparse.CHOLMOD.lowrankdowndate!
SuiteSparse.CHOLMOD.lowrankupdowndate!
```


```@meta
DocTestSetup = nothing
```
