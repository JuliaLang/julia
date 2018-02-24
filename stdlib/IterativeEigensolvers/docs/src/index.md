# [Iterative Eigensolvers](@id lib-itereigen)

```@meta
DocTestSetup = :(using IterativeEigensolvers)
```

Julia provides bindings to [ARPACK](http://www.caam.rice.edu/software/ARPACK/), which
can be used to perform iterative solutions for eigensystems (using [`eigs`](@ref))
or singular value decompositions (using [`svds`](@ref)).

`eigs` calculates the eigenvalues and, optionally, eigenvectors of its input(s)
using implicitly restarted Lanczos or Arnoldi iterations for real symmetric or
general nonsymmetric matrices respectively.

For the single matrix version,

`eigs(A; nev=6, ncv=max(20,2*nev+1), which=:LM, tol=0.0, maxiter=300, sigma=nothing, ritzvec=true, v0=zeros((0,))) -> (d,[v,],nconv,niter,nmult,resid)`

the following keyword arguments are supported:

* `nev`: Number of eigenvalues
* `ncv`: Number of Krylov vectors used in the computation; should satisfy `nev+1 <= ncv <= n`
  for real symmetric problems and `nev+2 <= ncv <= n` for other problems, where `n` is the
  size of the input matrix `A`. The default is `ncv = max(20,2*nev+1)`. Note that these
  restrictions limit the input matrix `A` to be of dimension at least 2.
* `which`: type of eigenvalues to compute. See the note below.

| `which` | type of eigenvalues                                                                                                       |
|:--------|:--------------------------------------------------------------------------------------------------------------------------|
| `:LM`   | eigenvalues of largest magnitude (default)                                                                                |
| `:SM`   | eigenvalues of smallest magnitude                                                                                         |
| `:LR`   | eigenvalues of largest real part                                                                                          |
| `:SR`   | eigenvalues of smallest real part                                                                                         |
| `:LI`   | eigenvalues of largest imaginary part (nonsymmetric or complex `A` only)                                                  |
| `:SI`   | eigenvalues of smallest imaginary part (nonsymmetric or complex `A` only)                                                 |
| `:BE`   | compute half of the eigenvalues from each end of the spectrum, biased in favor of the high end. (real symmetric `A` only) |

* `tol`: parameter defining the relative tolerance for convergence of Ritz values (eigenvalue estimates).
     A Ritz value ``θ`` is considered converged when its associated residual
     is less than or equal to the product of `tol` and ``max(ɛ^{2/3}, |θ|)``,
     where `ɛ = eps(real(eltype(A)))/2` is LAPACK's machine epsilon.
     The residual associated with ``θ`` and its corresponding Ritz vector ``v``
     is defined as the norm ``||Av - vθ||``.
     The specified value of `tol` should be positive; otherwise, it is ignored
     and ``ɛ`` is used instead.
     Default: ``ɛ``.

* `maxiter`: Maximum number of iterations (default = 300)
* `sigma`: Specifies the level shift used in inverse iteration. If `nothing` (default),
  defaults to ordinary (forward) iterations. Otherwise, find eigenvalues close to `sigma`
  using shift and invert iterations.
* `ritzvec`: Returns the Ritz vectors `v` (eigenvectors) if `true`
* `v0`: starting vector from which to start the iterations

We can see the various keywords in action in the following examples:
```jldoctest
julia> using IterativeEigensolvers

julia> A = Diagonal(1:4);

julia> λ, ϕ = eigs(A, nev = 2, which=:SM);

julia> λ
2-element Array{Float64,1}:
 1.0000000000000002
 2.0

julia> B = Diagonal([1., 2., -3im, 4im])
4×4 Diagonal{Complex{Float64},Array{Complex{Float64},1}}:
 1.0+0.0im      ⋅          ⋅          ⋅
     ⋅      2.0+0.0im      ⋅          ⋅
     ⋅          ⋅      0.0-3.0im      ⋅
     ⋅          ⋅          ⋅      0.0+4.0im

julia> λ, ϕ = eigs(B, nev=1, which=:LI);

julia> λ
1-element Array{Complex{Float64},1}:
 -4.440892098500626e-16 + 3.999999999999998im

julia> λ, ϕ = eigs(B, nev=1, which=:SI);

julia> λ
1-element Array{Complex{Float64},1}:
 1.3877787807814457e-16 - 2.999999999999999im

julia> λ, ϕ = eigs(B, nev=1, which=:LR);

julia> λ
1-element Array{Complex{Float64},1}:
 2.0 + 4.242754940683747e-17im

julia> λ, ϕ = eigs(B, nev=1, which=:SR);

julia> λ
1-element Array{Complex{Float64},1}:
 4.440892098500626e-16 + 4.0000000000000036im

julia> λ, ϕ = eigs(B, nev=1, sigma=1.5);

julia> λ
1-element Array{Complex{Float64},1}:
 1.9999999999999996 + 2.4290457684137336e-17im
```

!!! note
    The `sigma` and `which` keywords interact: the description of eigenvalues
    searched for by `which` do *not* necessarily refer to the eigenvalues of
    `A`, but rather the linear operator constructed by the specification of the
    iteration mode implied by `sigma`.

    | `sigma`         | iteration mode                   | `which` refers to eigenvalues of |
    |:----------------|:---------------------------------|:---------------------------------|
    | `nothing`       | ordinary (forward)               | ``A``                            |
    | real or complex | inverse with level shift `sigma` | ``(A - \\sigma I )^{-1}``        |

!!! note
    Although `tol` has a default value, the best choice depends strongly on the
    matrix `A`. We recommend that users _always_ specify a value for `tol`
    which suits their specific needs.

    For details of how the errors in the computed eigenvalues are estimated, see:

    * B. N. Parlett, "The Symmetric Eigenvalue Problem", SIAM: Philadelphia, 2/e
      (1998), Ch. 13.2, "Accessing Accuracy in Lanczos Problems", pp. 290-292 ff.
    * R. B. Lehoucq and D. C. Sorensen, "Deflation Techniques for an Implicitly
      Restarted Arnoldi Iteration", SIAM Journal on Matrix Analysis and
      Applications (1996), 17(4), 789–821.  doi:10.1137/S0895479895281484

For the two-input generalized eigensolution version,

`eigs(A, B; nev=6, ncv=max(20,2*nev+1), which=:LM, tol=0.0, maxiter=300, sigma=nothing, ritzvec=true, v0=zeros((0,))) -> (d,[v,],nconv,niter,nmult,resid)`

the following keyword arguments are supported:

* `nev`: Number of eigenvalues
* `ncv`: Number of Krylov vectors used in the computation; should satisfy `nev+1 <= ncv <= n`
  for real symmetric problems and `nev+2 <= ncv <= n` for other problems, where `n` is the
  size of the input matrices `A` and `B`. The default is `ncv = max(20,2*nev+1)`. Note that
  these restrictions limit the input matrix `A` to be of dimension at least 2.
* `which`: type of eigenvalues to compute. See the note below.

| `which` | type of eigenvalues                                                                                                       |
|:--------|:--------------------------------------------------------------------------------------------------------------------------|
| `:LM`   | eigenvalues of largest magnitude (default)                                                                                |
| `:SM`   | eigenvalues of smallest magnitude                                                                                         |
| `:LR`   | eigenvalues of largest real part                                                                                          |
| `:SR`   | eigenvalues of smallest real part                                                                                         |
| `:LI`   | eigenvalues of largest imaginary part (nonsymmetric or complex `A` only)                                                  |
| `:SI`   | eigenvalues of smallest imaginary part (nonsymmetric or complex `A` only)                                                 |
| `:BE`   | compute half of the eigenvalues from each end of the spectrum, biased in favor of the high end. (real symmetric `A` only) |

* `tol`: relative tolerance used in the convergence criterion for eigenvalues, similar to
     `tol` in the [`eigs(A)`](@ref) method for the ordinary eigenvalue
     problem, but effectively for the eigenvalues of ``B^{-1} A`` instead of ``A``.
     See the documentation for the ordinary eigenvalue problem in
     [`eigs(A)`](@ref) and the accompanying note about `tol`.
* `maxiter`: Maximum number of iterations (default = 300)
* `sigma`: Specifies the level shift used in inverse iteration. If `nothing` (default),
  defaults to ordinary (forward) iterations. Otherwise, find eigenvalues close to `sigma`
  using shift and invert iterations.
* `ritzvec`: Returns the Ritz vectors `v` (eigenvectors) if `true`
* `v0`: starting vector from which to start the iterations

`eigs` returns the `nev` requested eigenvalues in `d`, the corresponding Ritz vectors `v`
(only if `ritzvec=true`), the number of converged eigenvalues `nconv`, the number of
iterations `niter` and the number of matrix vector multiplications `nmult`, as well as the
final residual vector `resid`.

We can see the various keywords in action in the following examples:
```jldoctest
julia> using IterativeEigensolvers

julia> A = sparse(1.0I, 4, 4); B = Diagonal(1:4);

julia> λ, ϕ = eigs(A, B, nev = 2);

julia> λ
2-element Array{Float64,1}:
 1.0
 0.4999999999999999

julia> A = sparse(1.0I, 4, 4); B = Diagonal([1, -2im, 3, 4im]);

julia> λ, ϕ = eigs(A, B, nev=1, which=:SI);

julia> λ
1-element Array{Complex{Float64},1}:
 0.03291282838780993 - 2.0627621271174514im

julia> λ, ϕ = eigs(A, B, nev=1, which=:LI);

julia> λ
1-element Array{Complex{Float64},1}:
 -0.6428551411711136 + 2.1820633510068994im
```

!!! note
    The `sigma` and `which` keywords interact: the description of eigenvalues searched for by
    `which` do *not* necessarily refer to the eigenvalue problem ``Av = Bv\\lambda``, but rather
    the linear operator constructed by the specification of the iteration mode implied by `sigma`.

    | `sigma`         | iteration mode                   | `which` refers to the problem      |
    |:----------------|:---------------------------------|:-----------------------------------|
    | `nothing`       | ordinary (forward)               | ``Av = Bv\\lambda``                |
    | real or complex | inverse with level shift `sigma` | ``(A - \\sigma B )^{-1}B = v\\nu`` |


```@docs
IterativeEigensolvers.eigs(::Any)
IterativeEigensolvers.eigs(::Any, ::Any)
IterativeEigensolvers.svds
```

```@meta
DocTestSetup = nothing
```
