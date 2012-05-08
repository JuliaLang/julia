##
## Linear Programming and Mixed Integer Programming interfaces
## for optimization and constraint satisfaction problems
##

# note: be sure to load "sparse.jl" and "glpk.jl" before this file

# General notes: the interface is provided as a collection of
# high-level functions which use the glpk library.
# Most functions have almost same interface
# as matlab's linprog, e.g.:
#
# (z, x, flag) = linprog(f, A, b, Aeq, beq, params)
#
# There's a function for each solving method:
#   linprog_interior
#   linprog_simplex
#   linprog_exact
#   mixintprog
#
# The function linprog is an alias to linprog_interior
#
# These functions all seek to solve the problem
#
#   z = min_{x} (f' * x)
#
# where the vector x is subject to these constraints:
#
#   A * x <= b
#   Aeq * x == b
#   lb <= x <= ub
#
# The return flag is 0 in case of success, and follows
# the glpk library convention otherwise.
# In case of failure, z and x are set to nothing, otherwise
# they will hold the solution found
#
# The parameters Aeq, beq, lb, ub and params are optional.
# This mean they can be either passed as the constant 'nothing'
# or as an empty vector [] or not provided at all.
#
# Some methods have slightly different function calls, see
# individual notes for additional information

typealias SparseOrFullMat{T} Union(Matrix{T}, SparseMatrixCSC{T})
typealias MatOrNothing Union(Matrix, SparseMatrixCSC, Vector{None}, Nothing)

# Linear Programming, Interior point method (default)
#{{{

function linprog_interior{T<:Real,P<:Union(GLPInteriorParam,Nothing)}(f::Vector{T}, A::SparseOrFullMat{T}, b::Vector{T},
        Aeq::SparseOrFullMat{T}, beq::Vector{T}, lb::Vector{T}, ub::Vector{T}, params::P)

    lp, n = _jl_linprog__setup_prob(f, A, b, Aeq, beq, lb, ub, params)

    ret = glp_interior(lp, params)
    #println("ret=$ret")

    if ret == 0
        z = glp_ipt_obj_val(lp)
        x = zeros(Float64, n)
        for c = 1 : n
            x[c] = glp_ipt_col_prim(lp, c)
        end
        return (z, x, ret)
    else
        # throw exception here ?
        return (nothing, nothing, ret)
    end
end
function linprog_interior{T<:Real,P<:Union(GLPInteriorParam,Nothing)}(f::Vector{T}, A::MatOrNothing, b::VecOrNothing,
        Aeq::MatOrNothing, beq::VecOrNothing, lb::VecOrNothing, ub::VecOrNothing, params::P)

    cA = _jl_linprog__convert_matornothing(T, A)
    cb = _jl_linprog__convert_vecornothing(T, b)
    cAeq = _jl_linprog__convert_matornothing(T, Aeq)
    cbeq = _jl_linprog__convert_vecornothing(T, beq)
    clb = _jl_linprog__convert_vecornothing(T, lb)
    cub = _jl_linprog__convert_vecornothing(T, ub)
    return linprog_interior(f, cA, cb, cAeq, cbeq, clb, cub, params)

end

linprog_interior{T<:Real}(f::Vector{T}, A::MatOrNothing, b::VecOrNothing) =
        linprog_interior(f, A, b, nothing, nothing, nothing, nothing, nothing)

linprog_interior{T<:Real}(f::Vector{T}, A::MatOrNothing, b::VecOrNothing,
        Aeq::MatOrNothing, beq::VecOrNothing) =
        linprog_interior(f, A, b, Aeq, beq, nothing, nothing, nothing)

linprog_interior{T<:Real}(f::Vector{T}, A::MatOrNothing, b::VecOrNothing,
        Aeq::MatOrNothing, beq::VecOrNothing, lb::VecOrNothing,
        ub::VecOrNothing) =
        linprog_interior(f, A, b, Aeq, beq, lb, ub, nothing)

linprog = linprog_interior
#}}}

# Linear Programming, Simplex Method
#{{{
function linprog_simplex{T<:Real,P<:Union(GLPSimplexParam,Nothing)}(f::Vector{T}, A::SparseOrFullMat{T}, b::Vector{T},
        Aeq::SparseOrFullMat{T}, beq::Vector{T}, lb::Vector{T}, ub::Vector{T}, params::P)

    lp, n = _jl_linprog__setup_prob(f, A, b, Aeq, beq, lb, ub, params)

    ret = glp_simplex(lp, params)
    #println("ret=$ret")

    if ret == 0
        z = glp_get_obj_val(lp)
        x = zeros(Float64, n)
        for c = 1 : n
            x[c] = glp_get_col_prim(lp, c)
        end
        return (z, x, ret)
    else
        # throw exception here ?
        return (nothing, nothing, ret)
    end
end
function linprog_simplex{T<:Real,P<:Union(GLPSimplexParam,Nothing)}(f::Vector{T}, A::MatOrNothing, b::VecOrNothing,
        Aeq::MatOrNothing, beq::VecOrNothing, lb::VecOrNothing, ub::VecOrNothing, params::P)

    cA = _jl_linprog__convert_matornothing(T, A)
    cb = _jl_linprog__convert_vecornothing(T, b)
    cAeq = _jl_linprog__convert_matornothing(T, Aeq)
    cbeq = _jl_linprog__convert_vecornothing(T, beq)
    clb = _jl_linprog__convert_vecornothing(T, lb)
    cub = _jl_linprog__convert_vecornothing(T, ub)
    return linprog_simplex(f, cA, cb, cAeq, cbeq, clb, cub, params)

end

linprog_simplex{T<:Real}(f::Vector{T}, A::MatOrNothing, b::VecOrNothing) =
        linprog_simplex(f, A, b, nothing, nothing, nothing, nothing, nothing)

linprog_simplex{T<:Real}(f::Vector{T}, A::MatOrNothing, b::VecOrNothing,
        Aeq::MatOrNothing, beq::VecOrNothing) =
        linprog_simplex(f, A, b, Aeq, beq, nothing, nothing, nothing)

linprog_simplex{T<:Real}(f::Vector{T}, A::MatOrNothing, b::VecOrNothing,
        Aeq::MatOrNothing, beq::VecOrNothing, lb::VecOrNothing,
        ub::VecOrNothing) =
        linprog_simplex(f, A, b, Aeq, beq, lb, ub, nothing)
#}}}

# Linear Programming, Simplex-exact Method
#{{{

# Notes:
#  * uses glp_simplex as a preliminary step
#  * the exact step only accepts the "it_lim" and "tm_lim" options,
#    which means no message suppression is possible

function linprog_exact{T<:Real,P<:Union(GLPSimplexParam,Nothing)}(f::Vector{T}, A::SparseOrFullMat{T}, b::Vector{T},
        Aeq::SparseOrFullMat{T}, beq::Vector{T}, lb::Vector{T}, ub::Vector{T}, params::P)

    lp, n = _jl_linprog__setup_prob(f, A, b, Aeq, beq, lb, ub, params)

    ret = glp_simplex(lp, params)
    if ret != 0
        # throw exception here ?
        return (nothing, nothing, ret)
    end

    ret = glp_exact(lp, params)
    if ret == 0
        z = glp_get_obj_val(lp)
        x = zeros(Float64, n)
        for c = 1 : n
            x[c] = glp_get_col_prim(lp, c)
        end
        return (z, x, ret)
    else
        # throw exception here ?
        return (nothing, nothing, ret)
    end
end
function linprog_exact{T<:Real,P<:Union(GLPSimplexParam,Nothing)}(f::Vector{T}, A::MatOrNothing, b::VecOrNothing,
        Aeq::MatOrNothing, beq::VecOrNothing, lb::VecOrNothing, ub::VecOrNothing, params::P)

    cA = _jl_linprog__convert_matornothing(T, A)
    cb = _jl_linprog__convert_vecornothing(T, b)
    cAeq = _jl_linprog__convert_matornothing(T, Aeq)
    cbeq = _jl_linprog__convert_vecornothing(T, beq)
    clb = _jl_linprog__convert_vecornothing(T, lb)
    cub = _jl_linprog__convert_vecornothing(T, ub)
    return linprog_exact(f, cA, cb, cAeq, cbeq, clb, cub, params)

end

linprog_exact{T<:Real}(f::Vector{T}, A::MatOrNothing, b::VecOrNothing) =
        linprog_exact(f, A, b, nothing, nothing, nothing, nothing, nothing)

linprog_exact{T<:Real}(f::Vector{T}, A::MatOrNothing, b::VecOrNothing,
        Aeq::MatOrNothing, beq::VecOrNothing) =
        linprog_exact(f, A, b, Aeq, beq, nothing, nothing, nothing)

linprog_exact{T<:Real}(f::Vector{T}, A::MatOrNothing, b::VecOrNothing,
        Aeq::MatOrNothing, beq::VecOrNothing, lb::VecOrNothing,
        ub::VecOrNothing) =
        linprog_exact(f, A, b, Aeq, beq, lb, ub, nothing)
#}}}

# Mixed Integer Programming
#{{{

# Notes:
#  * same syntax as linprog algorithms, with an additional col_kind vector and
#    an additional set of parameters for the presolve step; and an additional
#    return flag for the presolve step:
#
#    (z, x, flag, ps_flag) = mixintprog(f, A, b, Aeq, beq, lb, ub, col_kind, param, ps_param)
#
#  * if the col_kind vector is not provided, all variables default to integer
#  * if the "presolve" options is set to GLP_OFF, then it uses linear programming
#    for presolving:
#    + if the presolve_params is not given, uses the simplex point method with
#      default options
#    + it the presolve_params is given, its type is used to determine which
#      method to use (GLPSimplexParam -> simplex, GLPInteriorParam -> interior
#      point)

function mixintprog{T<:Real,Ti<:Integer,P<:Union(GLPIntoptParam,Nothing),Px<:Union(GLPParam,Nothing)}(
        f::Vector{T}, A::SparseOrFullMat{T}, b::Vector{T}, Aeq::SparseOrFullMat{T}, beq::Vector{T},
        lb::Vector{T}, ub::Vector{T}, col_kind::Vector{Ti}, params::P, params_presolve::Px)

    lp, n = _jl_linprog__setup_prob(f, A, b, Aeq, beq, lb, ub, params)
    _jl_mixintprog_set_col_kind(lp, n, col_kind)

    if params == nothing || pointer(params) == C_NULL || params["presolve"] != GLP_ON
        if params_presolve == nothing || isa(params_presolve, GLPSimplexParam)
            ret_ps = glp_simplex(lp, params_presolve)
        elseif  isa(params_presolve, GLPInteriorParam)
            # TODO check whether this is possible at all
            # or simplex is always strictly necessary
            println("warning: this probably won't work")
            ret_ps = glp_interior(lp, params_presolve)
        else
            error("wrong type for params_presolve")
        end
        if ret_ps != 0
            # throw exception here ?
            # XXX GLP_ESTOP ??
            return (nothing, nothing, GLP_ESTOP, ret_ps)
        end
    else
        ret_ps = 0
    end

    ret = glp_intopt(lp, params)
    if ret == 0
        z = glp_mip_obj_val(lp)
        x = zeros(Float64, n)
        for c = 1 : n
            x[c] = glp_mip_col_val(lp, c)
        end
        return (z, x, ret, ret_ps)
    else
        # throw exception here ?
        return (nothing, nothing, ret, ret_ps)
    end
end
function mixintprog{T<:Real,P<:Union(GLPIntoptParam,Nothing),Px<:Union(GLPParam,Nothing)}(
        f::Vector{T}, A::MatOrNothing, b::VecOrNothing, Aeq::MatOrNothing, beq::VecOrNothing,
        lb::VecOrNothing, ub::VecOrNothing, col_kind::VecOrNothing, params::P, params_presolve::Px)

    cA = _jl_linprog__convert_matornothing(T, A)
    cb = _jl_linprog__convert_vecornothing(T, b)
    cAeq = _jl_linprog__convert_matornothing(T, Aeq)
    cbeq = _jl_linprog__convert_vecornothing(T, beq)
    clb = _jl_linprog__convert_vecornothing(T, lb)
    cub = _jl_linprog__convert_vecornothing(T, ub)
    ccol_kind = _jl_linprog__convert_vecornothing(Int32, col_kind)
    return mixintprog(f, cA, cb, cAeq, cbeq, clb, cub, ccol_kind, params, params_presolve)

end

mixintprog{T<:Real}(f::Vector{T}, A::MatOrNothing, b::VecOrNothing) =
        mixintprog(f, A, b, nothing, nothing, nothing, nothing, nothing, nothing, nothing)

mixintprog{T<:Real}(f::Vector{T}, A::MatOrNothing, b::VecOrNothing,
        Aeq::MatOrNothing, beq::VecOrNothing) =
        mixintprog(f, A, b, Aeq, beq, nothing, nothing, nothing, nothing, nothing)

mixintprog{T<:Real}(f::Vector{T}, A::MatOrNothing, b::VecOrNothing,
        Aeq::MatOrNothing, beq::VecOrNothing, lb::VecOrNothing,
        ub::VecOrNothing) =
        mixintprog(f, A, b, Aeq, beq, lb, ub, nothing, nothing, nothing)

mixintprog{T<:Real}(f::Vector{T}, A::MatOrNothing, b::VecOrNothing,
        Aeq::MatOrNothing, beq::VecOrNothing, lb::VecOrNothing,
        ub::VecOrNothing, col_kind::VecOrNothing) =
        mixintprog(f, A, b, Aeq, beq, lb, ub, col_kind, nothing, nothing)

mixintprog{T<:Real,P<:Union(GLPIntoptParam,Nothing)}(f::Vector{T}, A::MatOrNothing, b::VecOrNothing,
        Aeq::MatOrNothing, beq::VecOrNothing, lb::VecOrNothing,
        ub::VecOrNothing, col_kind::VecOrNothing, params::P) =
        mixintprog(f, A, b, Aeq, beq, lb, ub, col_kind, params, nothing)
#}}}

## Common auxiliary functions
#{{{
function _jl_linprog__convert_vecornothing{T}(::Type{T}, a::VecOrNothing)
    if isequal(a, nothing) || isa(a, Array{None})
        return T[]
    elseif T <: Integer
        if !(eltype(a) <: Integer)
            error("integer-valued array required, or [] or nothing")
        end
    elseif T <: Real
        if !(eltype(a) <: Real)
            error("real-valued array required, or [] or nothing")
        end
    end
    return convert(Array{T}, a)
end
function _jl_linprog__convert_matornothing{T}(::Type{T}, a::MatOrNothing)
    if isequal(a, nothing) || isa(a, Array{None})
        return Array(T, 0, 0)
    elseif T <: Integer
        if !(eltype(a) <: Integer)
            error("integer-valued array required, or [] or nothing")
        end
    elseif T <: Real
        if !(eltype(a) <: Real)
            error("real-valued array required, or [] or nothing")
        end
    end
    if issparse(a)
        return convert(SparseMatrixCSC{T}, a)
    else
        return convert(Array{T}, a)
    end
end

function _jl_linprog__setup_prob{T<:Real, P<:Union(GLPParam, Nothing)}(f::Vector{T}, A::SparseOrFullMat{T}, b::Vector{T},
        Aeq::SparseOrFullMat{T}, beq::Vector{T}, lb::Vector{T}, ub::Vector{T}, params::P)

    lp = GLPProb()
    glp_set_obj_dir(lp, GLP_MIN)

    n = size(f, 1)

    m = _jl_linprog__check_A_b(A, b, n)
    meq = _jl_linprog__check_A_b(Aeq, beq, n)

    has_lb, has_ub = _jl_linprog__check_lb_ub(lb, ub, n)

    #println("n=$n m=$m meq=$meq has_lb=$has_lb ub=$has_ub")

    if m > 0
        glp_add_rows(lp, m)
        for r = 1 : m
            #println("  r=$r b=$(b[r])")
            glp_set_row_bnds(lp, r, GLP_UP, 0.0, b[r])
        end
    end
    if meq > 0
        glp_add_rows(lp, meq)
        for r = 1 : meq
            r0 = r + m
            #println("  r=$r r0=$r0 beq=$(beq[r])")
            glp_set_row_bnds(lp, r0, GLP_FX, beq[r], beq[r])
        end
    end

    glp_add_cols(lp, n)

    for c = 1 : n
        glp_set_obj_coef(lp, c, f[c])
        #println("  c=$c f=$(f[c])")
    end

    if has_lb && has_ub
        for c = 1 : n
            #println("  c=$c lb=$(lb[c]) ub=$(ub[c])")
            bounds_type = (lb[c] != ub[c] ? GLP_DB : GLP_FX)
            glp_set_col_bnds(lp, c, bounds_type, lb[c], ub[c])
        end
    elseif has_lb
        for c = 1 : n
            #println("  c=$c lb=$(lb[c])")
            glp_set_col_bnds(lp, c, GLP_LO, lb[c], 0.0)
        end
    elseif has_ub
        for c = 1 : n
            #println("  c=$c ub=$(ub[c])")
            glp_set_col_bnds(lp, c, GLP_UP, 0.0, ub[c])
        end
    end

    if (m > 0 && issparse(A)) && (meq > 0 && issparse(Aeq))
        (ia, ja, ar) = find([A; Aeq])
    elseif (m > 0 && issparse(A)) && (meq == 0)
        (ia, ja, ar) = find(A)
    elseif (m == 0) && (meq > 0 && issparse(Aeq))
        (ia, ja, ar) = find(Aeq)
    else
        (ia, ja, ar) = _jl_linprog__dense_matrices_to_glp_format(m, meq, n, A, Aeq)
    end
    #println("ia=$ia")
    #println("ja=$ja")
    #println("ar=$ar")

    glp_load_matrix(lp, ia, ja, ar)
    return (lp, n)
end

function _jl_linprog__check_A_b{T}(A::SparseOrFullMat{T}, b::Vector{T}, n::Int)
    m = 0
    if !isempty(A)
        if size(A, 2) != n
            error("invlid A size: $(size(A))")
        end
        m = size(A, 1)
        if isempty(b)
            error("b is empty but a is not")
        end
        if size(b, 1) != m
            error("invalid b size: $(size(b))")
        end
    else
        if !isempty(b)
            error("A is empty but b is not")
        end
    end
    return m
end

function _jl_linprog__check_lb_ub{T}(lb::Vector{T}, ub::Vector{T}, n::Int)
    has_lb = false
    has_ub = false
    if !isempty(lb)
        if size(lb, 1) != n
            error("invlid lb size: $(size(lb))")
        end
        has_lb = true
    end
    if !isempty(ub)
        if size(ub, 1) != n
            error("invalid ub size: $(size(ub))")
        end
        has_ub = true
    end
    return (has_lb, has_ub)
end

function _jl_linprog__dense_matrices_to_glp_format(m, meq, n, A, Aeq)
    l = (m + meq) * n

    ia = zeros(Int32, l)
    ja = zeros(Int32, l)
    ar = zeros(Float64, l)

    k = 0
    for r = 1 : m
        for c = 1 : n
            k += 1
            ia[k] = r
            ja[k] = c
            ar[k] = A[r, c]
        end
    end
    for r = 1 : meq
        for c = 1 : n
            r0 = r + m
            k += 1
            ia[k] = r0
            ja[k] = c
            ar[k] = Aeq[r, c]
        end
    end
    return (ia, ja, ar)
end

function _jl_mixintprog_set_col_kind{Ti<:Integer}(lp::GLPProb, n::Int, col_kind::Vector{Ti})
    if isempty(col_kind)
        for i = 1 : n
            glp_set_col_kind(lp, i, GLP_IV)
        end
        return
    end
    if length(col_kind) != n
        error("wrong col_kind vector size")
    end
    for i = 1 : n
        glp_set_col_kind(lp, i, col_kind[i])
    end
end

#}}}
