using Test
import Core: ImmutableArray
import Core.Compiler: arrayfreeze_tfunc, mutating_arrayfreeze_tfunc, arraythaw_tfunc
const ImmutableVector{T} = Core.ImmutableArray{T,1}

@testset "ImmutableArray tfuncs" begin
    @test arrayfreeze_tfunc(Vector{Int}) === ImmutableVector{Int}
    @test arrayfreeze_tfunc(Vector) === ImmutableVector
    @test arrayfreeze_tfunc(Array) === ImmutableArray
    @test arrayfreeze_tfunc(Any) === ImmutableArray
    @test arrayfreeze_tfunc(ImmutableVector{Int}) === Union{}
    @test arrayfreeze_tfunc(ImmutableVector) === Union{}
    @test arrayfreeze_tfunc(ImmutableArray) === Union{}
    @test mutating_arrayfreeze_tfunc(Vector{Int}) === ImmutableVector{Int}
    @test mutating_arrayfreeze_tfunc(Vector) === ImmutableVector
    @test mutating_arrayfreeze_tfunc(Array) === ImmutableArray
    @test mutating_arrayfreeze_tfunc(Any) === ImmutableArray
    @test mutating_arrayfreeze_tfunc(ImmutableVector{Int}) === Union{}
    @test mutating_arrayfreeze_tfunc(ImmutableVector) === Union{}
    @test mutating_arrayfreeze_tfunc(ImmutableArray) === Union{}
    @test arraythaw_tfunc(ImmutableVector{Int}) === Vector{Int}
    @test arraythaw_tfunc(ImmutableVector) === Vector
    @test arraythaw_tfunc(ImmutableArray) === Array
    @test arraythaw_tfunc(Any) === Array
    @test arraythaw_tfunc(Vector{Int}) === Union{}
    @test arraythaw_tfunc(Vector) === Union{}
    @test arraythaw_tfunc(Array) === Union{}
end

# mutating_arrayfreeze optimization
# =================================

import Core.Compiler: argextype, singleton_type
const EMPTY_SPTYPES = Any[]

code_typed1(args...; kwargs...) = first(only(code_typed(args...; kwargs...)))::Core.CodeInfo
get_code(args...; kwargs...) = code_typed1(args...; kwargs...).code

# check if `x` is a statement with a given `head`
isnew(@nospecialize x) = Meta.isexpr(x, :new)

# check if `x` is a dynamic call of a given function
iscall(y) = @nospecialize(x) -> iscall(y, x)
function iscall((src, f)::Tuple{Core.CodeInfo,Base.Callable}, @nospecialize(x))
    return iscall(x) do @nospecialize x
        singleton_type(argextype(x, src, EMPTY_SPTYPES)) === f
    end
end
iscall(pred::Base.Callable, @nospecialize(x)) = Meta.isexpr(x, :call) && pred(x.args[1])

# check if `x` is a statically-resolved call of a function whose name is `sym`
isinvoke(y) = @nospecialize(x) -> isinvoke(y, x)
isinvoke(sym::Symbol, @nospecialize(x)) = isinvoke(mi->mi.def.name===sym, x)
isinvoke(pred::Function, @nospecialize(x)) = Meta.isexpr(x, :invoke) && pred(x.args[1]::Core.MethodInstance)

function is_array_alloc(@nospecialize x)
    Meta.isexpr(x, :foreigncall) || return false
    args = x.args
    name = args[1]
    isa(name, QuoteNode) && (name = name.value)
    isa(name, Symbol) || return false
    return Core.Compiler.alloc_array_ndims(name) !== nothing
end

# unescaped examples
# ------------------

# simplest -- vector
function unescaped1_1(gen)
    a = [1,2,3,4,5]
    return gen(a)
end
let src = code_typed1(unescaped1, (Type{Core.ImmutableArray},))
    @test count(is_array_alloc, src.code) == 1
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 1
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 0
    unescaped1(identity)
    allocated = @allocated unescaped1(identity)
    unescaped1(ImmutableArray)
    @test allocated == @allocated unescaped1(ImmutableArray)
end

# handle matrix etc. (actually this example also requires inter-procedural escape handling)
function unescaped1_2(gen)
    a = [1 2 3; 4 5 6]
    b = [1 2 3 4 5 6]
    return gen(a), gen(b)
end
let src = code_typed1(unescaped1_2, (Type{Core.ImmutableArray},))
    # @test count(is_array_alloc, src.code) == 1
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 2
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 0
    unescaped1_2(identity)
    allocated = @allocated unescaped1_2(identity)
    unescaped1_2(ImmutableArray)
    @test allocated == @allocated unescaped1_2(ImmutableArray)
end

# multiple returns don't matter
function unescaped2(gen)
    a = [1,2,3,4,5]
    return gen(a), gen(a)
end
let src = code_typed1(unescaped2, (Type{Core.ImmutableArray},))
    @test count(is_array_alloc, src.code) == 1
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 2
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 0
    unescaped2(identity)
    allocated = @allocated unescaped2(identity)
    unescaped2(ImmutableArray)
    @test allocated == @allocated unescaped2(ImmutableArray)
end

# arrayset
function unescaped3_1(gen)
    a = Vector{Int}(undef, 5)
    for i = 1:5
        a[i] = i
    end
    return gen(a)
end
let src = code_typed1(unescaped3_1, (Type{Core.ImmutableArray},))
    @test count(is_array_alloc, src.code) == 1
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 1
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 0
    unescaped3_1(identity)
    allocated = @allocated unescaped3_1(identity)
    unescaped3_1(ImmutableArray)
    @test allocated == @allocated unescaped3_1(ImmutableArray)
end

function unescaped3_2(gen)
    a = Matrix{Float64}(undef, 5, 2)
    for i = 1:5
        for j = 1:2
            a[i, j] = i + j
        end
    end
    return gen(a)
end
let src = code_typed1(unescaped3_2, (Type{Core.ImmutableArray},))
    @test count(is_array_alloc, src.code) == 1
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 1
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 0
    unescaped3_2(identity)
    allocated = @allocated unescaped3_2(identity)
    unescaped3_2(ImmutableArray)
    @test allocated == @allocated unescaped3_2(ImmutableArray)
end

# array resize
function unescaped4(gen, n)
    a = Int[]
    for i = 1:n
        push!(a, i)
    end
    return gen(a)
end
let src = code_typed1(unescaped4, (Type{Core.ImmutableArray},Int,))
    @test count(is_array_alloc, src.code) == 1
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 1
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 0
    unescaped4(identity, 42)
    allocated = @allocated unescaped4(identity, 42)
    unescaped4(ImmutableArray, 42)
    @test allocated == @allocated unescaped4(ImmutableArray, 42)
end

# inter-procedural
@noinline function same′(a)
    return reverse(reverse(a))
end
function unescaped5(gen)
    a = ones(5)
    a = same′(a)
    return gen(a)
end
let src = code_typed1(unescaped5, (Type{Core.ImmutableArray},))
    @test count(is_array_alloc, src.code) == 1
    @test count(isinvoke(:same′), src.code) == 1
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 1
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 0
    unescaped5(identity)
    allocated = @allocated unescaped5(identity)
    unescaped5(ImmutableArray)
    @test allocated == @allocated unescaped5(ImmutableArray)
end

# ignore ThrownEscape if it never happens when `arrayfreeze` is called
function unescaped6(gen, n)
    a = Int[]
    for i = 1:n
        push!(a, i)
    end
    n > 100 && throw(a)
    return gen(a)
end
let src = code_typed1(unescaped6, (Type{Core.ImmutableArray},Int,))
    @test count(is_array_alloc, src.code) == 1
    @test_broken count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 1
    @test_broken count(iscall((src, Core.arrayfreeze)), src.code) == 0
    unescaped6(identity, 42)
    allocated = @allocated unescaped6(identity, 42)
    unescaped6(ImmutableArray, 42)
    @test_broken allocated == @allocated unescaped6(ImmutableArray, 42)
end

# escaped examples
# ----------------

const Rx = Ref{Any}() # global memory

function escaped01(gen)
    a = [1,2,3,4,5]
    return a, gen(a)
end
let src = code_typed1(escaped01, (Type{ImmutableArray},))
    @test count(is_array_alloc, src.code) == 1
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 0
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 1
    escaped01(identity)
    allocated = @allocated escaped01(identity)
    escaped01(ImmutableArray)
    local a, b
    @test allocated < @allocated a, b = escaped01(ImmutableArray)
    @test a !== b
    @test !(a isa ImmutableArray)
end

escaped02(a, gen) = gen(a)
let src = code_typed1(escaped02, (Vector{Int}, Type{ImmutableArray},))
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 0
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 1
    a = [1,2,3]
    escaped02(a, ImmutableArray)
    b = escaped02(a, ImmutableArray)
    @test a !== b
    @test !(a isa ImmutableArray)
    @test b isa ImmutableArray
end

function escaped1(gen)
    a = [1,2,3,4,5]
    global global_array = a
    return gen(a)
end
let src = code_typed1(escaped1, (Type{Core.ImmutableArray},))
    @test count(is_array_alloc, src.code) == 1
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 0
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 1
    escaped1(identity)
    allocated = @allocated escaped1(identity)
    escaped1(ImmutableArray)
    local a
    @test allocated < @allocated a = escaped1(ImmutableArray)
    @test global_array !== a
    @test !(global_array isa ImmutableArray)
end

function escaped2(gen)
    a = [1,2,3,4,5]
    Rx[] = a
    return gen(a)
end
let src = code_typed1(escaped2, (Type{Core.ImmutableArray},))
    @test count(is_array_alloc, src.code) == 1
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 0
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 1
    escaped2(identity)
    allocated = @allocated escaped2(identity)
    escaped2(ImmutableArray)
    local a
    @test allocated < @allocated a = escaped2(ImmutableArray)
    @test Rx[] !== a
    @test !(Rx[] isa ImmutableArray)
end

function escaped3(gen)
    a = [1,2,3,4,5]
    try
        throw(a)
    catch err
        global global_array = err
    end
    return gen(a)
end
let src = code_typed1(escaped3, (Type{Core.ImmutableArray},))
    @test count(is_array_alloc, src.code) == 1
    @test count(iscall((src, Core.mutating_arrayfreeze)), src.code) == 0
    @test count(iscall((src, Core.arrayfreeze)), src.code) == 1
    escaped3(identity)
    allocated = @allocated escaped3(identity)
    escaped3(ImmutableArray)
    local a
    @test allocated < @allocated a = escaped3(ImmutableArray)
    @test global_array !== a
    @test !(global_array isa ImmutableArray)
end

@testset "maybecopy tests" begin
    g = nothing # global

    @noinline function escape(arr)
        g = arr
        return arr
    end

    function mc1()
        a = Vector{Int64}(undef, 5)
        b = Core.maybecopy(a) # doesn't escape in this function - so a === b
        @test a === b
    end

    # XXX broken until maybecopy implementation is correct
    function mc2()
        a = Vector{Int64}(undef, 5)
        try
            getindex(a, 6)
        catch e
            if isa(e, BoundsError)
                @test_broken !(e.a === a) # only escapes through throw, so this should copy
            end
        end
    end

    function mc3()
        a = Vector{Int64}(undef, 5)
        escape(a)
        b = Core.maybecopy(a)
        @test a === b # escapes elsewhere, so give back the actual object
    end

    function mc4()
        a = Vector{Int64}(undef, 5)
        escape(a)
        try
            getindex(a, 6)
        catch e
            if isa(e, BoundsError)
                @test e.a === a # already escaped so we don't copy
            end
        end
    end

    function test_maybecopy()
        mc1(); mc2(); mc3();
        mc4();
    end

    test_maybecopy()
end

@test typeof(Core.ImmutableArray([1,2,3]) .+ Core.ImmutableArray([4,5,6])) <: Core.ImmutableArray

# DiffEq Performance Tests

# using DifferentialEquations
# using StaticArrays

# function _build_atsit5_caches(::Type{T}) where {T}

#     cs = SVector{6, T}(0.161, 0.327, 0.9, 0.9800255409045097, 1.0, 1.0)

#     as = SVector{21, T}(
#         #=a21=# convert(T,0.161),
#         #=a31=# convert(T,-0.008480655492356989),
#         #=a32=# convert(T,0.335480655492357),
#         #=a41=# convert(T,2.8971530571054935),
#         #=a42=# convert(T,-6.359448489975075),
#         #=a43=# convert(T,4.3622954328695815),
#         #=a51=# convert(T,5.325864828439257),
#         #=a52=# convert(T,-11.748883564062828),
#         #=a53=# convert(T,7.4955393428898365),
#         #=a54=# convert(T,-0.09249506636175525),
#         #=a61=# convert(T,5.86145544294642),
#         #=a62=# convert(T,-12.92096931784711),
#         #=a63=# convert(T,8.159367898576159),
#         #=a64=# convert(T,-0.071584973281401),
#         #=a65=# convert(T,-0.028269050394068383),
#         #=a71=# convert(T,0.09646076681806523),
#         #=a72=# convert(T,0.01),
#         #=a73=# convert(T,0.4798896504144996),
#         #=a74=# convert(T,1.379008574103742),
#         #=a75=# convert(T,-3.290069515436081),
#         #=a76=# convert(T,2.324710524099774)
#     )

#     btildes = SVector{7,T}(
#                 convert(T,-0.00178001105222577714),
#                 convert(T,-0.0008164344596567469),
#                 convert(T,0.007880878010261995),
#                 convert(T,-0.1447110071732629),
#                 convert(T,0.5823571654525552),
#                 convert(T,-0.45808210592918697),
#                 convert(T,0.015151515151515152)
#     )
#     rs = SVector{22, T}(
#         #=r11=# convert(T,1.0),
#         #=r12=# convert(T,-2.763706197274826),
#         #=r13=# convert(T,2.9132554618219126),
#         #=r14=# convert(T,-1.0530884977290216),
#         #=r22=# convert(T,0.13169999999999998),
#         #=r23=# convert(T,-0.2234),
#         #=r24=# convert(T,0.1017),
#         #=r32=# convert(T,3.9302962368947516),
#         #=r33=# convert(T,-5.941033872131505),
#         #=r34=# convert(T,2.490627285651253),
#         #=r42=# convert(T,-12.411077166933676),
#         #=r43=# convert(T,30.33818863028232),
#         #=r44=# convert(T,-16.548102889244902),
#         #=r52=# convert(T,37.50931341651104),
#         #=r53=# convert(T,-88.1789048947664),
#         #=r54=# convert(T,47.37952196281928),
#         #=r62=# convert(T,-27.896526289197286),
#         #=r63=# convert(T,65.09189467479366),
#         #=r64=# convert(T,-34.87065786149661),
#         #=r72=# convert(T,1.5),
#         #=r73=# convert(T,-4),
#         #=r74=# convert(T,2.5),
#     )
#     return cs, as, btildes, rs
# end

# function test_imarrays()
#     function lorenz(u, p, t)
#         a,b,c = u
#         x,y,z = p
#         dx_dt = x * (b - a)
#         dy_dt = a*(y - c) - b
#         dz_dt = a*b - z * c
#         res = Vector{Float64}(undef, 3)
#         res[1], res[2], res[3] = dx_dt, dy_dt, dz_dt
#         Core.ImmutableArray(res)
#     end

#     _u0 = Core.ImmutableArray([1.0, 1.0, 1.0])
#     _tspan = (0.0, 100.0)
#     _p = (10.0, 28.0, 8.0/3.0)
#     prob = ODEProblem(lorenz, _u0, _tspan, _p)

#     u0 = prob.u0
#     tspan = prob.tspan
#     f = prob.f
#     p = prob.p

#     dt = 0.1f0
#     saveat = nothing
#     save_everystep = true
#     abstol = 1f-6
#     reltol = 1f-3

#     t = tspan[1]
#     tf = prob.tspan[2]

#     beta1 = 7/50
#     beta2 = 2/25
#     qmax = 10.0
#     qmin = 1/5
#     gamma = 9/10
#     qoldinit = 1e-4

#     if saveat === nothing
#         ts = Vector{eltype(dt)}(undef,1)
#         ts[1] = prob.tspan[1]
#         us = Vector{typeof(u0)}(undef,0)
#         push!(us,recursivecopy(u0))
#     else
#         ts = saveat
#         cur_t = 1
#         us = MVector{length(ts),typeof(u0)}(undef)
#         if prob.tspan[1] == ts[1]
#             cur_t += 1
#             us[1] = u0
#         end
#     end

#     u = u0
#     qold = 1e-4
#     k7 = f(u, p, t)

#     cs, as, btildes, rs = _build_atsit5_caches(eltype(u0))
#     c1, c2, c3, c4, c5, c6 = cs
#     a21, a31, a32, a41, a42, a43, a51, a52, a53, a54,
#     a61, a62, a63, a64, a65, a71, a72, a73, a74, a75, a76 = as
#     btilde1, btilde2, btilde3, btilde4, btilde5, btilde6, btilde7 = btildes

#     # FSAL
#     while t < tspan[2]
#         uprev = u
#         k1 = k7
#         EEst = Inf

#         while EEst > 1
#             dt < 1e-14 && error("dt<dtmin")

#             tmp = uprev+dt*a21*k1
#             k2 = f(tmp, p, t+c1*dt)
#             tmp = uprev+dt*(a31*k1+a32*k2)
#             k3 = f(tmp, p, t+c2*dt)
#             tmp = uprev+dt*(a41*k1+a42*k2+a43*k3)
#             k4 = f(tmp, p, t+c3*dt)
#             tmp = uprev+dt*(a51*k1+a52*k2+a53*k3+a54*k4)
#             k5 = f(tmp, p, t+c4*dt)
#             tmp = uprev+dt*(a61*k1+a62*k2+a63*k3+a64*k4+a65*k5)
#             k6 = f(tmp, p, t+dt)
#             u = uprev+dt*(a71*k1+a72*k2+a73*k3+a74*k4+a75*k5+a76*k6)
#             k7 = f(u, p, t+dt)

#             tmp = dt*(btilde1*k1+btilde2*k2+btilde3*k3+btilde4*k4+
#                         btilde5*k5+btilde6*k6+btilde7*k7)
#             tmp = tmp./(abstol.+max.(abs.(uprev),abs.(u))*reltol)
#             EEst = DiffEqBase.ODE_DEFAULT_NORM(tmp, t)

#             if iszero(EEst)
#                 q = inv(qmax)
#             else
#                 @fastmath q11 = EEst^beta1
#                 @fastmath q = q11/(qold^beta2)
#             end

#             if EEst > 1
#                 dt = dt/min(inv(qmin),q11/gamma)
#             else # EEst <= 1
#                 @fastmath q = max(inv(qmax),min(inv(qmin),q/gamma))
#                 qold = max(EEst,qoldinit)
#                 dtold = dt
#                 dt = dt/q #dtnew
#                 dt = min(abs(dt),abs(tf-t-dtold))
#                 told = t

#                 if (tf - t - dtold) < 1e-14
#                     t = tf
#                 else
#                     t += dtold
#                 end

#                 if saveat === nothing && save_everystep
#                     push!(us,recursivecopy(u))
#                     push!(ts,t)
#                 else saveat !== nothing
#                     while cur_t <= length(ts) && ts[cur_t] <= t
#                         savet = ts[cur_t]
#                         θ = (savet - told)/dtold
#                         b1θ, b2θ, b3θ, b4θ, b5θ, b6θ, b7θ = bθs(rs, θ)
#                         us[cur_t] = uprev + dtold*(
#                             b1θ*k1 + b2θ*k2 + b3θ*k3 + b4θ*k4 + b5θ*k5 + b6θ*k6 + b7θ*k7)
#                         cur_t += 1
#                     end
#                 end
#             end
#         end
#     end

#     if saveat === nothing && !save_everystep
#         push!(us,u)
#         push!(ts,t)
#     end

#     sol = DiffEqBase.build_solution(prob,Tsit5(),ts,us,calculate_error = false)

#     DiffEqBase.has_analytic(prob.f) && DiffEqBase.calculate_solution_errors!(sol;timeseries_errors=true,dense_errors=false)

#     sol
# end

# function test_marrays()
#     function lorenz(u, p, t)
#         a,b,c = u
#         x,y,z = p
#         dx_dt = x * (b - a)
#         dy_dt = a*(y - c) - b
#         dz_dt = a*b - z * c
#         res = Vector{Float64}(undef, 3)
#         res[1], res[2], res[3] = dx_dt, dy_dt, dz_dt
#         res
#     end

#     _u0 = [1.0, 1.0, 1.0]
#     _tspan = (0.0, 100.0)
#     _p = (10.0, 28.0, 8.0/3.0)
#     prob = ODEProblem(lorenz, _u0, _tspan, _p)

#     u0 = prob.u0
#     tspan = prob.tspan
#     f = prob.f
#     p = prob.p

#     dt = 0.1f0
#     saveat = nothing
#     save_everystep = true
#     abstol = 1f-6
#     reltol = 1f-3

#     t = tspan[1]
#     tf = prob.tspan[2]

#     beta1 = 7/50
#     beta2 = 2/25
#     qmax = 10.0
#     qmin = 1/5
#     gamma = 9/10
#     qoldinit = 1e-4

#     if saveat === nothing
#         ts = Vector{eltype(dt)}(undef,1)
#         ts[1] = prob.tspan[1]
#         us = Vector{typeof(u0)}(undef,0)
#         push!(us,recursivecopy(u0))
#     else
#         ts = saveat
#         cur_t = 1
#         us = MVector{length(ts),typeof(u0)}(undef)
#         if prob.tspan[1] == ts[1]
#             cur_t += 1
#             us[1] = u0
#         end
#     end

#     u = u0
#     qold = 1e-4
#     k7 = f(u, p, t)

#     cs, as, btildes, rs = _build_atsit5_caches(eltype(u0))
#     c1, c2, c3, c4, c5, c6 = cs
#     a21, a31, a32, a41, a42, a43, a51, a52, a53, a54,
#     a61, a62, a63, a64, a65, a71, a72, a73, a74, a75, a76 = as
#     btilde1, btilde2, btilde3, btilde4, btilde5, btilde6, btilde7 = btildes

#     # FSAL
#     while t < tspan[2]
#         uprev = u
#         k1 = k7
#         EEst = Inf

#         while EEst > 1
#             dt < 1e-14 && error("dt<dtmin")

#             tmp = uprev+dt*a21*k1
#             k2 = f(tmp, p, t+c1*dt)
#             tmp = uprev+dt*(a31*k1+a32*k2)
#             k3 = f(tmp, p, t+c2*dt)
#             tmp = uprev+dt*(a41*k1+a42*k2+a43*k3)
#             k4 = f(tmp, p, t+c3*dt)
#             tmp = uprev+dt*(a51*k1+a52*k2+a53*k3+a54*k4)
#             k5 = f(tmp, p, t+c4*dt)
#             tmp = uprev+dt*(a61*k1+a62*k2+a63*k3+a64*k4+a65*k5)
#             k6 = f(tmp, p, t+dt)
#             u = uprev+dt*(a71*k1+a72*k2+a73*k3+a74*k4+a75*k5+a76*k6)
#             k7 = f(u, p, t+dt)

#             tmp = dt*(btilde1*k1+btilde2*k2+btilde3*k3+btilde4*k4+
#                         btilde5*k5+btilde6*k6+btilde7*k7)
#             tmp = tmp./(abstol.+max.(abs.(uprev),abs.(u))*reltol)
#             EEst = DiffEqBase.ODE_DEFAULT_NORM(tmp, t)

#             if iszero(EEst)
#                 q = inv(qmax)
#             else
#                 @fastmath q11 = EEst^beta1
#                 @fastmath q = q11/(qold^beta2)
#             end

#             if EEst > 1
#                 dt = dt/min(inv(qmin),q11/gamma)
#             else # EEst <= 1
#                 @fastmath q = max(inv(qmax),min(inv(qmin),q/gamma))
#                 qold = max(EEst,qoldinit)
#                 dtold = dt
#                 dt = dt/q #dtnew
#                 dt = min(abs(dt),abs(tf-t-dtold))
#                 told = t

#                 if (tf - t - dtold) < 1e-14
#                     t = tf
#                 else
#                     t += dtold
#                 end

#                 if saveat === nothing && save_everystep
#                     push!(us,recursivecopy(u))
#                     push!(ts,t)
#                 else saveat !== nothing
#                     while cur_t <= length(ts) && ts[cur_t] <= t
#                         savet = ts[cur_t]
#                         θ = (savet - told)/dtold
#                         b1θ, b2θ, b3θ, b4θ, b5θ, b6θ, b7θ = bθs(rs, θ)
#                         us[cur_t] = uprev + dtold*(
#                             b1θ*k1 + b2θ*k2 + b3θ*k3 + b4θ*k4 + b5θ*k5 + b6θ*k6 + b7θ*k7)
#                         cur_t += 1
#                     end
#                 end
#             end
#         end
#     end

#     if saveat === nothing && !save_everystep
#         push!(us,u)
#         push!(ts,t)
#     end

#     sol = DiffEqBase.build_solution(prob,Tsit5(),ts,us,calculate_error = false)

#     DiffEqBase.has_analytic(prob.f) && DiffEqBase.calculate_solution_errors!(sol;timeseries_errors=true,dense_errors=false)

#     sol
# end
