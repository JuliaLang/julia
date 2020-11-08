# This file is a part of Julia. License is MIT: https://julialang.org/license

# The aim of these tests is to enforce "quality assurance" for inferrability of Julia's
# Base and specifically its precompiled methods. Passing these tests & warnings does not
# indicate that Julia has no inference problems, but they are designed to reveal what
# would otherwise be hidden problems. While `@inferred` only tests the return type, the tests
# in this file are designed to check the overall inference quality of method internals.

# If you fail tests or get new warnings here, you can usually fix problems using
# `@code_warntype` or Cthulhu.jl's `@descend`.

using Test

isdefined(@__MODULE__, :is_atrisk_type) || include("testhelpers/methodanalysis.jl")

## Test the testing tools

@testset "is_atrisk_type" begin
    @test  is_atrisk_type(Tuple{typeof(==),Any,Any})
    @test  is_atrisk_type(Tuple{typeof(==),Symbol,Any})
    @test  is_atrisk_type(Tuple{typeof(==),Any,Symbol})
    @test !is_atrisk_type(Tuple{typeof(==),Symbol,Symbol})
    @test !is_atrisk_type(Tuple{typeof(convert),Type{Any},Any})
    @test !is_atrisk_type(Tuple{typeof(convert),Type{AbstractString},AbstractString})
    @test !is_atrisk_type(Tuple{typeof(convert),Type{AbstractString},String})
    @test  is_atrisk_type(Tuple{typeof(convert),Type{String},AbstractString})
    @test !is_atrisk_type(Tuple{typeof(convert),Type{Union{Int,Float32}},Int})
    @test !is_atrisk_type(Tuple{typeof(convert),Type{Union{Int,Float32}},Int32})
    @test  is_atrisk_type(Tuple{typeof(convert),Type{Union{Int,Float32}},Integer})
    @test !is_atrisk_type(Tuple{typeof(convert),Type{T} where T<:Union{Int,Float32},Int})
    @test !is_atrisk_type(Tuple{typeof(map),Function,Vector{Any}})
    @test !is_atrisk_type(Tuple{typeof(getindex),Dict{Union{String,Int},Any},Union{String,Int}})
    @test  is_atrisk_type(Tuple{typeof(getindex),Dict{Union{String,Int},Any},Any})
    @test !is_atrisk_type(Tuple{Type{BoundsError},Any,Any})
    @test  is_atrisk_type(Tuple{typeof(sin),Any})
    @test !is_atrisk_type(Tuple{typeof(length),Tuple{Any,Any}})
    @test  is_atrisk_type(Tuple{typeof(setindex!),Vector{Int},Any,Int})
    @test !is_atrisk_type(Tuple{typeof(setindex!),Vector{Any},Any,Int})
    @test  is_atrisk_type(Tuple{typeof(push!),Vector{Int},Any})
    @test !is_atrisk_type(Tuple{typeof(push!),Vector{Any},Any})
    @test !is_atrisk_type(Tuple{typeof(push!),Vector{T} where T,Any})
end

## Prepwork

# Count # of backedges for MethodInstances with abstract types

function get_atrisk_methodinstances()
    mivulnerabilities = Pair{MethodInstance,Int}[]
    for mi in methodinstances()
        if !fromcc(mi.def.module)
            if is_atrisk_type(mi.specTypes)
                push!(mivulnerabilities, mi => length(all_backedges(mi)))
            end
        end
    end
    return sort!(mivulnerabilities; by=last)
end
mivulnerabilities = get_atrisk_methodinstances()

# Get all the things that depends on these at-risk methodinstances
const atrisk_methodinstances = Set{MethodInstance}()
for (mi, c) in mivulnerabilities
    isdefined(mi, :backedges) && all_backedges!(atrisk_methodinstances, mi)
    push!(atrisk_methodinstances, mi)
end

# Split into exported & private functions
mivulnerabilities_exported = Pair{MethodInstance,Int}[]
mivulnerabilities_private = similar(mivulnerabilities_exported)
for (mi, c) in mivulnerabilities
    if isexported(mi)
        push!(mivulnerabilities_exported, mi=>c)
    else
        push!(mivulnerabilities_private, mi=>c)
    end
end

## Tests

@testset "Base.require vulnerabilities" begin
    # Invalidating the code that loads packages leads to major slowdowns, especially if it happens repeatedly
    # in a dependent chain of package loads. Ideally, we'd make this code-path "bulletproof".
    for m in methods(Base.require)
        @test isempty(remove_unlikely_methodinstances(atrisk_triggers(m, first.(mivulnerabilities_exported))))

        # It's far less important to protect against invalidation of private functions,
        # since generally packages should not extend these functions.
        # @test_broken isempty(remove_unlikely_methodinstances(atrisk_triggers(m, first.(mivulnerabilities_private))))
    end
end

# If you fail these tests, it may or may not be your fault---you may just be the
# one that pushed one of these tests over the edge. Check `badcounts` and `meancounts`
# before and after your changes; if the change is quite small, it's unlikely that your changes
# are the root cause. In such cases, failures here should be investigated or reported,
# but if inference problems can be fixed elsewhere you may not have to change your PR.
# Conversely, if your changes *do* increase these numbers substantially, your changes have likely
# triggered widespread inference problems---you should fix them before merging your PR.
#
# Never increase the thresholds here without public discussion. Indeed, the goal is to
# shrink them as much as possible.
@testset "Aggregate at-risk methodinstances" begin
    # Test overall number of atrisk MethodInstances and their average number of backedges
    badexp = Set(remove_unlikely_methodinstances(first.(mivulnerabilities_exported)))
    badcounts = filter(pr->pr.first ∈ badexp, mivulnerabilities_exported)
    threshbc = 1000
    if length(badcounts) > threshbc
        @warn "There are $(length(badcounts)) at-risk specializations of exported methods, which is above the previous threshold"
    elseif length(badcounts) < 0.8*threshbc
        @info "There are now only $(length(badcounts)) at-risk specializations of exported methods, consider dropping the threshold"
    end
    meancounts = sum(last.(badcounts))/length(badcounts)
    threshmc = 19
    if meancounts > threshmc
        @warn "The mean number of at-risk backedges is now $meancounts, which is above the previous threshold"
    elseif meancounts < 0.8*threshmc
        @info "The mean number of at-risk backedges is now only $meancounts, consider dropping the threshold"
    end
end

@testset "Specific return types" begin
    # All the is* functions
    # Not all of the broken cases have been checked carefully; it's possible some of these should return
    # `Union{Bool,Missing}` or something.
    @test        function_returns(isabspath, Bool)
    @test        function_returns(isabstracttype, Bool)
    @test_broken function_returns(isapprox, Bool)
    @test_broken function_returns(isascii, Bool)
    # @test function_returns(isassigned, Bool)
    @test        function_returns(isbits, Bool)
    @test        function_returns(isbitstype, Bool)
    @test        function_returns(isblockdev, Bool)
    @test        function_returns(ischardev, Bool)
    @test        function_returns(iscntrl, Bool)
    @test        function_returns(isconcretetype, Bool)
    @test        function_returns(isconst, Bool)
    @test        function_returns(isdefined, Bool)
    @test        function_returns(isdigit, Bool)
    @test        function_returns(isdir, Bool)
    @test        function_returns(isdirpath, Bool)
    @test_broken function_returns(isdisjoint, Bool)
    @test        function_returns(isdispatchtuple, Bool)
    @test_broken function_returns(isempty, Bool)
    @test_broken function_returns(isequal, Bool; minargs=2)
    @test_broken function_returns(iseven, Bool)
    @test        function_returns(isexported, Bool)
    @test        function_returns(isfifo, Bool)
    @test        function_returns(isfile, Bool)
    @test_broken function_returns(isfinite, Bool)
    @test_broken function_returns(isinf, Bool)
    @test_broken function_returns(isinteger, Bool)
    @test        function_returns(isinteractive, Bool)
    @test_broken function_returns(isless, Bool)
    @test        function_returns(isletter, Bool)
    @test        function_returns(islink, Bool)
    @test        function_returns(islocked, Bool)
    @test        function_returns(islowercase, Bool)
    @test_broken function_returns(ismarked, Bool)
    @test        function_returns(ismissing, Bool)
    @test        function_returns(ismount, Bool)
    @test        function_returns(ismutable, Bool)
    @test        function_returns(isnan, Bool)
    @test        function_returns(isnothing, Bool)
    @test        function_returns(isnumeric, Bool)
    @test_broken function_returns(isodd, Bool)
    @test_broken function_returns(isone, Bool)
    @test_broken function_returns(isopen, Bool)
    @test        function_returns(ispath, Bool)
    @test_broken function_returns(isperm, Bool)
    @test_broken function_returns(ispow2, Bool)
    @test        function_returns(isprimitivetype, Bool)
    @test        function_returns(isprint, Bool)
    @test        function_returns(ispunct, Bool)
    @test_broken function_returns(isreadable, Bool)
    @test_broken function_returns(isreadonly, Bool)
    @test_broken function_returns(isready, Bool)
    @test_broken function_returns(isreal, Bool)
    @test        function_returns(issetequal, Bool)
    @test        function_returns(issetgid, Bool)
    @test        function_returns(issetuid, Bool)
    @test        function_returns(issocket, Bool)
    @test_broken function_returns(issorted, Bool)
    @test        function_returns(isspace, Bool)
    @test        function_returns(issticky, Bool)
    @test        function_returns(isstructtype, Bool)
    @test_broken function_returns(issubnormal, Bool)
    @test_broken function_returns(issubset, Bool)
    @test        function_returns(istaskdone, Bool)
    @test        function_returns(istaskfailed, Bool)
    @test        function_returns(istaskstarted, Bool)
    @test_broken function_returns(istextmime, Bool)
    @test        function_returns(isuppercase, Bool)
    @test_broken function_returns(isvalid, Bool)
    @test_broken function_returns(iswritable, Bool)
    @test        function_returns(isxdigit, Bool)
    @test_broken function_returns(iszero, Bool)

    @test        function_returns(eof, Bool)
end

@testset "Never inferred" begin
    # Check that we never infer certain methodinstances,
    # It would be great to broaden this beyond Real, but this is a good start.
    # If you fail these tests, it means an internal operation forced
    # the compiler to generate one of these methods for a poorly-inferred combination of types.
    function subtype_real(@nospecialize T)
        while isa(T, TypeVar)
            T = T.ub
        end
        return T<:Real
    end
    for f in (==, <,) # isequal, <=)
        for mi in methodinstances(f)
            if any(subtype_real, Base.unwrap_unionall(mi.specTypes).parameters)
                if is_atrisk_type(mi.specTypes)
                    @warn "Specialization $(mi.specTypes) was inferred, this may indicate degraded inference quality"
                end
            end
        end
    end
end
