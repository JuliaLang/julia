using Test

function invalidate_struct(m::Module, name::Symbol)
    # Delete the constructors for the old type, in an attempt to ensure that
    # old objects can't be created anymore.
    T = getfield(m, name)
    for constructor in methods(T)
        Base.delete_method(constructor)
    end
    # Rename the old type (move it "out of the way")
    new_name = gensym(name)
    return Base.rename_binding(m, name, new_name)
end

## In the first set of tests, we redefine the types before using them
include("testhelpers/RedefStruct.jl")

oldtype = invalidate_struct(RedefStruct, :A)
ex = quote
    struct A
        x::Int8   # change the field type
    end
end
Core.eval(RedefStruct, ex)
@test fieldtype(RedefStruct.A, :x) === Int8
a = RedefStruct.A(-1)
@test a.x === Int8(-1)
@test_throws MethodError RedefStruct.fA(a)  # this method was defined for the old A
aa = RedefStruct.gA(5)
@test !isa(aa, oldtype)
@test aa.x === Int8(5)

oldtype = invalidate_struct(RedefStruct, :B)
ex = quote
    struct B{T<:Integer}
        z::T
    end
end
Core.eval(RedefStruct, ex)
@test fieldtype(RedefStruct.B{Int16}, :z) === Int16
b = RedefStruct.B{Int16}(-1)
@test b.z === Int16(-1)
@test_throws MethodError RedefStruct.fB(b)
# It's not possible to directly construct the TypeError that gets returned, so let's cheat
typerr = try RedefStruct.gB(Int) catch err err end
@test isa(typerr, TypeError) && typerr.func == :B && typerr.context == "T" &&
      typerr.expected.name == :T && typerr.expected.ub === Integer && typerr.got === Float64
Core.eval(RedefStruct, :(gB(::Type{T}) where T = B{unsigned(T)}(2)))
bb = RedefStruct.gB(Int)
@test !isa(bb, oldtype)
@test bb.z === UInt(2)

oldtype = invalidate_struct(RedefStruct, :C)
ex = quote
    struct C{T,N,A<:AbstractArray{T,N}}
        data::A

        function C{T,N,A}(data::AbstractArray) where {T,N,A}
            return new{T,N,A}(data)
        end
    end
end
Core.eval(RedefStruct, ex)
c32 = RedefStruct.C{Float32,2,Array{Float32,2}}([0.1 0.2; 0.3 0.4])
@test c32.data isa Array{Float32,2}
@test_throws MethodError RedefStruct.C([0.1 0.2; 0.3 0.4])  # outer constructor is not yet defined
Core.eval(RedefStruct, :(C(data::AbstractArray{T,N}) where {T,N} = C{T,N,typeof(data)}(data)))
c64 = RedefStruct.C([0.1 0.2; 0.3 0.4])
@test c64.data isa Array{Float64,2}
@test_throws MethodError RedefStruct.fC(c32)
cc = RedefStruct.gC((3,2))
@test cc.data == zeros(3, 2) && isa(cc.data, Matrix{Float64})


## Do it again, this time having already used the old methods
include("testhelpers/RedefStruct.jl")

a = RedefStruct.gA(3)
@test RedefStruct.fA(a)
oldtype = invalidate_struct(RedefStruct, :A)
ex = quote
    struct A
        x::Int8   # change the field type
    end
end
Core.eval(RedefStruct, ex)
@test fieldtype(RedefStruct.A, :x) === Int8
a = RedefStruct.A(-1)
@test a.x === Int8(-1)
@test_throws MethodError RedefStruct.fA(a)  # this method was defined for the old A
aa = RedefStruct.gA(5)
@test !isa(aa, oldtype)
@test aa.x === Int8(5)

b = RedefStruct.gB(Int)
@test b isa RedefStruct.B{Float64}
@test RedefStruct.fB(b)
oldtype = invalidate_struct(RedefStruct, :B)
ex = quote
    struct B{T<:Integer}
        z::T
    end
end
Core.eval(RedefStruct, ex)
@test fieldtype(RedefStruct.B{Int16}, :z) === Int16
b = RedefStruct.B{Int16}(-1)
@test b.z === Int16(-1)
@test_throws MethodError RedefStruct.fB(b)
typerr = try RedefStruct.gB(Int) catch err err end
@test_broken isa(typerr, TypeError) && typerr.func == :B && typerr.context == "T" &&
      typerr.expected.name == :T && typerr.expected.ub === Integer && typerr.got === Float64
typerr = try RedefStruct.gB(Float32) catch err err end
@test isa(typerr, TypeError) && typerr.func == :B && typerr.context == "T" &&
      typerr.expected.name == :T && typerr.expected.ub === Integer && typerr.got === Float32
Core.eval(RedefStruct, :(gB(::Type{T}) where T = B{unsigned(T)}(2)))
bb = RedefStruct.gB(Int)
@test !isa(bb, oldtype)
@test bb.z === UInt(2)

c = RedefStruct.gC((3,2))
@test RedefStruct.fC(c) == 3
@test c.data == zeros(3, 2)
oldtype = invalidate_struct(RedefStruct, :C)
ex = quote
    struct C{T,N,A<:AbstractArray{T,N}}
        data::A

        function C{T,N,A}(data::AbstractArray) where {T,N,A}
            return new{T,N,A}(data)
        end
    end
end
Core.eval(RedefStruct, ex)
c32 = RedefStruct.C{Float32,2,Array{Float32,2}}([0.1 0.2; 0.3 0.4])
@test c32.data isa Array{Float32,2}
@test_throws MethodError RedefStruct.C([0.1 0.2; 0.3 0.4])  # outer constructor is not yet defined
Core.eval(RedefStruct, :(C(data::AbstractArray{T,N}) where {T,N} = C{T,N,typeof(data)}(data)))
c64 = RedefStruct.C([0.1 0.2; 0.3 0.4])
@test c64.data isa Array{Float64,2}
@test_throws MethodError RedefStruct.fC(c32)
cc = RedefStruct.gC((3,2))
@test cc.data == zeros(3, 2) && isa(cc.data, Matrix{Float64})
