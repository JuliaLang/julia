# A specialization whose signature has no type hash (here: a free type
# variable bounded by a `Union`, as inference leaves behind for an abstract
# call site) is kept out of its method's keyset, so a method with only such
# specializations has no keyset. Pruning its specializations for `--trim` must
# not hand the shared empty `Memory{Any}` singleton's place in the image to
# the method's new keyset, or every empty `Memory{Any}` in the executable
# comes back as that keyset.
module UnindexedSpecializations

@noinline g(x::Vector{T}, y::Vector{T}, z) where {T<:Union{Int,Float64}} = length(x) + length(y)
h(x, y) = (g(x, y, 1), g(x, y, 1.0))
# Inferring `h` for abstract arguments specializes `g` for
# `Tuple{typeof(g), Vector{T}, Vector{T}, Int} where T<:Union{Int,Float64}` and
# its `Float64` counterpart (two, so that the method gets a specializations
# table). `main` keeps them reachable so that they survive pruning.
code_typed(h, (Vector, Vector))
const MIS = Any[mi for mi in Base.specializations(only(methods(g)))]
@assert length(MIS) == 2 && all(mi -> Base.unwrap_unionall(mi.specTypes).hash == 0, MIS)

function @main(args::Vector{String})::Cint
    println(Core.stdout, length(MIS), " ", length(Memory{Any}(undef, 0)))
    s = Base.IdSet{Any}()
    push!(s, :a)
    push!(s, :b)
    println(Core.stdout, length(s), " ", :a in s, " ", :c in s)
    return 0
end

end
