abstract Enum

# functions for all Enums
Base.convert(x::Enum) = x.n
for op in (:<, :>, :<=, :>=, :(==))
    @eval ($op){T<:Enum}(a::T, b::T) = ($op)(a.n, b.n)
end

# enum usage:
# @enum Sort [:sortnone=>0, :sortasc=>1, :sortdesc=>2]
#
macro enum(T, symsexp)
    const s = eval(symsexp)
    @assert (!isempty(s))
    const tsyms = typeof(s)
    @assert (tsyms<:Associative{Symbol})
    @assert (tsyms.parameters[1] == Symbol)
    @assert (tsyms.parameters[2]<:Integer)
    const syms = Dict([(s,convert(Cint, n))::(Symbol,Cint) for (s,n) in s]) # eval/comprehension causes type loss

    blk = quote
        immutable $(esc(T)) <: $(esc(Enum))
            n::Cint
            $(esc(T))(n::Cint) = begin
                @assert (n in $(values(syms))) "Invalid n"
                new(n)
            end
        end

        # enum specific functions
        $(esc(:(Base.typemin)))(::Type{$(esc(T))}) = $(esc(T))($(minimum(values(syms))))
        $(esc(:(Base.typemax)))(::Type{$(esc(T))}) = $(esc(T))($(maximum(values(syms))))
        $(esc(:(Base.names)))(::Type{$(esc(T))}) = $(collect(keys(syms)))
        $(esc(:(Base.length)))(::Type{$(esc(T))}) = $(length(syms))
        $(esc(:(Base.convert)))(::Type{$(esc(T))}, n::Integer) = $(esc(T))(convert(Cint, n))
    end
    for (sym,n) in syms
        push!(blk.args, :(const $(esc(sym)) = $(esc(T))(convert(Cint, $n))))
    end
    push!(blk.args, :nothing)
    blk.head = :toplevel
    blk
end

@enum Sort [:sortnone=>0, :sortasc=>1, :sortdesc=>2]
