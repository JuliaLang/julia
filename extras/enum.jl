macro enum(T,syms...)
    blk = quote
        immutable ($T)
            n::Int
        end
        $(esc(:symbols))(_::Type{$T}) = $syms
        Base.show(io::IO, x::($T)) = print($(esc(:symbols))(($T))[x.n+1])
    end
    for (i,sym) in enumerate(syms)
        push!(blk.args, :(const $(esc(sym)) = $(T)($(i-1))))
    end
    push!(blk.args, :nothing)
    blk.head = :toplevel
    return blk
end
