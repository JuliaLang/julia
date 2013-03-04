macro enum(T,syms...)
    N = int(Int.nbits)
    T = esc(T)
    sh = Expr(:block)
    sh.args = {:(if x===$sym return print($(string(sym))) end) for sym in syms}
    blk = quote
        bitstype ($N) ($T)
        $(esc(:symbols))(_::Type{$T}) = $syms
        $(esc(:isequal))(a::($T), b::($T)) = eq_int(unbox($T,a),unbox($T,b))
        $(esc(:show))(io::IO, x::($T)) = $sh
    end
    for (i,sym) in enumerate(syms)
        push!(blk.args, :(const $(esc(sym)) = box($T,unbox(Int,$(i-1)))))
    end
    push!(blk.args, :nothing)
    blk.head = :toplevel
    return blk
end
