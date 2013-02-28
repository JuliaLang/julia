
type Keywords <: Associative{Symbol,Any}
    w::Dict{Symbol,Any}
end
Keywords(ks::(Symbol...), vs::(Any...)) = Keywords(Dict{Symbol,Any}(ks,vs))
Keywords() = Keywords(Dict{Symbol,Any}())

keywords(x) = convert(Keywords, x)
convert(::Type{Keywords}, kw::Keywords) = kw
convert(::Type{Keywords}, d::Dict{Symbol,Any}) = Keywords(d)
convert(::Type{Keywords}, d::Dict) =
    Keywords((Symbol=>Any)[symbol(k) => v for (k,v) in d])

length(k::Keywords) = length(k.w)
start(k::Keywords) = start(k.w)
done(k::Keywords, i) = done(k.w, i)
next(k::Keywords, i) = next(k.w, i)

get(k::Keywords, s::Symbol, default::ANY) =
    has(k.w,s) ? ref(k.w,s) : default
has(k::Keywords, s::Symbol) = has(k.w, s)
ref(k::Keywords, s::Symbol) = ref(k.w, s)
assign(k::Keywords, v::ANY, s::Symbol) = assign(k.w, v, s)

function show(io::IO, k::Keywords)
    print(io, "Keywords(")
    first = true
    for (a,b) in k.w
        first || print(io, ", ")
        first = false
        print(io, a, '=', b)
    end
    print(io, ')')
end

bitstype 64 NKeywords
int(x::NKeywords) = box(Int,unbox(NKeywords,x))
nkeywords(x::Int) = box(NKeywords,unbox(Int,x))
