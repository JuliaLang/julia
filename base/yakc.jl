@noinline function (y::Core.YAKC{A, R})(args...) where {A,R}
    typeassert(args, A)
    ccall(y.fptr1, Any, (Any, Ptr{Any}, Int), y, Any[args...], length(args))::R
end

"""
    _yakc(argt::Type{<:Tuple}, lb::Type, ub::Type, source::CodeInfo, captures...)

Create a new YAKC taking arguments specified by the types `argt`. When called,
this yakc will execute the source specified in `source`. The `lb` and `ub`
arguments constrain the return type of the yakc. In particular, any return
value of type `Core.YAKC{argt, R} where lb<:R<:ub` is semantically valid. If
the optimizer runs, it may replace `R` by the narrowest possible type inference
was able to determine. To guarantee a particular value of `R`, set lb===ub.
"""
Core._yakc


# YAKC macro goes here
