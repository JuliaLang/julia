replstr(x) = sprint((io,x) -> writemime(io,MIME("text/plain"),x), x)

@test replstr(cell(2)) == "2-element Array{Any,1}:\n #undef\n #undef"
@test replstr(cell(2,2)) == "2x2 Array{Any,2}:\n #undef  #undef\n #undef  #undef"
@test replstr(cell(2,2,2)) == "2x2x2 Array{Any,3}:\n[:, :, 1] =\n #undef  #undef\n #undef  #undef\n\n[:, :, 2] =\n #undef  #undef\n #undef  #undef"

immutable T5589
    names::Vector{UTF8String}
end
@test replstr(T5589(Array(UTF8String,100))) == "T5589([#undef,#undef,#undef,#undef,#undef,#undef,#undef,#undef,#undef,#undef  …  #undef,#undef,#undef,#undef,#undef,#undef,#undef,#undef,#undef,#undef])"
