# This file is a part of Julia. License is MIT: https://julialang.org/license

# juliac input script for building libjulia-frontend-jl: a drop-in
# implementation of the libjulia-frontend interface compiled from
# JuliaSyntax/JuliaLowering. See the Makefile in this directory.

using JuliaFrontend
using JuliaFrontend.JuliaSyntax
using JuliaFrontend.JuliaLowering

# Make the flisp frontend unreachable from Julia code in this image: the
# default Core._parse/Core._lower hooks installed during bootstrap are
# Base.fl_parse/Base.fl_lower, which reach flisp through the runtime's
# frontend trampolines. Base.__init__ replaces the parser with JuliaSyntax
# (when JULIA_USE_FLISP_PARSER is unset), but lowering would still go to
# flisp; these method overrides reroute both to this package
# unconditionally. Note this also means the remainder of this image build
# is lowered by JuliaLowering.
@eval Base function fl_parse(text::Union{Core.SimpleVector,String},
                             filename::String, lineno, offset, options)
    if text isa Core.SimpleVector
        text, text_len = text
        return $(JuliaFrontend).frontend_parse(text, Int(text_len), filename,
                                             Int(lineno), Int(offset), options)
    end
    GC.@preserve text begin
        return $(JuliaFrontend).frontend_parse(pointer(text), sizeof(text), filename,
                                              Int(lineno), Int(offset), options)
    end
end

@eval Base function fl_lower(ex, mod::Module, filename::Union{String,Ptr{UInt8}}="none",
                             lineno::Integer=0, world::UInt=typemax(Csize_t),
                             warn::Bool=false)
    filename = filename isa Ptr{UInt8} ? unsafe_string(filename) : filename
    return $(JuliaFrontend).frontend_lower(ex, mod, filename, Int(lineno), world, warn)
end
