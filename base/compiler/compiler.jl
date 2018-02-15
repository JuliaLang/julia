# This file is a part of Julia. License is MIT: https://julialang.org/license

getfield(getfield(Main, :Core), :eval)(getfield(Main, :Core), :(baremodule Compiler

using Core.Intrinsics, Core.IR

import Core: print, println, show, write, unsafe_write, stdout, stderr,
             _apply, svec, apply_type, Builtin, IntrinsicFunction, MethodInstance

const getproperty = getfield
const setproperty! = setfield!

ccall(:jl_set_istopmod, Cvoid, (Any, Bool), Compiler, false)

eval(x) = Core.eval(Compiler, x)
eval(m, x) = Core.eval(m, x)

include(x) = Core.include(Compiler, x)
include(mod, x) = Core.include(mod, x)

#############
# from Base #
#############

# essential files and libraries
include("essentials.jl")
include("ctypes.jl")
include("generator.jl")
include("reflection.jl")
include("options.jl")

# core operations & types
function return_type end # promotion.jl expects this to exist
include("promotion.jl")
include("tuple.jl")
include("pair.jl")
include("traits.jl")
include("range.jl")
include("expr.jl")
include("error.jl")

# core numeric operations & types
include("bool.jl")
include("number.jl")
include("int.jl")
include("operators.jl")
include("pointer.jl")
const checked_add = +
const checked_sub = -

# core array operations
include("indices.jl")
include("array.jl")
include("abstractarray.jl")

# map-reduce operators
macro simd(forloop)
    esc(forloop)
end
include("reduce.jl")

# core structures
include("bitarray.jl")
include("bitset.jl")
include("abstractdict.jl")
include("iterators.jl")
include("namedtuple.jl")

# core docsystem
include("docs/core.jl")

############
# compiler #
############

inlining_enabled() = (JLOptions().can_inline == 1)
coverage_enabled() = (JLOptions().code_coverage != 0)

include("compiler/utilities.jl")
include("compiler/validation.jl")

include("compiler/inferenceresult.jl")
include("compiler/params.jl")
include("compiler/inferencestate.jl")

include("compiler/typeutils.jl")
include("compiler/typelimits.jl")
include("compiler/typelattice.jl")
include("compiler/tfuncs.jl")

include("compiler/abstractinterpretation.jl")
include("compiler/typeinfer.jl")
include("compiler/optimize.jl") # TODO: break this up further + extract utilities

include("compiler/bootstrap.jl")

end # baremodule Compiler
))
