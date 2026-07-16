"""
    Compiler.Unified

The provider side of UnifiedIR (§8.3 Interop): boundary converters between
`CodeInfo`/`IRCode` and UnifiedIR, an inference port running natively on
UnifiedIR, the optimizer port, the `Queries` API (§8.5), and activation
through the ordinary compiler-replacement mechanism
(`Core.OptimizedGenerics.CompilerPlugins`).

Nothing here touches bootstrap: load the package, call
[`with_unified_compiler`](@ref) (or `activate!`) after the fact.
"""
module Unified

using UnifiedIR
using UnifiedIR: StmtId, RegionId, NULL_STMT, NULL_REGION, @K_str
# The Compiler stdlib this port runs against: injected by
# `Compiler.load_unified!` (which evaluates this file in a Main-rooted
# carrier so dot-syntax keeps Base.getproperty semantics — the Compiler
# baremodule's subtree rebinds it to raw getfield) and aliased back as
# `Compiler.Unified`. Every qualified `Compiler.foo` reference below
# addresses that instance.
const Compiler = Base.parentmodule(@__MODULE__).CompilerModule

export codeinfo_to_ir, ir_to_codeinfo, UnsupportedIR,
    lowered_ir, define_ir_method!, roundtrip_codeinfo,
    infer_ir!, UInferConfig, optimize_ir!,
    infer_return, typed_ir, effects_of, InferenceConfig,
    typed_region_ir!,
    with_unified_compiler, UnifiedCacheOwner, @code_unified

include("codeinfo_entry.jl")
include("eh_entry.jl")
include("exit_lowered.jl")
include("exit_typed.jl")
include("methods.jl")
include("uinference.jl")
include("transfers.jl")
include("sroa.jl")
include("adce.jl")
include("structurize.jl")
include("inline2.jl")
include("optimize.jl")
include("completeness.jl")
include("queries.jl")
include("late.jl")
include("activate.jl")

end # module Unified
