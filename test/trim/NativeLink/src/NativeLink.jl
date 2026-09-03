# Built as a script (see build.jl) so that this top-level code runs in the compiler
# process, ahead of codegen. Module initializers would not: while an image is being
# generated they are deferred to image load.
using Base.Libc.Libdl: LazyLibrary, LibraryID, dlid

# libjulia is linked into every trimmed executable, so a natively bound call into it
# resolves at link time
const LIBJULIA = LazyLibrary("libjulia"; id = LibraryID(Base.UUID("6a2ab6db-2a0d-40e1-8b5e-7a9e0d2f4c11"), "libjulia"))

native_ver_patch() = ccall((:jl_ver_patch, LIBJULIA), Cint, ())

ccall(:jl_set_foreign_link_policy, Cvoid, (Any, Cint), dlid(LIBJULIA), true)
ccall(:jl_set_export_foreign_symbol_usage, Cvoid, (Cstring,),
      joinpath(@__DIR__, "..", "used-foreign-symbols.json"))

function @main(args::Vector{String})::Cint
    println(Core.stdout, "ver patch: ", native_ver_patch())
    return 0
end
