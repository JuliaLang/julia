# Built as a script (see build.jl) so that this top-level code runs in the compiler
# process, ahead of codegen. Module initializers would not: while an image is being
# generated they are deferred to image load.
using Base.Libc.Libdl: LazyLibrary, LibraryID, dlid

# libjulia is linked into every trimmed executable, so a natively bound call into it
# resolves at link time
const LIBJULIA = LazyLibrary("libjulia"; id = LibraryID(Base.UUID("6a2ab6db-2a0d-40e1-8b5e-7a9e0d2f4c11"), "libjulia"))

native_ver_patch() = ccall((:jl_ver_patch, LIBJULIA), Cint, ())
native_ver_patch_ptr() = cglobal((:jl_ver_patch, LIBJULIA))

# a symbol of the `julia` executable that compiles this, where there is one; never called
exe_symbol() = ccall(:jl_get_pgcstack_static, Ptr{Cvoid}, ())

ccall(:jl_set_foreign_link_policy, Cvoid, (Any, Cint), dlid(LIBJULIA), true)
ccall(:jl_set_foreign_symbol_record_path, Cvoid, (Cstring,),
      joinpath(@__DIR__, "..", "foreign-symbol-record.json"))

function @main(args::Vector{String})::Cint
    println(Core.stdout, "ver patch: ", native_ver_patch())
    println(Core.stdout, "ver patch ptr: ", native_ver_patch_ptr() != C_NULL)
    length(args) > 100 && exe_symbol()
    return 0
end
