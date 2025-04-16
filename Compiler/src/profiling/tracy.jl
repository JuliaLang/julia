module Tracy

import ..@noinline, ..@atomic, ..Cint, ..Vector, ..push!, ..unsafe_convert, ..esc,
       ..pointer_from_objref, ..String, ..Ptr, ..UInt8, ..Cvoid,
       ..Expr, ..LineNumberNode, ..Symbol, ..UInt32, ..C_NULL, ..(===)

_strpointer(s::String) = ccall(:jl_string_ptr, Ptr{UInt8}, (Any,), s)

mutable struct TracySrcLoc
    @atomic zone_name::Ptr{UInt8}
    @atomic function_name::Ptr{UInt8}
    @atomic file::Ptr{UInt8}
    const line::UInt32
    const color::UInt32
    # Roots
    const zone_name_str::String
    const function_name_sym::Symbol
    const file_sym::Symbol
end
TracySrcLoc(zone_name::String, function_name::Symbol, file::Symbol, line::UInt32, color::UInt32) =
    TracySrcLoc(C_NULL, C_NULL, C_NULL, line, color, zone_name, function_name, file)

@noinline function reinit!(srcloc::TracySrcLoc)
    @atomic :monotonic srcloc.file = unsafe_convert(Ptr{UInt8}, srcloc.file_sym)
    @atomic :monotonic srcloc.function_name = unsafe_convert(Ptr{UInt8}, srcloc.function_name_sym)
    @atomic :release srcloc.zone_name = _strpointer(srcloc.zone_name_str)
end

struct TracyZoneCtx
    id::UInt32
    active::Cint
end

const srclocs = Vector{TracySrcLoc}()

function tracy_zone_create(name::String, ex::Expr, linfo::LineNumberNode)
    # Intern strings
    for loc in srclocs
        if loc.zone_name_str === name
            name = loc.zone_name_str
            break
        end
    end
    loc = TracySrcLoc(name, Symbol("unknown"), linfo.file, UInt32(linfo.line), UInt32(0))
    # Also roots `loc` in `srclocs`
    push!(srclocs, loc)
    return loc
end

function tracy_zone_begin(loc, active)
    if (@atomic :acquire loc.zone_name) === Ptr{UInt8}(0)
        reinit!(loc)
    end
    # `loc` is rooted in the global `srclocs`
    ptr = Ptr{TracySrcLoc}(pointer_from_objref(loc))
    return ccall((:___tracy_emit_zone_begin, "libTracyClient"), TracyZoneCtx, (Ptr{TracySrcLoc}, Cint), ptr, active)
end

function tracy_zone_end(ctx)
    ccall((:___tracy_emit_zone_end, "libTracyClient"), Cvoid, (TracyZoneCtx,), ctx)
end

end
