
module Tracy

import ..@noinline, ..Cint, ..Vector, ..push!, ..unsafe_convert, ..esc,
       ..pointer_from_objref, ..String, ..Ptr, ..UInt8, ..Cvoid,
       ..Expr, ..LineNumberNode, ..Symbol, ..UInt32

_strpointer(s::String) = ccall(:jl_string_ptr, Ptr{UInt8}, (Any,), s)

mutable struct TracySrcLoc
    zone_name::Ptr{UInt8}
    function_name::Ptr{UInt8}
    file::Ptr{UInt8}
    line::UInt32
    color::UInt32
    # Roots
    zone_name_str::String
    function_name_sym::Symbol
    file_sym::Symbol
end
TracySrcLoc(zone_name::String, function_name::Symbol, file::Symbol, line::UInt32, color::UInt32) =
    TracySrcLoc(_strpointer(zone_name),
                unsafe_convert(Ptr{UInt8}, function_name),
                unsafe_convert(Ptr{UInt8}, file),
                line, color, zone_name, function_name, file)

@noinline function reinit!(srcloc::TracySrcLoc)
    srcloc.zone_name = _strpointer(srcloc.zone_name_str)
    srcloc.function_name = unsafe_convert(Ptr{UInt8}, srcloc.function_name_sym)
    srcloc.file = unsafe_convert(Ptr{UInt8}, srcloc.file_sym)
end

struct TracyZoneCtx
    id::UInt32
    active::Cint
end

const unknown_function = Symbol("unknown")
const srclocs = Vector{TracySrcLoc}()

function _zone_create(name::String, ex::Expr, linfo::LineNumberNode)
    # Intern strings
    for loc in srclocs
        if loc.zone_name_str === name
            name = loc.zone_name_str
            break
        end
    end
    loc = TracySrcLoc(name, unknown_function, linfo.file, UInt32(linfo.line), UInt32(0))
    push!(srclocs, loc)
    return loc
end

function _zone_begin(loc, active)
    if loc.zone_name === Ptr{UInt8}(0)
        reinit!(loc)
    end
    ptr = Ptr{TracySrcLoc}(pointer_from_objref(loc))
    return ccall((:___tracy_emit_zone_begin, "libTracyClient"), TracyZoneCtx, (Ptr{TracySrcLoc}, Cint), ptr, active)
end

function _zone_end(ctx)
    ccall((:___tracy_emit_zone_end, "libTracyClient"), Cvoid, (TracyZoneCtx,), ctx)
end

end
