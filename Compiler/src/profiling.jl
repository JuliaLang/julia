const WITH_ITTAPI = ccall(:jl_ittapi_enabled, Cint, ()) != 0
const WITH_TRACY = ccall(:jl_tracy_enabled, Cint, ()) != 0

# Maybe create a singleton type TimingLock so one can use `@lock`
lock_timing() = ccall(:jl_timing_lock_lock, Cvoid, ())
unlock_timing() = ccall(:jl_timing_lock_unlock, Cvoid, ())

include("profiling/tracy.jl")
include("profiling/ittapi.jl")

if WITH_TRACY || WITH_ITTAPI
    macro zone(name, ex::Expr)
        lock_timing()
        srcloc = WITH_TRACY && Tracy.tracy_zone_create(name, ex, __source__)
        tracy_begin_expr = WITH_TRACY ? :(ctx_tracy = Tracy.tracy_zone_begin($srcloc, true)) : :()
        tracy_end_expr = WITH_TRACY ? :(Tracy.tracy_zone_end(ctx_tracy)) : :()

        event = WITH_ITTAPI && ITTAPI.ittapi_zone_create(name, ex, __source__)
        ittapi_begin_expr = WITH_ITTAPI ? :(ctx_ittapi = ITTAPI.ittapi_zone_begin($event, true)) : :()
        ittapi_end_expr = WITH_ITTAPI ? :(ITTAPI.ittapi_zone_end(ctx_ittapi)) : :()
        unlock_timing()
        return quote
            $tracy_begin_expr
            $ittapi_begin_expr
            $(Expr(:tryfinally,
            :($(esc(ex))),
                quote
                    $tracy_end_expr
                    $ittapi_end_expr
                end
        ))
        end
    end
else
    macro zone(name::String, ex::Expr)
        esc(ex)
    end
end
