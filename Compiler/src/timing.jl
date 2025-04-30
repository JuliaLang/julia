if ccall(:jl_timing_enabled, Cint, ()) != 0
    have_scoped = isdefined(@__MODULE__, :Base) && isdefined(Base, :ScopedValues)
    if have_scoped
        using Base.ScopedValues: ScopedValue
        const TRACY_CONTEXT = ScopedValue{Ptr{Cvoid}}(C_NULL)

        function _get_current_tracy_block()
            ctx = TRACY_CONTEXT[]
            if ctx === C_NULL
                error("must be called from within a @zone")
            end
            return ctx
        end

        function timing_print(str)
            ccall(
                :jl_timing_puts,
                Cvoid,
                (Ptr{Cvoid}, Cstring),
                _get_current_tracy_block(),
                string(str)
            )
        end

        #=
        macro timing_printf(fmt, args...)
            esc(:(
                ccall(
                    :jl_timing_printf,
                    Cvoid,
                    (Ptr{Cvoid}, Cstring, Cstring...),
                    _get_current_tracy_block(),
                    $fmt,
                    $(args...)
                )
            ))
        end
        =#
    end

    function getzonedexpr(name::Union{Symbol, String}, ex::Expr, func::Symbol, file::Symbol, line::Integer, color::Integer)
        event = RefValue{Ptr{Cvoid}}(C_NULL)
        name = QuoteNode(Symbol(name))
        func = QuoteNode(func)
        file = QuoteNode(file)

        # XXX: This buffer must be large enough to store any jl_timing_block_t (runtime-checked)
        buffer = (0, 0, 0, 0, 0, 0, 0)
        buffer_size = Core.sizeof(buffer)

        scope_expr = have_scoped ?
            :(Base.ScopedValues.Scope(Core.current_scope(), TRACY_CONTEXT => timing_block_ptr)) :
            nothing

        return quote
            if $event[] === C_NULL
                $event[] = ccall(:_jl_timing_event_create, Ptr{Cvoid},
                                 (Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Cint, Cint),
                                 :CORE_COMPILER, $name, $func, $file, $line, $color)
            end
            local timing_block = RefValue($buffer)
            local timing_block_ptr = pointer_from_objref(timing_block)
            $(Expr(:gc_preserve, quote
                ccall(:_jl_timing_block_init, Cvoid, (Ptr{Cvoid}, Csize_t, Ptr{Cvoid}), timing_block_ptr, $buffer_size, $event[])
                ccall(:_jl_timing_block_start, Cvoid, (Ptr{Cvoid},), timing_block_ptr)
                $(Expr(:tryfinally,
                    :($(Expr(:escape, ex))),
                    quote
                        ccall(:_jl_timing_block_end, Cvoid, (Ptr{Cvoid},), timing_block_ptr)
                    end,
                    :($scope_expr)
                ))
            end, :timing_block))
        end
    end
    macro zone(name, ex::Expr)
        return getzonedexpr(name, ex, :unknown_julia_function, __source__.file, __source__.line, 0)
    end
else
    macro zone(name, ex::Expr)
        return esc(ex)
    end
    timing_print(str) = nothing
    #=
    macro timing_printf(fmt, args...)
        return nothing
    end
    =#
end
