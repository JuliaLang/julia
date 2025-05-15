if ccall(:jl_timing_enabled, Cint, ()) != 0
    function getzonedexpr(name::Union{Symbol, String}, ex::Expr, func::Symbol, file::Symbol, line::Integer, color::Integer)
        event = RefValue{Ptr{Cvoid}}(C_NULL)
        name = QuoteNode(Symbol(name))
        func = QuoteNode(func)
        file = QuoteNode(file)

        # XXX: This buffer must be large enough to store any jl_timing_block_t (runtime-checked)
        buffer = (0, 0, 0, 0, 0, 0, 0)
        buffer_size = Core.sizeof(buffer)
        return quote
            if $event[] === C_NULL
                $event[] = ccall(:_jl_timing_event_create, Ptr{Cvoid},
                                 (Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Cint, Cint),
                                 :CORE_COMPILER, $name, $func, $file, $line, $color)
            end
            timing_block = RefValue($buffer)
            block_ptr = pointer_from_objref(timing_block)
            $(Expr(:gc_preserve, quote
                ccall(:_jl_timing_block_init, Cvoid, (Ptr{Cvoid}, Csize_t, Ptr{Cvoid}), block_ptr, $buffer_size, $event[])
                ccall(:_jl_timing_block_start, Cvoid, (Ptr{Cvoid},), block_ptr)
                $(Expr(:tryfinally,
                    :($(Expr(:escape, ex))),
                    quote
                        ccall(:_jl_timing_block_end, Cvoid, (Ptr{Cvoid},), block_ptr)
                    end
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
end
