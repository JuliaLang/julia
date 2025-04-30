if ccall(:jl_timing_enabled, Cint, ()) != 0
    const scoped_values_available = isdefined(@__MODULE__, :Base) && isdefined(Base, :ScopedValues)
    const use_tls = true # kappa # use tls or scoped values
    if !scoped_values_available
        const CURRENT_TIMING_BLOCK = RefValue(C_NULL)
        @inline _get_current_timing_block_nocheck() = CURRENT_TIMING_BLOCK
        @inline function set_current_timing_block!(block::Ptr{Cvoid})
            old_block = CURRENT_TIMING_BLOCK[]
            CURRENT_TIMING_BLOCK[] = block
            return old_block
        end
        @inline function restore_current_timing_block!(old_block::Ptr{Cvoid})
            CURRENT_TIMING_BLOCK[] = old_block
        end
    elseif use_tls
        const TIMING_BLOCK_TLS_KEY = :timing_block_tls_key
        @inline function _get_current_timing_block_nocheck()
            ref_block = get(task_local_storage(), TIMING_BLOCK_TLS_KEY, nothing)::Union{Nothing, RefValue{Ptr{Cvoid}}}
            return ref_block === nothing ? C_NULL : ref_block[]
        end
        @inline function set_current_timing_block!(block::Ptr{Cvoid})
            ref_block = get!(()->RefValue(C_NULL), task_local_storage(), TIMING_BLOCK_TLS_KEY)::RefValue{Ptr{Cvoid}}
            old = ref_block[]
            ref_block[] = block
            return (old, ref_block)
        end
        @inline function restore_current_timing_block!((old, ref)::Tuple{Ptr{Cvoid}, RefValue{Ptr{Cvoid}}})
            ref[] = old
        end
    else
        using Base.ScopedValues: ScopedValue
        const CURRENT_TIMING_BLOCK = ScopedValue{Ptr{Cvoid}}(C_NULL)
        _get_current_timing_block_nocheck() = CURRENT_TIMING_BLOCK[]
    end

    function _get_current_timing_block()
        block = _get_current_timing_block_nocheck()
        if block === C_NULL
            error("must be called from within a @zone")
        end
        return block
    end

    function getzonedexpr(name::Union{Symbol, String}, ex::Expr, func::Symbol, file::Symbol, line::Integer, color::Integer)
        event = RefValue{Ptr{Cvoid}}(C_NULL)
        name = QuoteNode(Symbol(name))
        func = QuoteNode(func)
        file = QuoteNode(file)

        # XXX: This buffer must be large enough to store any jl_timing_block_t (runtime-checked)
        buffer = (0, 0, 0, 0, 0, 0, 0)
        buffer_size = Core.sizeof(buffer)

        before_expr = if !scoped_values_available || use_tls
           :(prev_block_ptr = $set_current_timing_block!(timing_block_ptr))
        else
           nothing
        end

        after_expr = if !scoped_values_available || use_tls
            :($restore_current_timing_block!(prev_block_ptr))
        else
            nothing
        end

        scope_expr = if !scoped_values_available || use_tls
            nothing
        else
            :(Base.ScopedValues.Scope(Core.current_scope(), CURRENT_TIMING_BLOCK => timing_block_ptr))
        end

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
                    quote
                        $before_expr
                        $(Expr(:escape, ex))
                    end,
                    quote
                        ccall(:_jl_timing_block_end, Cvoid, (Ptr{Cvoid},), timing_block_ptr)
                        $after_expr
                    end,
                    :($scope_expr)
                ))
            end, :timing_block))
        end
    end
    macro zone(name, ex::Expr)
        return getzonedexpr(name, ex, :unknown_julia_function, __source__.file, __source__.line, 0)
    end
    @inline function timing_print(str)
        ccall(
            :jl_timing_puts,
            Cvoid,
            (Ptr{Cvoid}, Ptr{UInt8}),
            _get_current_timing_block(),
            string(str)
        )
    end
else
    macro zone(name, ex::Expr)
        return esc(ex)
    end
    timing_print(str::String) = nothing
end
