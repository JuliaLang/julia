if ccall(:jl_timing_enabled, Cint, ()) != 0
    if isdefined(@__MODULE__, :task_local_storage)
        # Consider using ScopedValues when it is fast, c.f https://github.com/topolarity/Tracy.jl/pull/36
        @inline function _get_current_timing_block_nocheck()
            ref_block = get(task_local_storage(), :timing_block_tls_key, nothing)::Union{Nothing, RefValue{Ptr{Cvoid}}}
            return ref_block === nothing ? C_NULL : ref_block[]
        end
        @inline function set_current_timing_block!(block::Ptr{Cvoid})
            ref_block = get!(()->RefValue(C_NULL), task_local_storage(), :timing_block_tls_key)::RefValue{Ptr{Cvoid}}
            old = ref_block[]
            ref_block[] = block
            return (old, ref_block)
        end
        @inline function restore_current_timing_block!((old, ref)::Tuple{Ptr{Cvoid}, RefValue{Ptr{Cvoid}}})
            ref[] = old
        end
        @inline function timing_print(str::Union{String, Symbol})
            ccall(
                :jl_timing_puts,
                Cvoid,
                (Ptr{Cvoid}, Ptr{UInt8}),
                _get_current_timing_block(),
                str
            )
        end
    else
        set_current_timing_block!(block::Ptr{Cvoid}) = nothing
        restore_current_timing_block!(::Nothing) = nothing
        timing_print(str::Union{String, Symbol}) = error("can not add text to zones when thread local storage is not available")
    end

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
            local timing_block = RefValue($buffer)
            local timing_block_ptr = pointer_from_objref(timing_block)
            $(Expr(:gc_preserve, quote
                ccall(:_jl_timing_block_init, Cvoid, (Ptr{Cvoid}, Csize_t, Ptr{Cvoid}), timing_block_ptr, $buffer_size, $event[])
                ccall(:_jl_timing_block_start, Cvoid, (Ptr{Cvoid},), timing_block_ptr)
                $(Expr(:tryfinally,
                    quote
                        timing_block_data = $set_current_timing_block!(timing_block_ptr)
                        $(Expr(:escape, ex))
                    end,
                    quote
                        ccall(:_jl_timing_block_end, Cvoid, (Ptr{Cvoid},), timing_block_ptr)
                        $restore_current_timing_block!(timing_block_data)
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
    timing_print(str::Union{String, Symbol}) = nothing
end
