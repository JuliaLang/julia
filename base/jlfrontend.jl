module JLFrontend
    using CSTParser
    using FancyDiagnostics

    function ps_diag_to_expr(ps, str, filename="REPL")
        diag = FancyDiagnostics.REPLDiagnostic(filename, str, ps.errors)
        return FancyDiagnostics.BaseHooks.is_incomplete(diag) ? Expr(:incomplete, diag) : Expr(:error, diag)
    end

    function jl_parse_string(str::Ptr{UInt8}, len::Csize_t, pos0::Cint, greedy::Cint)
        greedy = greedy != 0
        # Non-greedy mode not yet supported
        @assert greedy
        io = IOBuffer(unsafe_wrap(Array, str, (len,)))
        seek(io, pos0)
        ps = CSTParser.ParseState(io)
        result, ps = CSTParser.parse(ps)
        !isempty(ps.errors) && return ps_diag_to_expr(ps, unsafe_string(str, len)), pos0 + result.fullspan
        Expr(result), pos0 + result.fullspan
    end

    function jl_parse_all(str::Ptr{UInt8}, len::Csize_t, filename::Ptr{UInt8}, filename_len::Csize_t)
        str = unsafe_string(str, len)
        ps = CSTParser.ParseState(str)
        top = Expr(:toplevel)
        while !ps.done && isempty(ps.errors)
            result, ps = CSTParser.parse(ps)
            !isempty(ps.errors) && return ps_diag_to_expr(ps, str, unsafe_string(filename, filename_len))
            push!(top.args, Expr(result))
        end
        top
    end

    struct jl_frontend_t
        init::Ptr{Cvoid}

        jl_parse_all::Ptr{Cvoid}
        jl_parse_string::Ptr{Cvoid}
        jl_parse_eval_all::Ptr{Cvoid}

        jl_macroexpand::Ptr{Cvoid}
        jl_macroexpand1::Ptr{Cvoid}
        jl_expand_with_loc::Ptr{Cvoid}
        jl_expand_stmt_with_loc::Ptr{Cvoid}

        jl_is_operator::Ptr{Cvoid}
        jl_is_unary_operator::Ptr{Cvoid}
        jl_is_unary_and_binary_operator::Ptr{Cvoid}
        jl_operator_precedence::Ptr{Cvoid}
    end

    struct FrontendRef
        ptr::Ptr{jl_frontend_t}
    end
    FrontendRef() = FrontendRef(cglobal(:jl_frontend, jl_frontend_t))
    Base.getproperty(fr::FrontendRef, sym::Symbol) = unsafe_load(Ptr{Ptr{Cvoid}}(getfield(fr, :ptr)) + fieldoffset(jl_frontend_t, Base.fieldindex(jl_frontend_t, sym)))
    Base.setproperty!(fr::FrontendRef, sym::Symbol, v::Ptr{Cvoid}) = unsafe_store!(Ptr{Ptr{Cvoid}}(getfield(fr, :ptr)) + fieldoffset(jl_frontend_t, Base.fieldindex(jl_frontend_t, sym)), v)

    function init_frontend!()
        frontend = FrontendRef()
        frontend.jl_parse_string = @cfunction(jl_parse_string, Any, (Ptr{UInt8}, Csize_t, Cint, Cint))
        frontend.jl_parse_all = @cfunction(jl_parse_all, Any, (Ptr{UInt8}, Csize_t, Ptr{UInt8}, Csize_t))
    end

    __init__() = init_frontend!()
end
