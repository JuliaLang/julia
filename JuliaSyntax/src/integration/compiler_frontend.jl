# This file is a part of Julia. License is MIT: https://julialang.org/license

using .Base.CompilerFrontend

struct DefaultCompilerFrontend <: AbstractCompilerFrontend
    world::UInt
    syntax_version::VersionNumber
end

function CompilerFrontend.compiler_frontend(fe::DefaultCompilerFrontend, ver::VersionNumber)
    DefaultCompilerFrontend(fe.world, ver)
end

struct ParseResult
    stream::ParseStream
    rule::Symbol
    filename::String
    first_line::Int
end

function in_fixed_world(f, frontend)
    Base.invoke_in_world(frontend.world, f)
end

function CompilerFrontend.parsecode(frontend::DefaultCompilerFrontend, rule::Symbol, code::AbstractString,
                                    first_index::Integer; filename="none", first_line=1)
    in_fixed_world(frontend) do
        stream = ParseStream(code, first_index, version=frontend.syntax_version)
        parse!(stream; rule=rule, incremental=true)
        next_byte = last_byte(stream) + 1
        return (ParseResult(stream, rule, filename, first_line), next_byte)
    end
end

function CompilerFrontend.syntaxtree(frontend::DefaultCompilerFrontend,
                                     res::ParseResult)
    in_fixed_world(frontend) do
        syntaxtree(frontend, Expr, res)
    end
end

function CompilerFrontend.syntaxtree(frontend::DefaultCompilerFrontend, ::Type{Expr},
                                     res::ParseResult)
    in_fixed_world(frontend) do
        stream = res.stream
        ex = all_trivia(stream) ? nothing :
             build_base_compat_expr(stream, res.rule;
                                    filename=res.filename, first_line=res.first_line)
        return ex
    end
end

function CompilerFrontend.syntaxtree(frontend::DefaultCompilerFrontend, ::Type{T},
                                     res::ParseResult) where {T}
    stream = res.stream
    ex = all_trivia(stream) ? nothing :
         build_tree(T, stream; filename=res.filename, first_line=res.first_line)
    return ex
end

function CompilerFrontend.checkparse(frontend::DefaultCompilerFrontend, res::ParseResult;
                                     warn=false)
    in_fixed_world(frontend) do
        stream = res.stream
        if any_error(stream)
            throw(ParseError(stream; filename=res.filename, first_line=res.first_line))
        end
        if warn
            # TODO: Show warnings to logger instead of stdout
            show_diagnostics(stdout, stream)
        end
        nothing
    end
end

function CompilerFrontend.lower_init(frontend::DefaultCompilerFrontend, mod::Module, ex;
                                     mapexpr=nothing, filename="none", first_line=0, warn=true,
                                     logexpr=nothing, enter_toplevel=true)
    in_fixed_world(frontend) do
        # Note that `ex` should carry version information if it arises from another
        # module, but Expr doesn't have a way to represent this. So for now any
        # syntax changes need to be compatible enough that the compiler frontend
        # can detect and deal with all Expr variations up to VERSION.
        Base.FlispLoweringIterator(ex, first_line, filename, warn, enter_toplevel, mapexpr, logexpr)
    end
end
