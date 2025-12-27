# This file is a part of Julia. License is MIT: https://julialang.org/license

# Continuation of compiler_frontend.jl; separate for bootstrapping reasons.

# TODO: Make this usable when loading JuliaSyntax as a package
struct DefaultCompilerFrontend <: AbstractCompilerFrontend
    syntax_version::VersionNumber
end

function CompilerFrontend.compiler_frontend(::DefaultCompilerFrontend, ver::VersionNumber)
    DefaultCompilerFrontend(ver)
end

struct JuliaSyntaxParseResult
    stream::JuliaSyntax.ParseStream
    rule::Symbol
    filename::String
    first_line::Int
end

function CompilerFrontend.parsecode(frontend::DefaultCompilerFrontend, rule::Symbol, code::AbstractString,
                                    first_index::Integer; filename="none", first_line=1)
    stream = JuliaSyntax.ParseStream(code, first_index, version=frontend.syntax_version)
    JuliaSyntax.parse!(stream; rule=rule, incremental=true)
    next_byte = JuliaSyntax.last_byte(stream) + 1
    return (JuliaSyntaxParseResult(stream, rule, filename, first_line), next_byte)
end

function CompilerFrontend.syntaxtree(frontend::DefaultCompilerFrontend,
                                     res::JuliaSyntaxParseResult)
    syntaxtree(frontend, Expr, res)
end

function CompilerFrontend.syntaxtree(::AbstractCompilerFrontend, ::Type{Expr},
                                     res::JuliaSyntaxParseResult)
    stream = res.stream
    ex = JuliaSyntax.all_trivia(stream) ? nothing :
         JuliaSyntax.build_base_compat_expr(stream, res.rule;
                                            filename=res.filename, first_line=res.first_line)
    return ex
end

function CompilerFrontend.syntaxtree(::AbstractCompilerFrontend, ::Type{T},
                                     res::JuliaSyntaxParseResult) where {T}
    stream = res.stream
    ex = JuliaSyntax.all_trivia(stream) ? nothing :
         JuliaSyntax.build_tree(T, stream; filename=res.filename, first_line=res.first_line)
    return ex
end

function CompilerFrontend.checkparse(::AbstractCompilerFrontend, res::JuliaSyntaxParseResult;
                                     warn=false)
    stream = res.stream
    if JuliaSyntax.any_error(stream)
        throw(JuliaSyntax.ParseError(stream; filename=res.filename, first_line=res.first_line))
    end
    if warn
        # TODO: Show warnings to logger instead of stdout
        JuliaSyntax.show_diagnostics(stdout, stream)
    end
    nothing
end

function CompilerFrontend.lower_init(::DefaultCompilerFrontend, mod::Module, ex;
                                     mapexpr=nothing, filename="none", first_line=0, warn=true,
                                     logexpr=nothing, enter_toplevel=true)
    # Note that `ex` should carry version information if it arises from another
    # module, but Expr doesn't have a way to represent this. So for now any
    # syntax changes need to be compatible enough that the compiler frontend
    # can detect and deal with all Expr variations up to VERSION.
    FlispLoweringIterator(ex, first_line, filename, warn, enter_toplevel, mapexpr, logexpr)
end
