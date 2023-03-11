"""
    Diagnostic(first_byte, last_byte; [error="msg" | warning="msg"])

A diagnostic message, referring to the source code byte range
first_byte:last_byte, with a `warning` or `error` message.

Messages should be concise, matter-of-fact and not include decorations:

* Concise: "Show don't tell". Where possible, let's show the user what's wrong
  by annotating their original source code via the byte range.
* Matter-of-fact: Admonishing the user isn't helpful. Let's gently show them
  what's wrong instead, using a neutral tone.
* Decorations: Capitalization, punctuation and diagnostic class ("error" /
  "warning") should be omitted. These decorations will be added by the
  formatting code.

TODO: At some point we should enhance Diagnostic to allow multiple sub-ranges
for better annotation. Let's follow the excellent precedent set by Rust's
[rustc_errors::Diagnostic](https://doc.rust-lang.org/stable/nightly-rustc/rustc_errors/struct.Diagnostic.html).

TODO: We should cater for extended descriptions containing multiple sentences
via a diagnostic code which can be used to look up detailed information. Again,
Rust does this well.
"""
struct Diagnostic
    first_byte::Int
    last_byte::Int
    level::Symbol
    message::String
end

function Diagnostic(first_byte, last_byte; error=nothing, warning=nothing)
    message = !isnothing(error)   ? error :
              !isnothing(warning) ? warning :
              Base.error("No message in diagnostic")
    level = !isnothing(error) ? :error : :warning
    Diagnostic(first_byte, last_byte, level, message)
end

first_byte(d::Diagnostic) = d.first_byte
last_byte(d::Diagnostic)  = d.last_byte
is_error(d::Diagnostic)   = d.level == :error
Base.range(d::Diagnostic) = first_byte(d):last_byte(d)

# Make relative path into a file URL
function _file_url(filename)
    @static if Sys.iswindows()
        # TODO: Test this with windows terminal
        path = replace(abspath(filename), '\\'=>'/')
    else
        path = abspath(filename)
    end
    "file://$(path)"
end

function show_diagnostic(io::IO, diagnostic::Diagnostic, source::SourceFile)
    color,prefix = diagnostic.level == :error   ? (:light_red, "Error")      :
                   diagnostic.level == :warning ? (:light_yellow, "Warning") :
                   diagnostic.level == :note    ? (:light_blue, "Note")      :
                   (:normal, "Info")
    line, col = source_location(source, first_byte(diagnostic))
    linecol = "$line:$col"
    filename = source.filename
    file_href = nothing
    if !isnothing(filename)
        locstr = "$filename:$linecol"
        if !startswith(filename, "REPL[")
            file_href = _file_url(filename)*"#$linecol"
        end
    else
        locstr = "line $linecol"
    end
    _printstyled(io, "# $prefix @ ", fgcolor=:light_black)
    _printstyled(io, "$locstr", fgcolor=:light_black, href=file_href)
    print(io, "\n")
    highlight(io, source, range(diagnostic),
              note=diagnostic.message, notecolor=color,
              context_lines_before=1, context_lines_after=0)
end

function show_diagnostics(io::IO, diagnostics::AbstractVector{Diagnostic}, source::SourceFile)
    first = true
    for d in diagnostics
        first || println(io)
        first = false
        show_diagnostic(io, d, source)
    end
end

function show_diagnostics(io::IO, diagnostics::AbstractVector{Diagnostic}, text::AbstractString)
    show_diagnostics(io, diagnostics, SourceFile(text))
end

function emit_diagnostic(diagnostics::AbstractVector{Diagnostic},
                         fbyte::Integer, lbyte::Integer; kws...)
    push!(diagnostics, Diagnostic(fbyte, lbyte; kws...))
end

function any_error(diagnostics::AbstractVector{Diagnostic})
    any(is_error(d) for d in diagnostics)
end
