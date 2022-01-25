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
              error("No message in diagnostic")
    level = !isnothing(error) ? :error : :warning
    Diagnostic(first_byte, last_byte, level, message)
end

first_byte(d::Diagnostic) = d.first_byte
last_byte(d::Diagnostic)  = d.last_byte
is_error(d::Diagnostic)   = d.level == :error

function show_diagnostic(io::IO, diagnostic::Diagnostic, source::SourceFile)
    col,prefix = diagnostic.level == :error   ? (:light_red, "Error")      :
                 diagnostic.level == :warning ? (:light_yellow, "Warning") :
                 diagnostic.level == :note    ? (:light_blue, "Note")      :
                 (:normal, "Info")
    printstyled(io, "$prefix: ", color=col)
    print(io, diagnostic.message, ":\n")

    p = first_byte(diagnostic)
    q = last_byte(diagnostic)
    code = source.code
    if q < p || (p == q && code[p] == '\n')
        # An empty or invisible range!  We expand it symmetrically to make it
        # visible.
        p = max(firstindex(code), prevind(code, p))
        q = min(lastindex(code), nextind(code, q))
    end

    # p and q mark the start and end of the diagnostic range. For context,
    # buffer these out to the surrouding lines.
    a,b = source_line_range(source, p, context_lines_before=2, context_lines_after=1)
    c,d = source_line_range(source, q, context_lines_before=1, context_lines_after=2)

    hicol = (100,40,40)

    print(io, source[a:prevind(code, p)])
    # There's two situations, either
    if b >= c
        # The diagnostic range is compact and we show the whole thing
        # a...............
        # .....p...q......
        # ...............b
        _printstyled(io, source[p:q]; color=hicol)
    else
        # Or large and we trucate the code to show only the region around the
        # start and end of the error.
        # a...............
        # .....p..........
        # ...............b
        # (snip)
        # c...............
        # .....q..........
        # ...............d
        _printstyled(io, source[p:b]; color=hicol)
        println(io, "…")
        _printstyled(io, source[c:q]; color=hicol)
    end
    print(io, source[nextind(code,q):d])
    println(io)
end

function show_diagnostics(io::IO, diagnostics::AbstractVector{Diagnostic}, code::SourceFile)
    for d in diagnostics
        show_diagnostic(io, d, code)
    end
end

function show_diagnostics(io::IO, diagnostics::AbstractVector{Diagnostic}, code)
    if !isempty(diagnostics)
        show_diagnostics(io, diagnostics, SourceFile(code))
    end
end

function any_error(diagnostics::AbstractVector{Diagnostic})
    any(is_error(d) for d in diagnostics)
end
