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
    color,prefix = diagnostic.level == :error   ? (:light_red, "Error")      :
                   diagnostic.level == :warning ? (:light_yellow, "Warning") :
                   diagnostic.level == :note    ? (:light_blue, "Note")      :
                   (:normal, "Info")
    line, col = source_location(source, first_byte(diagnostic))
    linecol = "$line:$col"
    if !isnothing(source.filename)
        locstr = "$(source.filename):$linecol"
        if get(io, :color, false)
            # Also add hyperlinks in color terminals
            url = "file://$(abspath(source.filename))#$linecol"
            locstr = "\e]8;;$url\e\\$locstr\e]8;;\e\\"
        end
    else
        locstr = "line $linecol"
    end
    print(io, prefix, ": ")
    printstyled(io, diagnostic.message, color=color)
    printstyled(io, "\n", "@ $locstr", color=:light_black)
    print(io, "\n")

    p = first_byte(diagnostic)
    q = last_byte(diagnostic)
    text = sourcetext(source)
    if q < p || (p == q && source[p] == '\n')
        # An empty or invisible range!  We expand it symmetrically to make it
        # visible.
        p = max(firstindex(text), prevind(text, p))
        q = min(lastindex(text), nextind(text, q))
    end

    # p and q mark the start and end of the diagnostic range. For context,
    # buffer these out to the surrouding lines.
    a,b = source_line_range(source, p, context_lines_before=2, context_lines_after=1)
    c,d = source_line_range(source, q, context_lines_before=1, context_lines_after=2)

    hicol = (100,40,40)

    # TODO: show line numbers on left

    print(io, source[a:prevind(text, p)])
    # There's two situations, either
    if b >= c
        # The diagnostic range is compact and we show the whole thing
        # a...............
        # .....p...q......
        # ...............b
        _printstyled(io, source[p:q]; bgcolor=hicol)
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
        _printstyled(io, source[p:b]; bgcolor=hicol)
        println(io, "…")
        _printstyled(io, source[c:q]; bgcolor=hicol)
    end
    print(io, source[nextind(text,q):d])
    println(io)
end

function show_diagnostics(io::IO, diagnostics::AbstractVector{Diagnostic}, source::SourceFile)
    for d in diagnostics
        show_diagnostic(io, d, source)
    end
end

function show_diagnostics(io::IO, diagnostics::AbstractVector{Diagnostic}, text::AbstractString)
    if !isempty(diagnostics)
        show_diagnostics(io, diagnostics, SourceFile(text))
    end
end

function any_error(diagnostics::AbstractVector{Diagnostic})
    any(is_error(d) for d in diagnostics)
end
