#!/usr/bin/env julia

const patterns = split("""
    *.1
    *.c
    *.cpp
    *.h
    *.inc
    *.jl
    *.lsp
    *.make
    *.md
    *.mk
    *.rst
    *.scm
    *.sh
    *.yml
    *Makefile
""")

# Note: `git ls-files` gives `/` as a path separator on Windows,
#   so we just use `/` for all platforms.
allow_tabs(path) =
    path == "Make.inc" ||
    endswith(path, "Makefile") ||
    endswith(path, ".make") ||
    endswith(path, ".mk") ||
    startswith(path, "src/support") ||
    startswith(path, "src/flisp") ||
    endswith(path, "test/syntax.jl") ||
    endswith(path, "test/triplequote.jl")

const errors = Set{Tuple{String,Int,String}}()

function check_whitespace()
    for path in eachline(`git ls-files -- $patterns`)
        lineno = 0
        non_blank = 0

        file_err(msg) = push!(errors, (path, 0, msg))
        line_err(msg) = push!(errors, (path, lineno, msg))

        isfile(path) || continue
        for line in eachline(path, keep=true)
            lineno += 1
            contains(line, '\r')   && file_err("non-UNIX line endings")
            contains(line, '\ua0') && line_err("non-breaking space")
            allow_tabs(path) ||
                contains(line, '\t')   && line_err("tab")
            endswith(line, '\n')   || line_err("no trailing newline")
            line = chomp(line)
            endswith(line, r"\s")  && line_err("trailing whitespace")
            contains(line, r"\S")  && (non_blank = lineno)
        end
        non_blank < lineno         && line_err("trailing blank lines")
    end

    if isempty(errors)
        println(stderr, "Whitespace check found no issues.")
        exit(0)
    else
        println(stderr, "Whitespace check found $(length(errors)) issues:")
        for (path, lineno, msg) in sort!(collect(errors))
            if lineno == 0
                println(stderr, "$path -- $msg")
            else
                println(stderr, "$path:$lineno -- $msg")
            end
        end
        exit(1)
    end
end

check_whitespace()
