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

const is_gha = something(tryparse(Bool, get(ENV, "GITHUB_ACTIONS", "false")), false)

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

function check_whitespace()
    # Get file list from ARGS if provided, otherwise use git ls-files
    errors = Set{Tuple{String,Int,String}}()
    files_to_check = filter(arg -> arg != "--fix", ARGS)
    if isempty(files_to_check)
        files_to_check = eachline(`git ls-files -- $patterns`)
    end

    files_fixed = 0
    if "--fix" in ARGS
        for path in files_to_check
            content = newcontent = read(path, String)
            isempty(content) && continue
            if !allow_tabs(path)
                tabpattern = r"^([ \t]+)"m => (x -> replace(x, r"((?: {4})*)( *\t)" => s"\1    ")) # Replace tab sequences at start of line after any number of 4-space groups
                newcontent = replace(newcontent, tabpattern)
            end
            newcontent = replace(newcontent,
                r"\s*$" => '\n',                # Remove trailing whitespace and normalize line ending at eof
                r"\s*?[\r\n]" => '\n',          # Remove trailing whitespace and normalize line endings on each line
                r"\xa0" => ' '                  # Replace non-breaking spaces
            )
            if content != newcontent
                write(path, newcontent)
                files_fixed += 1
            end
        end
        if files_fixed > 0
            println(stderr, "Fixed whitespace issues in $files_fixed files.")
        end
    end

    for path in files_to_check
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
                if is_gha
                    println(stdout, "::warning title=Whitespace check,file=", path, "::", msg)
                end
            else
                println(stderr, "$path:$lineno -- $msg")
                if is_gha
                    println(stdout, "::warning title=Whitespace check,file=", path, ",line=", lineno, "::", msg)
                end
            end
        end
        exit(1)
    end
end

check_whitespace()
