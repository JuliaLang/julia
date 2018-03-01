# This file is a part of Julia. License is MIT: https://julialang.org/license

# editing and paging files

import Base.shell_split
using Base: find_source_file

"""
    editor()

Determine the editor to use when running functions like `edit`. Return an `Array` compatible
for use within backticks. You can change the editor by setting `JULIA_EDITOR`, `VISUAL` or
`EDITOR` as an environment variable.
"""
function editor()
    if Sys.iswindows() || Sys.isapple()
        default_editor = "open"
    elseif isfile("/etc/alternatives/editor")
        default_editor = realpath("/etc/alternatives/editor")
    else
        default_editor = "emacs"
    end
    # Note: the editor path can include spaces (if escaped) and flags.
    args = shell_split(get(ENV,"JULIA_EDITOR", get(ENV,"VISUAL", get(ENV,"EDITOR", default_editor))))
    isempty(args) && error("editor is empty")
    return args
end

"""
    edit(path::AbstractString, line::Integer=0)

Edit a file or directory optionally providing a line number to edit the file at.
Return to the `julia` prompt when you quit the editor. The editor can be changed
by setting `JULIA_EDITOR`, `VISUAL` or `EDITOR` as an environment variable.
"""
function edit(path::AbstractString, line::Integer=0)
    command = editor()
    name = basename(first(command))
    if endswith(path, ".jl")
        f = find_source_file(path)
        f !== nothing && (path = f)
    end
    background = true
    line_unsupported = false
    if startswith(name, "vim.") || name == "vi" || name == "vim" || name == "nvim" ||
            name == "mvim" || name == "nano" ||
            name == "emacs" && any(c -> c in ["-nw", "--no-window-system" ], command) ||
            name == "emacsclient" && any(c -> c in ["-nw", "-t", "-tty"], command)
        cmd = line != 0 ? `$command +$line $path` : `$command $path`
        background = false
    elseif startswith(name, "emacs") || name == "gedit" || startswith(name, "gvim")
        cmd = line != 0 ? `$command +$line $path` : `$command $path`
    elseif name == "textmate" || name == "mate" || name == "kate"
        cmd = line != 0 ? `$command $path -l $line` : `$command $path`
    elseif startswith(name, "subl") || startswith(name, "atom")
        cmd = line != 0 ? `$command $path:$line` : `$command $path`
    elseif name == "code" || (Sys.iswindows() && uppercase(name) == "CODE.EXE")
        cmd = line != 0 ? `$command -g $path:$line` : `$command -g $path`
    elseif startswith(name, "notepad++")
        cmd = line != 0 ? `$command $path -n$line` : `$command $path`
    elseif Sys.isapple() && name == "open"
        cmd = `open -t $path`
        line_unsupported = true
    else
        cmd = `$command $path`
        background = false
        line_unsupported = true
    end

    if Sys.iswindows() && name == "open"
        @static Sys.iswindows() && # don't emit this ccall on other platforms
            systemerror(:edit, ccall((:ShellExecuteW, "shell32"), stdcall, Int,
                                     (Ptr{Cvoid}, Cwstring, Cwstring, Ptr{Cvoid}, Ptr{Cvoid}, Cint),
                                     C_NULL, "open", path, C_NULL, C_NULL, 10) ≤ 32)
    elseif background
        spawn(pipeline(cmd, stderr=stderr))
    else
        run(cmd)
    end
    line != 0 && line_unsupported && println("Unknown editor: no line number information passed.\nThe method is defined at line $line.")

    nothing
end

"""
    edit(function, [types])

Edit the definition of a function, optionally specifying a tuple of types to
indicate which method to edit. The editor can be changed by setting `JULIA_EDITOR`,
`VISUAL` or `EDITOR` as an environment variable.
"""
edit(f)                   = edit(functionloc(f)...)
edit(f, @nospecialize t)  = edit(functionloc(f,t)...)
edit(file, line::Integer) = error("could not find source file for function")

# terminal pager

if Sys.iswindows()
    function less(file::AbstractString, line::Integer)
        pager = shell_split(get(ENV, "PAGER", "more"))
        g = pager[1] == "more" ? "" : "g"
        run(Cmd(`$pager +$(line)$(g) \"$file\"`, windows_verbatim = true))
    end
else
    function less(file::AbstractString, line::Integer)
        pager = shell_split(get(ENV, "PAGER", "less"))
        run(`$pager +$(line)g $file`)
    end
end

"""
    less(file::AbstractString, [line::Integer])

Show a file using the default pager, optionally providing a starting line number. Returns to
the `julia` prompt when you quit the pager.
"""
less(file::AbstractString) = less(file, 1)

"""
    less(function, [types])

Show the definition of a function using the default pager, optionally specifying a tuple of
types to indicate which method to see.
"""
less(f)                   = less(functionloc(f)...)
less(f, @nospecialize t)  = less(functionloc(f,t)...)
less(file, line::Integer) = error("could not find source file for function")
