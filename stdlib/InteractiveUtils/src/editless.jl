# This file is a part of Julia. License is MIT: https://julialang.org/license

# editing and paging files

import Base.shell_split
using Base: find_source_file

struct EditorEntry{P,Fn}
    pattern::P
    fn::Fn
    wait::Bool
    takesline::Bool
end
const editors = Vector{EditorEntry}(undef, 0)

struct EditorCommand{C,E}
    parsed_command::C
    entry::E
end
function parse_command(entry::EditorEntry, command, path, line)
    if entry.takesline
        cmd = entry.fn(command, path, line)
        if !isnothing(cmd)
            return EditorCommand(cmd, entry), true
        end
    end

    cmd = entry.fn(command, path)
    if !isnothing(cmd)
        return EditorCommand(cmd, entry), false
    end

    nothing, false
end

function Base.run(x::EditorCommand{Cmd})
    if !x.entry.wait
        run(pipeline(x.parsed_command, stderr=stderr), wait=false)
    else
        run(x.parsed_command)
    end
end
Base.run(x::EditorCommand{Function}) = x.parsed_command()

"""
    define_editor(fn, pattern;wait=false)

Define a new editor matching `pattern` that can be used to open a file
(possibly at a given line number) using `fn`.

The `fn` argument is a function that determines how to open a file with the
given editor.  It should take 2 or 3 arguments, as follows:

* `command` - an array of strings representing the editor command. It can be
  safely interpolated into a command created using backtick notation.
* `path`  - the path to the source file to open
* `line` - the optional line number to open to; if specified the returned command
  must open the file at the given line.

`fn` must return either an appropriate `Cmd` object to open a
file, a function (taking 0 arguments) that will open the file directly
(returning a `Cmd` is the preferred approach), or `nothing`. Use `nothing`
to indicate that this editor is not appropriate for the current environment
and another editor should be attempted.

The `pattern` argument is a string, regular expression, or an array of strings
and regular expressions. For the `fn` to be called one of the patterns must
match the value of `EDITOR`, `VISUAL` or `JULIA_EDITOR`.  For strings, only
whole words can match (i.e. "vi" doesn't match "vim -g" but will match
"/usr/bin/vi -m").

If multiple defined editors match, the one most recently defined will be
used.

By default julia does not wait for the editor to close, running it in the
background. However, if the editor is terminal based, you will probably want to
set `wait=true` and julia will wait for the editor to close before resuming.

If no editor entry can be found, then a file is opened by running
`\$command \$path`.

Note that a number of default editors (all priority 0) are already defined. All
of the following commands should already work:

- emacs
- vim
- nvim
- nano
- textmate
- mate
- kate
- subl
- atom
- notepad++
- Visual Studio Code
- open

# Example:
The following defines the usage of terminal-based `emacs`:

    define_editor(r"\bemacs\b.*(-nw|--no-window-system)",
                  wait=true) do cmd, path, line
        `\$cmd +\$line \$path`
    end
"""

function define_editor(fn, pattern; wait=false, priority=0)
    nargs = map(x -> x.nargs - 1, methods(fn).ms)
    has3args = 3 ∈ nargs
    has2args = 2 ∈ nargs
    entry = EditorEntry(pattern, fn, wait, has3args)
    pushfirst!(editors, entry)

    if !(has3args || has2args)
        error("Editor function must take 2 or 3 arguments")
    end
end

function define_default_editors()
    define_editor(["vim","vi","nvim","mvim","nano"],
                  wait=true) do cmd, path, line
        `$cmd +$line $path`
    end
    define_editor([r"\bemacs","gedit",r"\bgvim"]) do cmd, path, line
        `$cmd +$line $path`
    end
    define_editor(r"\bemacs\b.*(-nw|--no-window-system)",
                  wait=true) do cmd, path, line
        `$cmd +$line $path`
    end
    define_editor(r"\bemacsclient\b.*(-nw|-t|-tty)",
                  wait=true) do cmd, path, line
        `$cmd +$line $path`
    end
    define_editor(["textmate","mate","kate"]) do cmd, path, line
        `$cmd $path -l $line`
    end
    define_editor([r"\bsubl",r"\batom"]) do cmd, path, line
        `$cmd $path:$line`
    end
    define_editor("code") do cmd, path, line
        `$cmd -g $path:$line`
    end
    define_editor(r"\bnotepad++") do cmd, path, line
        `$cmd $path -n$line`
    end
    if Sys.iswindows()
        define_editor(r"\bCODE\.EXE\b"i) do cmd, path, line
            `$cmd -g $path:$line`
        end
        define_editor("open") do cmd, path, line
            function()
                @static if Sys.iswindows() # don't emit this ccall on other platforms
                    result = ccall((:ShellExecuteW, "shell32"), stdcall,
                                   Int, (Ptr{Cvoid}, Cwstring, Cwstring,
                                         Ptr{Cvoid}, Ptr{Cvoid}, Cint),
                                   C_NULL, "open", path, C_NULL, C_NULL, 10)
                    systemerror(:edit, result ≤ 32)
                end
            end
        end
    elseif Sys.isapple()
        define_editor("open") do cmd, path
            `open -t $path`
        end
    end
end

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
    args = shell_split(get(ENV, "JULIA_EDITOR",
                           get(ENV,"VISUAL", get(ENV,"EDITOR", default_editor))))
    isempty(args) && error("editor is empty")
    return args
end

editormatches(pattern::String, command) =
    occursin(Regex("\\b"*pattern*"\\b"), command)
editormatches(pattern::Regex, command) =
    occursin(pattern, command)
editormatches(pattern::AbstractArray, command) =
    any(x -> editormatches(x, command), pattern)
findeditors(command) =
    filter(e -> editormatches(e.pattern,join(command," ")), editors)

"""
    edit(path::AbstractString, line::Integer=0)

Edit a file or directory optionally providing a line number to edit the file at.
Return to the `julia` prompt when you quit the editor. The editor can be changed
by setting `JULIA_EDITOR`, `VISUAL` or `EDITOR` as an environment variable.
To ensure that the file can be opened at the given line, you may need to
call `define_editor` first.
"""
function edit(path::AbstractString, line::Integer=0)
    !isempty(editors) || define_default_editors()
    command = editor()
    if endswith(path, ".jl")
        f = find_source_file(path)
        f !== nothing && (path = f)
    end

    parsed = nothing
    line_supported = false
    for entry in findeditors(command)
        parsed, line_supported = parse_command(entry, command, path, line)
        isnothing(parsed) || break
    end
    if isnothing(parsed)
        parsed = `$command $path`
        line_supported = false
    end

    if line != 0 && !line_supported
        @info("Unknown editor: no line number information passed.\n"*
              "The method is defined at line $line.")
    end
    run(parsed)

    nothing
end

"""
    edit(function, [types])
    edit(module)

Edit the definition of a function, optionally specifying a tuple of types to indicate which
method to edit. For modules, open the main source file. The module needs to be loaded with
`using` or `import` first.

!!! compat "Julia 1.1"
    `edit` on modules requires at least Julia 1.1.

To ensure that the file can be opened at the given line, you may need to call
`define_editor` first.
"""
edit(f)                   = edit(functionloc(f)...)
edit(f, @nospecialize t)  = edit(functionloc(f,t)...)
edit(file, line::Integer) = error("could not find source file for function")
edit(m::Module) = edit(pathof(m))

# terminal pager

if Sys.iswindows()
    function less(file::AbstractString, line::Integer)
        pager = shell_split(get(ENV, "PAGER", "more"))
        g = pager[1] == "more" ? "" : "g"
        run(Cmd(`$pager +$(line)$(g) \"$file\"`, windows_verbatim = true))
        nothing
    end
else
    function less(file::AbstractString, line::Integer)
        pager = shell_split(get(ENV, "PAGER", "less"))
        run(`$pager +$(line)g $file`)
        nothing
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
