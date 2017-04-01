# Interacting With Julia

Julia comes with a full-featured interactive command-line REPL (read-eval-print loop) built into
the `julia` executable. In addition to allowing quick and easy evaluation of Julia statements,
it has a searchable history, tab-completion, many helpful keybindings, and dedicated help and
shell modes. The REPL can be started by simply calling `julia` with no arguments or double-clicking
on the executable:

```
$ julia
               _
   _       _ _(_)_     |  A fresh approach to technical computing
  (_)     | (_) (_)    |  Documentation: http://docs.julialang.org
   _ _   _| |_  __ _   |  Type "?help" for help.
  | | | | | | |/ _` |  |
  | | |_| | | | (_| |  |  Version 0.6.0-dev.2493 (2017-01-31 18:53 UTC)
 _/ |\__'_|_|_|\__'_|  |  Commit c99e12c* (0 days old master)
|__/                   |  x86_64-linux-gnu

julia>
```

To exit the interactive session, type `^D` -- the control key together with the `d` key on a blank
line -- or type `quit()` followed by the return or enter key. The REPL greets you with a banner
and a `julia>` prompt.

## The different prompt modes

### The Julian mode

The REPL has four main modes of operation. The first and most common is the Julian prompt. It
is the default mode of operation; each new line initially starts with `julia>`. It is here that
you can enter Julia expressions. Hitting return or enter after a complete expression has been
entered will evaluate the entry and show the result of the last expression.

```jldoctest
julia> string(1 + 2)
"3"
```

There are a number useful features unique to interactive work. In addition to showing the result,
the REPL also binds the result to the variable `ans`. A trailing semicolon on the line can be
used as a flag to suppress showing the result.

```jldoctest
julia> string(3 * 4);

julia> ans
"12"
```

In Julia mode, the REPL supports something called *prompt pasting*. This activates when pasting
text that starts with `julia> ` into the REPL. In that case, only expressions starting with
`julia> ` are parsed, others are removed. This makes it is possible to paste a chunk of code
that has been copied from a REPL session without having to scrub away prompts and outputs. This
feature is enabled by default but can be disabled or enabled at will with `Base.REPL.enable_promptpaste(::Bool)`.
If it is enabled, you can try it out by pasting the code block above this paragraph straight into
the REPL. This feature does not work on the standard Windows command prompt due to its limitation
at detecting when a paste occurs.

### Help mode

When the cursor is at the beginning of the line, the prompt can be changed to a help mode by typing
`?`. Julia will attempt to print help or documentation for anything entered in help mode:

```julia
julia> ? # upon typing ?, the prompt changes (in place) to: help?>

help?> string
search: string String stringmime Cstring Cwstring RevString readstring randstring bytestring SubString

  string(xs...)

  Create a string from any values using the print function.
```

Macros, types and variables can also be queried:

```
help?> @time
  @time

  A macro to execute an expression, printing the time it took to execute, the number of allocations,
  and the total number of bytes its execution caused to be allocated, before returning the value of the
  expression.

  See also @timev, @timed, @elapsed, and @allocated.

help?> AbstractString
search: AbstractString AbstractSparseMatrix AbstractSparseVector AbstractSet

  No documentation found.

  Summary:

  abstract AbstractString <: Any

  Subtypes:

  Base.Test.GenericString
  DirectIndexString
  String
```

Help mode can be exited by pressing backspace at the beginning of the line.

### [Shell mode](@id man-shell-mode)

Just as help mode is useful for quick access to documentation, another common task is to use the
system shell to execute system commands. Just as `?` entered help mode when at the beginning
of the line, a semicolon (`;`) will enter the shell mode. And it can be exited by pressing backspace
at the beginning of the line.

```julia
julia> ; # upon typing ;, the prompt changes (in place) to: shell>

shell> echo hello
hello
```

### Search modes

In all of the above modes, the executed lines get saved to a history file, which can be searched.
 To initiate an incremental search through the previous history, type `^R` -- the control key
together with the `r` key. The prompt will change to ```(reverse-i-search)`':```, and as you
type the search query will appear in the quotes. The most recent result that matches the query
will dynamically update to the right of the colon as more is typed. To find an older result using
the same query, simply type `^R` again.

Just as `^R` is a reverse search, `^S` is a forward search, with the prompt ```(i-search)`':```.
 The two may be used in conjunction with each other to move through the previous or next matching
results, respectively.

## Key bindings

The Julia REPL makes great use of key bindings. Several control-key bindings were already introduced
above (`^D` to exit, `^R` and `^S` for searching), but there are many more. In addition to the
control-key, there are also meta-key bindings. These vary more by platform, but most terminals
default to using alt- or option- held down with a key to send the meta-key (or can be configured
to do so).

| Keybinding          | Description                                                                      |
|:------------------- |:-------------------------------------------------------------------------------- |
| **Program control** |                                                                                  |
| `^D`                | Exit (when buffer is empty)                                                      |
| `^C`                | Interrupt or cancel                                                              |
| `^L`                | Clear console screen                                                             |
| Return/Enter, `^J`  | New line, executing if it is complete                                            |
| meta-Return/Enter   | Insert new line without executing it                                             |
| `?` or `;`          | Enter help or shell mode (when at start of a line)                               |
| `^R`, `^S`          | Incremental history search, described above                                      |
| **Cursor movement** |                                                                                  |
| Right arrow, `^F`   | Move right one character                                                         |
| Left arrow, `^B`    | Move left one character                                                          |
| Home, `^A`          | Move to beginning of line                                                        |
| End, `^E`           | Move to end of line                                                              |
| `^P`                | Change to the previous or next history entry                                     |
| `^N`                | Change to the next history entry                                                 |
| Up arrow            | Move up one line (or to the previous history entry)                              |
| Down arrow          | Move down one line (or to the next history entry)                                |
| Page-up             | Change to the previous history entry that matches the text before the cursor     |
| Page-down           | Change to the next history entry that matches the text before the cursor         |
| `meta-F`            | Move right one word                                                              |
| `meta-B`            | Move left one word                                                               |
| **Editing**         |                                                                                  |
| Backspace, `^H`     | Delete the previous character                                                    |
| Delete, `^D`        | Forward delete one character (when buffer has text)                              |
| meta-Backspace      | Delete the previous word                                                         |
| `meta-D`            | Forward delete the next word                                                     |
| `^W`                | Delete previous text up to the nearest whitespace                                |
| `^K`                | "Kill" to end of line, placing the text in a buffer                              |
| `^Y`                | "Yank" insert the text from the kill buffer                                      |
| `^T`                | Transpose the characters about the cursor                                        |
| `^Q`                | Write a number in REPL and press `^Q` to open editor at corresponding stackframe |


### Customizing keybindings

Julia's REPL keybindings may be fully customized to a user's preferences by passing a dictionary
to `REPL.setup_interface()`. The keys of this dictionary may be characters or strings. The key
`'*'` refers to the default action. Control plus character `x` bindings are indicated with `"^x"`.
Meta plus `x` can be written `"\\Mx"`. The values of the custom keymap must be `nothing` (indicating
that the input should be ignored) or functions that accept the signature `(PromptState, AbstractREPL, Char)`.
The `REPL.setup_interface()` function must be called before the REPL is initialized, by registering
the operation with `atreplinit()`. For example, to bind the up and down arrow keys to move through
history without prefix search, one could put the following code in `.juliarc.jl`:

```julia
import Base: LineEdit, REPL

const mykeys = Dict{Any,Any}(
  # Up Arrow
  "\e[A" => (s,o...)->(LineEdit.edit_move_up(s) || LineEdit.history_prev(s, LineEdit.mode(s).hist)),
  # Down Arrow
  "\e[B" => (s,o...)->(LineEdit.edit_move_up(s) || LineEdit.history_next(s, LineEdit.mode(s).hist))
)

function customize_keys(repl)
  repl.interface = REPL.setup_interface(repl; extra_repl_keymap = mykeys)
end

atreplinit(customize_keys)
```

Users should refer to `base/LineEdit.jl` to discover the available actions on key input.

## Tab completion

In both the Julian and help modes of the REPL, one can enter the first few characters of a function
or type and then press the tab key to get a list all matches:

```julia
julia> stri[TAB]
stride     strides     string      stringmime  strip

julia> Stri[TAB]
StridedArray    StridedMatrix    StridedVecOrMat  StridedVector    String
```

The tab key can also be used to substitute LaTeX math symbols with their Unicode equivalents,
and get a list of LaTeX matches as well:

```julia
julia> \pi[TAB]
julia> π
π = 3.1415926535897...

julia> e\_1[TAB] = [1,0]
julia> e₁ = [1,0]
2-element Array{Int64,1}:
 1
 0

julia> e\^1[TAB] = [1 0]
julia> e¹ = [1 0]
1×2 Array{Int64,2}:
 1  0

julia> \sqrt[TAB]2     # √ is equivalent to the sqrt() function
julia> √2
1.4142135623730951

julia> \hbar[TAB](h) = h / 2\pi[TAB]
julia> ħ(h) = h / 2π
ħ (generic function with 1 method)

julia> \h[TAB]
\hat              \hermitconjmatrix  \hkswarow          \hrectangle
\hatapprox        \hexagon           \hookleftarrow     \hrectangleblack
\hbar             \hexagonblack      \hookrightarrow    \hslash
\heartsuit        \hksearow          \house             \hspace

julia> α="\alpha[TAB]"   # LaTeX completion also works in strings
julia> α="α"
```

A full list of tab-completions can be found in the [Unicode Input](@ref) section of the manual.

Completion of paths works for strings and julia's shell mode:

```julia
julia> path="/[TAB]"
.dockerenv  .juliabox/   boot/        etc/         lib/         media/       opt/         root/        sbin/        sys/         usr/
.dockerinit bin/         dev/         home/        lib64/       mnt/         proc/        run/         srv/         tmp/         var/
shell> /[TAB]
.dockerenv  .juliabox/   boot/        etc/         lib/         media/       opt/         root/        sbin/        sys/         usr/
.dockerinit bin/         dev/         home/        lib64/       mnt/         proc/        run/         srv/         tmp/         var/
```

Tab completion can help with investigation of the available methods matching the input arguments:

```julia
julia> max([TAB] # All methods are displayed, not shown here due to size of the list

julia> max([1, 2], [TAB] # All methods where `Vector{Int}` matches as first argument
max(x, y) in Base at operators.jl:215
max(a, b, c, xs...) in Base at operators.jl:281

julia> max([1, 2], max(1, 2), [TAB] # All methods matching the arguments.
max(x, y) in Base at operators.jl:215
max(a, b, c, xs...) in Base at operators.jl:281
```

Keywords are also displayed in the suggested methods, see second line after `;` where `limit`
and `keep` are keyword arguments:

```julia
julia> split("1 1 1", [TAB]
split(str::AbstractString) in Base at strings/util.jl:278
split{T<:AbstractString}(str::T, splitter; limit, keep) in Base at strings/util.jl:254
```

The completion of the methods uses type inference and can therefore see if the arguments match
even if the arguments are output from functions. The function needs to be type stable for the
completion to be able to remove non-matching methods.

Tab completion can also help completing fields:

```julia
julia> Pkg.a[TAB]
add       available
```

Fields for output from functions can also be completed:

```julia
julia> split("","")[1].[TAB]
endof  offset  string
```

The completion of fields for output from functions uses type inference, and it can only suggest
fields if the function is type stable.

## Customizing Colors

The colors used by Julia and the REPL can be customized, as well. To change the color of the Julia
prompt you can add something like the following to your `juliarc.jl` file:

```julia
function customize_colors(repl)
    repl.prompt_color = Base.text_colors[:cyan]
end

atreplinit(customize_colors)
```

The available color keys can be seen by typing `Base.text_colors` in the help mode of the REPL.
In addition, the integers 0 to 255 can be used as color keys for terminals
with 256 color support.

You can also change the colors for the help and shell prompts and
input and answer text by setting the appropriate field of `repl` in the `customize_colors` function
above (respectively, `help_color`, `shell_color`, `input_color`, and `answer_color`). For the
latter two, be sure that the `envcolors` field is also set to false.

You can also customize the color used to render warning and informational messages by
setting the appropriate environment variables. For instance, to render error, warning, and informational
messages respectively in magenta, yellow, and cyan you can add the following to your `juliarc.jl` file:

```julia
ENV["JULIA_ERROR_COLOR"] = :magenta
ENV["JULIA_WARN_COLOR"] = :yellow
ENV["JULIA_INFO_COLOR"] = :cyan
```
