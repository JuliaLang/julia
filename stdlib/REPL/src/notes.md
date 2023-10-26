Named Colors from `Base.text_colors`:
```
link                          \e[5m
light_white                    \e[97m
italic                         \e[3m
white                          \e[37m
light_cyan                     \e[96m
light_red                      \e[91m
blue                           \e[34m
cyan                           \e[36m
magenta                        \e[35m
black                          \e[30m
light_magenta                  \e[95m
default                        \e[39m
light_yellow                   \e[93m
nothing                        
hidden                         \e[8m
red                            \e[31m
light_black                    \e[90m
light_blue                     \e[94m
normal                         \e[0m
light_green                    \e[92m
green                          \e[32m
bold                           \e[1m
yellow                         \e[33m
reverse                        \e[7m
underline                      \e[4m
```

- [ ] Input color from `Base.input_color()`
- [ ] Answer color from `Base.answer_color()`

## PR Questions

In this PR I chose to modify instances of `String` with `Union{String,AnnotatedString}`, but I'm open to feedback regarding whether another choice is better. Perhaps widening it to `AbstractString` instead. I'm not familiar with how the compiler might handle these differently, etc.

At present this requires running julia with `--compiled-modules=no` in order for the styled prompt string `const`s to be initialized:

```julia
const JULIA_PROMPT = styled"{repl_prompt_julia:julia> }"
const PKG_PROMPT = # ... etc
```

When running with incremental precompilation, the above code causes issues with precompilation because it causes side-effects. I figured other stdlib/base macros might require similar precompilation-navigating logic, but after looking into it I realized that `styled` is rather special because it [calls eval](https://github.com/JuliaLang/StyledStrings.jl/blob/4777e6008108ac3b3408403c08d1e76e2e6f4c80/src/stylemacro.jl#L672).

I'm sure that the `_PROMPT` consts could be declared in a precompile-friendly way, but I wonder if this could be fixed within the `styled` macro. Maybe there are macros beyond base/stdlib which solve the same problem.

- [ ] report precompilation issue to `StyledStrings.jl`

Relevant PRs
https://github.com/JuliaLang/julia/pull/36689
https://github.com/JuliaLang/julia/pull/46474/


## PR 
Btw, you mentioned running into some trouble with `get(io, :color, false)`. Could I get your perspective on the existing color-related logic? Specifically regarding how the `:color` attribute is first initialized, down to it being

My impression while working around this code is that the color "propagation" throughout the REPL code down to the final `write` to the terminal is quite fragile, and I'm wondering if the complexity is necessary (to reflect a complex. Here are a few examples of the brittleness, and I'd like your input on which direction might be the most appropriate solution:
-  It took me quite a while to debug why the final `io::IO` struct in `StyledStrings/src/io.jl#_ansi_writer` never had `:color = true`. It turns out that somewhere in the callstack above in [LineEdit.jl](https://github.com/JuliaLang/julia/blob/c54a3f2b83bfca5f79a1b482abaa9b8e7f0da9ce/stdlib/REPL/src/LineEdit.jl#L617-L622) the soon-to-be-written output is first written to a new `IOBuffer`. So instead of a `UnixTerminal` being passed to `_ansi_writer` (where `:color = true`), an `IOBuffer` is passed, and anything which might behave in a way which would correctly say that `:color = true`, such as a `IOContext`, `REPL.Terminals.UnixTerminal` or other `IO` subtype is lost.
- methods throughout `REPL.jl` and `LineEdit.jl` have a `color` parameter that must be propagated all the way from `REPL.run_frontend` down through `print_response`