# TerminalMenus

[![Build Status](https://travis-ci.org/nick-paul/TerminalMenus.jl.svg?branch=master)](https://travis-ci.org/nick-paul/TerminalMenus.jl) [![Build status](https://ci.appveyor.com/api/projects/status/weaqa64co5boj87g?svg=true)](https://ci.appveyor.com/project/nick-paul/terminalmenus-jl)

![Demo](http://npaul.co/files/TerminalMenus-demo.gif)

TerminalMenus.jl enables small, low-profile interactive menus in the terminal.


# Installation

TerminalMenus requires Julia 0.6. Use `Pkg` to install:

```
Pkg.add("TerminalMenus")
```


# Examples

```julia
using TerminalMenus

options = ["apple", "orange", "grape", "strawberry",
            "blueberry", "peach", "lemon", "lime"]

```

## RadioMenu

The RadioMenu allows the user to select one option from the list. The `request`
function displays the interactive menu and returns the index of the selected
choice. If a user presses 'q' or `ctrl-c`, `request` will return a `-1`.


```julia
# `pagesize` is the number of items to be displayed at a time.
#  The UI will scroll if the number of options is greater
#   than the `pagesize`
menu = RadioMenu(options, pagesize=4)

# `request` displays the menu and returns the index after the
#   user has selected a choice
choice = request("Choose your favorite fruit:", menu)

if choice != -1
    println("Your favorite fruit is ", options[choice], "!")
else
    println("Menu canceled.")
end

```

Output:

```
Choose your favorite fruit:
^  grape
   strawberry
 > blueberry
v  peach
Your favorite fruit is blueberry!
```

## MultiSelectMenu

The MultiSelectMenu allows users to select many choices from a list.

```julia
# here we use the default `pagesize` 10
menu = MultiSelectMenu(options)

# `request` returns a `Set` of selected indices
# if the menu us canceled (ctrl-c or q), return an empty set
choices = request("Select the fruits you like:", menu)

if length(choices) > 0
    println("You like the following fruits:")
    for i in choices
        println("  - ", options[i])
    end
else
    println("Menu canceled.")
end
```

Output:

```
Select the fruits you like:
[press: d=done, a=all, n=none]
   [ ] apple
 > [X] orange
   [X] grape
   [ ] strawberry
   [ ] blueberry
   [X] peach
   [ ] lemon
   [ ] lime
You like the following fruits:
  - orange
  - grape
  - peach
```

# Customization / Configuation

All interface customization is done through the keyword only
`TerminalMenus.config()` function.

## Arguments

 - `charset::Symbol=:na`: ui characters to use (`:ascii` or `:unicode`); overridden by other arguments
 - `cursor::Char='>'|'→'`: character to use for cursor
 - `up_arrow::Char='^'|'↑'`: character to use for up arrow
 - `down_arrow::Char='v'|'↓'`: character to use for down arrow
 - `checked::String="[X]"|"✓"`: string to use for checked
 - `unchecked::String="[ ]"|"⬚")`: string to use for unchecked
 - `scroll::Symbol=:na`: If `:wrap` then wrap the cursor around top and bottom, if :`nowrap` do not wrap cursor
 - `supress_output::Bool=false`: For testing. If true, menu will not be printed to console.
 - `ctrl_c_interrupt::Bool=true`: If `false`, return empty on ^C, if `true` throw InterruptException() on ^C

## Examples

```julia
julia> menu = MultiSelectMenu(options, pagesize=5);

julia> request(menu) # ASCII is used by default
[press: d=done, a=all, n=none]
   [ ] apple
   [X] orange
   [ ] grape
 > [X] strawberry
v  [ ] blueberry
Set([4, 2])

julia> TerminalMenus.config(charset=:unicode)

julia> request(menu)
[press: d=done, a=all, n=none]
   ⬚ apple
   ✓ orange
   ⬚ grape
 → ✓ strawberry
↓  ⬚ blueberry
Set([4, 2])

julia> TerminalMenus.config(checked="YEP!", unchecked="NOPE", cursor='⧐')

julia> request(menu)
[press: d=done, a=all, n=none]
   NOPE apple
   YEP! orange
   NOPE grape
 ⧐ YEP! strawberry
↓  NOPE blueberry
Set([4, 2])

```

# TODO

  - Nested menus
  - More customization?

---

*The interactive menu has been tested on Ubuntu 16.04 and windows 7, 8, & 10.
If there are any issues on your platform, please submit an issue or a pull
request.*
