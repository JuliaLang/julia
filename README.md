# TerminalMenus

[![Build Status](https://travis-ci.org/nick-paul/TerminalMenus.jl.svg?branch=master)](https://travis-ci.org/nick-paul/TerminalMenus.jl)

[![Coverage Status](https://coveralls.io/repos/nick-paul/TerminalMenus.jl/badge.svg?branch=master&service=github)](https://coveralls.io/github/nick-paul/TerminalMenus.jl?branch=master)

[![codecov.io](http://codecov.io/github/nick-paul/TerminalMenus.jl/coverage.svg?branch=master)](http://codecov.io/github/nick-paul/TerminalMenus.jl?branch=master)


TerminalMenus.jl is a small package that enables interactive menus in the terminal. It is currently only supports linux. 

## Example

```julia

using TerminalMenus

options = ["apple", "orange", "grape", "strawberry",
            "blueberry", "peach", "lemon", "lime"]

menu = RadioMenu(options, pagesize=4)

choice = request("Choose your favorite fruit:", menu)

if choice != -1
    println("Your favorite fruit is ", options[choice], "!")
else
    println("Menu canceled.")
end

```
