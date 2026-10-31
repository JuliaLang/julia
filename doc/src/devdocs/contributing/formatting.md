# Code Formatting Guidelines

## General Formatting Guidelines for Julia code contributions

 - Follow the latest dev version of [Julia Style Guide](https://docs.julialang.org/en/v1/manual/style-guide/).
 - Use whitespace to make the code more readable
 - No whitespace at the end of a line (trailing whitespace)
 - Comments are good, especially when they explain the algorithm
 - Try to adhere to a 92 character line length limit
 - It is generally preferred to use ASCII operators and identifiers over
   Unicode equivalents whenever possible
 - In docstrings refer to the language as "Julia" and the executable as "`julia`"

## General Formatting Guidelines For C code contributions

 - 4 spaces per indentation level, no tabs
 - Space between `if` and `(` (`if (x) ...`)
 - Newline before opening `{` in function definitions
 - `f(void)` for 0-argument function declarations
 - Newline between `}` and `else` instead of `} else {`
 - If one part of an `if..else` chain uses `{ }` then all should
 - No whitespace at the end of a line
