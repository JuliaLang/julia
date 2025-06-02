### Code Formatting Guidelines

#### General Formatting Guidelines for Julia code contributions

 - Follow the latest dev version of [Julia Style Guide](https://docs.julialang.org/en/v1/manual/style-guide/).
 - use whitespace to make the code more readable
 - no whitespace at the end of a line (trailing whitespace)
 - comments are good, especially when they explain the algorithm
 - try to adhere to a 92 character line length limit
 - it is generally preferred to use ASCII operators and identifiers over
   Unicode equivalents whenever possible
 - in docstrings refer to the language as "Julia" and the executable as "`julia`"

#### General Formatting Guidelines For C code contributions

 - 4 spaces per indentation level, no tabs
 - space between `if` and `(` (`if (x) ...`)
 - newline before opening `{` in function definitions
 - `f(void)` for 0-argument function declarations
 - newline between `}` and `else` instead of `} else {`
 - if one part of an `if..else` chain uses `{ }` then all should
 - no whitespace at the end of a line
