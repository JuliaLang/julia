
***********
 Variables
***********

Julia provides an extremely flexible system for naming variables.
Capitalization carries no semantic meaning, nor does the first letter of a
variable. ::

    julia> ix = 1.0
    1.0

    julia> y = -3
    -3

    julia> Z = "My string"
    "My string"

    julia> customary_phrase = "Hello world!"
    "Hello world!"

    julia> BeginningOfTheUniversalDeclarationOfHumanRightsInChinese = "人人生而自由，在尊严和权力上一律平等。"
    "人人生而自由，在尊严和权力上一律平等。"

They can even be given Unicode names::

    julia> δ = 0.00001
    0.00001

    julia> 안녕하세요 = "Hello" 
    "Hello"

Julia will even let you redefine built-in constants and functions if needed::

    julia> pi
           
    3.141592653589793
    
    julia> pi = 3
    Warning: imported binding for pi overwritten in module Main
    3
    
    julia> pi
    3
    
    julia> sqrt = 4
    4
    
However, this is obviously not recommended to avoid potential confusion.

The only explicitly disallowed names for variables are the names of built-in
statements::

    julia> else = false
    ERROR: syntax: unexpected else
    
    julia> try = "No"
    ERROR: syntax: unexpected =


Stylistic convention
====================

While Julia imposes few restrictions on valid names, it has become useful to
adopt the following conventions on names in Julia:

- Names of variables are in lower case, with word separation indicated by
  underscores (``'\_'``).
- Names of ``Type``\ s begin with a capital letter and word separation is
  shown with CamelCase instead of underscores.
- Names of ``function``\ s and ``macro``\s are in lower case, without
  underscores.
- Functions that modify their inputs have names that end in ``!``. These
  functions are sometimes called mutating functions or in-place functions.

