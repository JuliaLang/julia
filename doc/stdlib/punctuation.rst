*************
 Punctuation
*************

Extended documentation for mathematical symbols & functions is :ref:`here <mathematical-operators>`.

   =========   ================================================
   symbol      meaning
   =========   ================================================
   ``@m``      invoke macro ``m``; followed by space-separated expressions
   ``!``       prefix "not" operator
   ``a!( )``   at the end of a function name, `!` indicates that a function modifies its argument(s)
   ``#``       begin single line comment
   ``#=``      begin multi-line comment (these are nestable)
   ``=#``      end multi-line comment
   ``$``       bitwise xor operator, string and expression interpolation
   ``%``       remainder operator
   ``^``       exponent operator
   ``&``       bitwise and
   ``&&``      short-circuiting boolean and
   ``|``       bitwise or
   ``||``      short-circuiting boolean or
   ``*``       multiply, or matrix multiply
   ``()``      the empty tuple
   ``~``       bitwise not operator
   ``\``       backslash operator
   ``'``       complex transpose operator A\ :sup:`H`
   ``a[]``     array indexing
   ``[,]``     vertical concatenation
   ``[;]``     also vertical concatenation
   ``[  ]``    with space-separated expressions, horizontal concatenation
   ``T{ }``    parametric type instantiation
   ``{  }``    construct a cell array (deprecated in 0.4 in favor of ``Any[]``)
   ``;``       statement separator
   ``,``       separate function arguments or tuple components
   ``?``       3-argument conditional operator (conditional ? if_true : if_false)
   ``""``      delimit string literals
   ``''``      delimit character literals
   ``` ```     delimit external process (command) specifications
   ``...``     splice arguments into a function call or declare a varargs function or type
   ``.``       access named fields in objects or names inside modules, also prefixes elementwise operators
   ``a:b``     range a, a+1, a+2, ..., b
   ``a:s:b``   range a, a+s, a+2s, ..., b
   ``:``       index an entire dimension (1:end)
   ``::``      type annotation, depending on context
   ``:( )``    quoted expression
   ``:a``      symbol a
   =========   ================================================

