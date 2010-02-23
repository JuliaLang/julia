(include "utils.scm")
(include "match.scm")
(include "julia-parser.scm")
(include "julia-syntax.scm")

(gambit-only
 (c-define (jl-parse-string s) (char-string) scheme-object "jl_parse_string" ""
   (julia-parse s))
 
 )
