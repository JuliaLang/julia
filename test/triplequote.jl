# This file is a part of Julia. License is MIT: http://julialang.org/license

# triple-quote delimited strings
@test """abc""" == "abc"
@test """ab"c""" == "ab\"c"
@test """ab""c""" == "ab\"\"c"
@test """ab"\"c""" == "ab\"\"c"
@test """abc\"""" == "abc\""
n = 3
@test """$n\n""" == "$n\n"
@test """$(n)""" == "3"
@test """$(2n)""" == "6"
@test """$(n+4)""" == "7"
@test """$("string")""" == "string"
a = [3,1,2]
@test """$(a[2])""" == "1"
@test """$(a[3]+7)""" == "9"
@test """$(floor(Int,4.5))""" == "4"
nl = "
"
@test """
     a
     b

     c
     """ == "a$(nl)b$(nl)$(nl)c$(nl)"
@test """
      """ == ""
@test """x
     a
    """ == "x$(nl) a$(nl)"
@test """
     $n
   """ == "  $n$(nl)"
@test """
      a
     b
       c""" == " a$(nl)b$(nl)  c"
# tabs + spaces
@test """
	 a
	 b
	""" == " a$(nl) b$(nl)"
@test """
      a
       """ == "a$(nl) "
s = "   p"
@test """
      $s""" == "$s"
@test """
       $s
      """ == " $s$(nl)"
@test """\t""" == "\t"
@test """
      \t""" == ""
@test """
      foo
      \tbar""" == "foo$(nl)\tbar"
@test """
      foo
      \tbar
      """ == "foo$(nl)\tbar$(nl)"
@test """
      foo
      bar\t""" == "foo$(nl)bar\t"
@test """
      $("\n      ")
      """ == "\n      $(nl)"

