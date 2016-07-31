using PimpMyREPL.Tokenize
using Base.Test



# write your own tests here

tokvec(x) = collect(tokenize(x))

v = tokvec("function foo end")
