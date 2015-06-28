# This file is a part of Julia. License is MIT: http://julialang.org/license

# Tests of join()
@test join([]) == ""
@test join(["a"],"?") == "a"
@test join("HELLO",'-') == "H-E-L-L-O"
@test join(1:5, ", ", " and ") == "1, 2, 3, 4 and 5"
@test join(["apples", "bananas", "pineapples"], ", ", " and ") == "apples, bananas and pineapples"

# issue #9178 `join` calls `done()` twice on the iterables
type i9178
    nnext::Int64
    ndone::Int64
end
Base.start(jt::i9178) = (jt.nnext=0 ; jt.ndone=0 ; 0)
Base.done(jt::i9178, n) = (jt.ndone += 1 ; n > 3)
Base.next(jt::i9178, n) = (jt.nnext += 1 ; ("$(jt.nnext),$(jt.ndone)", n+1))
@test join(i9178(0,0), ";") == "1,1;2,2;3,3;4,4"

# quotes + interpolation (issue #455)
@test "$("string")" == "string"
arr = ["a","b","c"]
@test "[$(join(arr, " - "))]" == "[a - b - c]"
