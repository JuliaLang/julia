# test metadata.jl

# SETUP
##
Base.emptymeta!()

function fillmeta(objs)
    for (i,te) in enumerate(objs)
        setmeta!(te, i, -i)
    end
end

abstract atype
type btype
    val
end
module amod7586
a = 5
end
# user-defined metadata storage:
import Base: getmeta, getmeta!, hasmeta, setmeta!, deletemeta!
type MyType6498
    val::Int
    _metadata::MetaData
    MyType6498(a) = new(a, MetaData())
end
hasmeta(obj::MyType6498) = !isempty(obj._metadata)
getmeta(obj::MyType6498) = hasmeta(obj) ?  obj._metadata : throw(KeyError(obj))
getmeta!(obj::MyType6498) = obj._metadata
setmeta!(obj::MyType6498, md::MetaData) = obj._metadata = md

deletemeta!(obj::MyType6498) = (obj._metadata = MetaData(); nothing)

# other objects
fn12(x) = x+1
nu = 5
ar = [3,4,5]
ra = 1:10
di = ["a"=>5, "b"=>7]
st = "asdf"
re = r"^\s*(?:#|$)"s
ex = :(4+5)
sym = :a
tu =  (5, 't')
global gl = 5.6
myt = MyType6498(5)

totest = {
          # generic functions/methods
          fn12, methods(fn12).defs,*, :,
          # non-generic function
          isa,
          # variables
          ENV, STDOUT, nu, ar, ra, di, st, re, ex, sym, tu, 
          # types
          atype, btype,
          # modules
          amod7586,
          # misc
          Intrinsics.mul_int, ANY, VecOrMat, C_NULL, Intrinsics.ccall, pi,
          myt,  # this has metadata
          }

# TESTS
## empty:
testkey = 5
for te in totest
    @test_throws KeyError getmeta(te)
    @test isequal(getmeta(te, testkey, nothing), nothing)
end

# test clearing
fillmeta(totest)
Base.emptymeta!()
for te in totest
    if !isa(te,MyType6498)
        @test_throws KeyError getmeta(te)
    else
        # user-defined metadata is not cleared with Base.emptymeta!()
        getmeta(te) 
    end
end
fillmeta(totest)
for te in totest
    Base.deletemeta!(te)
    @test_throws KeyError getmeta(te)
end

# test metadata content
fillmeta(totest)
#@test isequal(getmeta(myt), myt._metadata)
for (i,te) in enumerate(totest)
    md = getmeta(te)
    @test md[i] == -i
end

for (i,te) in enumerate(totest)
    md = MetaData()
    md[i] = -2i
    setmeta!(te, md)
    @test getmeta(te)[i] == -2i
end

for (i,te) in enumerate(totest)
    if isimmutable(te) 
        cop = nothing
        # for an ObjectIdDict this passes:
        try
            cop = copy(te) # this fails on some immutable: Methods, etc
        end
        if cop!=nothing
            getmeta(cop)
        end
    end
end

for (i,te) in enumerate(totest)
    cop = nothing
    try
        cop = Base.copywithmeta(te)
    end
    if cop!=nothing
        @test isequal(getmeta(te), getmeta(cop))
    end
end


# Dictionary type: ObjectIdDict vs Dict
md = MetaData()
md[1] = 2
str = "asdf"
numb = 5
setmeta!(str, md)
setmeta!(numb, md)
@test isequal(getmeta(numb),md)
@test isequal(getmeta(str),md)
# this works
@test isequal(getmeta(5),md)

# These tests only pass for isa(META, (Weak)ObjectIdDict):
@test_throws KeyError getmeta("asdf")
di = {"a"=>5}
setmeta!(di, md)
di["b"] = 5
@test hasmeta(di)

# Failing tests / oddities
##

## macro issues:
## Macros cannot be passed around, they are evaluated at parse
## time.  Thus uncommenting the following leads to an error:
# setmeta!(@which, md)
