getdoc = Base.Help.getdoc
setdoc! = Base.Help.setdoc!
hasdoc = Base.Help.hasdoc

## Old Help:
# Tests that old-help from helpdb.jl is correct.  These tests are
# quite fragile/temperamental, I'm not sure why.  Running in parallel
# may be one of the problems?

# initialise old-help
Base.Help.init_help()
helpdb = evalfile(Base.Help.helpdb_filename())

# For these the help is broken, sometimes.
broken_help = {"eval", "MS_SYNC", "CPU_CORES", "BoundingBox", "isinside", "ENDIAN_BOM", "C_NULL",
               "im"  ,"Inf16"  ,"NaN16"  ,"NaN"  ,"NaN32"  ,"ccall"   ,"Inf32"  ,"Inf", "cglobal"}
# - eval does not work with help.jl machinery
# - MS_SYNC, CPU_CORES are both Int64
# - "BoundingBox", "isinside" don't exist anymore
# - ENDIAN_BOM

mod_,obj,desc = 1,2,3
missingdoc = 0
for hd in helpdb
    mod_,obj,desc = hd
    if obj in broken_help
        continue
    end
    str = ""
    if obj[1]=='@' # a macro
        obj = obj # keep as string
        str = obj
    else
        try
            obj = eval(parse(obj))
            str = string(obj)
        catch
            try
                obj = eval(parse(mod_ * "." * obj))
                str = mod_ * "." * string(obj)
            catch
                obj = obj # keep as string
            end
        end
    end
    # @show obj, typeof(obj), desc
    # @test hasdoc(obj)
    # @test contains(getdoc(obj).desc,desc)
    if hasdoc(obj)
        @test contains(getdoc(obj).desc,desc)
    else
        missingdoc += 1
    end
end
if missingdoc>0
    println("Docs missing from $missingdoc objects")
end


## New-style doc

## test low-level functions getdoc, getdoc!, setdoc!, hasdoc
a = [1,2]
@test !hasdoc(a)
@test_throws ErrorException getdoc(a)
@test getdoc(a,nothing)==nothing
he = Base.Help.HelpEntry()
setdoc!(a, he)
@test hasdoc(a)
@test isequal(getdoc(a),he)
@test isequal(getdoc(a,nothing), he)
# strings are special
st1 = randstring(5)
st2 = randstring(5)
setdoc!(st1, he)
setdoc!(st2, he, string_into_meta=true)
for st in [st1,st2]
    @test hasdoc(st)
    @test isequal(getdoc(a),he)
    @test isequal(getdoc(a,nothing), he)
end
@test !hasmeta(st1)
@test hasmeta(st2)

## test doc-function
b = Dict()
st = randstring(5); st2 = randstring(5)
doc(b, st)
@test getdoc(b).desc==st
@test getdoc(b).mod==nothing
@test hasmeta(b)
doc(b, st; mod=Base)
@test getdoc(b).desc==st
@test getdoc(b).mod==Base
@test hasmeta(b)

## test @doc-macro
st = randstring(5)
@doc st type MyType9798 end
@test getdoc(MyType9798).desc==st
st = randstring(5)
@doc st abstract MyA
@test getdoc(MyA).desc==st
st = randstring(5) 
@doc st ffff(x) = 5x
@test getdoc(ffff).desc==st
st = randstring(5)
@doc st function gggg(x) 
    5x
end
@test getdoc(gggg).desc==st
st = randstring(5)
@doc st macro  
    MyM(ex) 
end
@test getdoc("@MyM").desc==st

# This equivalent one-liner gives an error for me:
# @doc st macro MyM(ex) end
# @test getdoc("@MyM")[:desc]==st

# module definitions inside macros don't work well so instead it
# throws an error.  However because this is at load time we cannot
# test for it:
# st = randstring(5)
# @test_throws @doc st module MyMod end


