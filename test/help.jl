getdoc = Base.Help.getdoc
setdoc! = Base.Help.setdoc!
hasdoc = Base.Help.hasdoc

function makeHelpDict() 
    hd = Base.Help.HelpDict()
    hd[:desc] = ""
    hd[:mod] = nothing
    return hd
end

## test low-level functions getdoc, setdoc!, hasdoc
a = [1,2]
@test !hasdoc(a)
@test_throws ErrorException getdoc(a)
@test getdoc(a,nothing)==nothing
hd = makeHelpDict()
setdoc!(a, hd)
@test hasdoc(a)
@test isequal(getdoc(a),hd)
@test isequal(getdoc(a,nothing), hd)
# strings are special
st1 = randstring(5)
st2 = randstring(5)
setdoc!(st1, hd)
setdoc!(st2, hd, string_into_meta=true)
for st in [st1,st2]
    @test hasdoc(st)
    @test isequal(getdoc(a),hd)
    @test isequal(getdoc(a,nothing), hd)
end
@test !hasmeta(st1)
@test hasmeta(st2)

## test doc-function
b = Dict()
st = randstring(5); st2 = randstring(5)
doc(b, st)
@test getdoc(b)[:desc]==st
@test getdoc(b)[:mod]==nothing
@test hasmeta(b)
doc(b, st; mod=Base)
@test getdoc(b)[:desc]==st
@test getdoc(b)[:mod]==Base
@test hasmeta(b)

## test @doc-macro
st = randstring(5)
@doc st type MyType9798 end
@test getdoc(MyType9798)[:desc]==st
st = randstring(5)
@doc st abstract MyA
@test getdoc(MyA)[:desc]==st
st = randstring(5) 
@doc st ffff(x) = 5x
@test getdoc(ffff)[:desc]==st
st = randstring(5)
@doc st function gggg(x) 
    5x
end
@test getdoc(gggg)[:desc]==st
st = randstring(5)
@doc st macro  
    MyM(ex) 
end
@test getdoc("@MyM")[:desc]==st

# This equivalent one-liner gives an error for me:
# @doc st macro MyM(ex) end
# @test getdoc("@MyM")[:desc]==st

# module definitions inside macros don't work well so instead it
# throws an error.  However because this is at load time we cannot
# test for it:
# st = randstring(5)
# @test_throws @doc st module MyMod end


## Old Help:
# Tests that old-help from helpdb.jl is correct.  These tests are
# quite fragile/temperamental, I'm not sure why.  Running in parallel
# may be one of the problems?

# initialise old-help
Base.Help.init_help()
helpdb = evalfile(Base.Help.helpdb_filename())

# For these the help is broken, sometimes.
broken_help = {eval, MS_SYNC, CPU_CORES, "BoundingBox", "isinside", ENDIAN_BOM, C_NULL}
# sometimes_brocken = {im, Inf16, Inf32, Inf}
# append!(broken_help, sometimes_brocken)
# - eval does not work with help.jl machinery
# - MS_SYNC, CPU_CORES are both Int64
# - "BoundingBox", "isinside" don't exist anymore
# - ENDIAN_BOM

mod_,obj,desc = 1,2,3
for hd in helpdb
    @show hd
    mod_,obj,desc = hd
    if obj=="im" || obj=="Inf16" || obj==="NaN16" || obj==="NaN" || obj==="NaN32" || obj=="ccall"
        continue
        # otherwise I get this error?!
# LLVM ERROR: Cannot select: 0x2f42fa0: i64 = bitcast 0x9425f40 [ORD=88214] [ID=9] dbg:operators.jl:9:1
#   0x9425f40: i8 = truncate 0x2f4d780 [ORD=88214] [ID=8]
#     0x2f4d780: i32,ch = CopyFromReg 0x15779e8, 0x9321120 [ORD=88214] [ID=7]
#       0x9321120: i32 = Register %vreg1 [ORD=88214] [ID=2]
# In function: julia_==;19624
    end

    os = obj
    @show obj, typeof(obj), desc
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
    @show obj, typeof(obj), desc
    if !(obj in broken_help)
#        @show obj, desc
       @test hasdoc(obj)
       @test contains(getdoc(obj)[:desc],desc)
    end
end

