# This file is a part of Julia. License is MIT: http://julialang.org/license

bt = backtrace()
have_backtrace = false
for l in bt
    lkup = ccall(:jl_lookup_code_address, Any, (Ptr{Void},), l)
    if lkup[1] == :backtrace
        @test lkup[7] == false # fromC
        have_backtrace = true
        break
    end
end

@test have_backtrace

# Test location information for inlined code (ref issues #1334 #12544)
module test_inline_bt
using Base.Test

function get_bt_frame(functionname, bt)
    for i = 1:length(bt)
        lkup = ccall(:jl_lookup_code_address, Any, (Ptr{Void}, Cint), bt[i]-1, true)
        # find the function frame
        lkup[1] == functionname && (return lkup)
    end
end

# same-file inline
eval(Expr(:function, Expr(:call, :test_inline_1),
                     Expr(:block, LineNumberNode(42),
                     LineNumberNode(37),
                     Expr(:call, :throw, "foo"))))

# different-file inline
const absfilepath = OS_NAME == :Windows ? "C:\\foo\\bar\\baz.jl" : "/foo/bar/baz.jl"
eval(Expr(:function, Expr(:call, :test_inline_2),
                     Expr(:block, LineNumberNode(99),
                     LineNumberNode(666),
                     LineNumberNode(111),
                     Expr(:call, :throw, "foo"))))

try
    eval(:(test_inline_1()))
    error("unexpected")
catch err
    lkup = get_bt_frame(:test_inline_1, catch_backtrace())
    @test lkup !== nothing || "Missing backtrace in inlining test"

    fname, file, line, inlinedfile, inlinedline, linfo, fromC = lkup
    @test endswith(string(inlinedfile), "backtrace.jl")
    @test inlinedline == 42
end
try
    eval(:(test_inline_2()))
    error("unexpected")
catch err
    lkup = get_bt_frame(:test_inline_2, catch_backtrace())
    @test lkup !== nothing || "Missing backtrace in inlining test"

    fname, file, line, inlinedfile, inlinedline, linfo, fromC = lkup
    @test string(file) == absfilepath
    @test line == 111
    @test endswith(string(inlinedfile), "backtrace.jl")
    @test inlinedline == 99
end

end # module

#issue 12977: line numbers for kwarg methods.
linenum = @__LINE__; f12977(; args...) = ()
loc = functionloc(f12977)
@test endswith(loc[1], "backtrace.jl")
@test loc[2] == linenum

# issue #922: SimplifyCFG pass merges throws
code_loc(p, skipC=true) = ccall(:jl_lookup_code_address, Any, (Ptr{Void},Cint), p-1, skipC)

@noinline function test_throw_commoning(x)
    if x==1; throw(AssertionError()); end
    if x==2; throw(AssertionError()); end
end

let
    local b1, b2
    try
        test_throw_commoning(1)
    catch
        b1 = catch_backtrace()
    end
    try
        test_throw_commoning(2)
    catch
        b2 = catch_backtrace()
    end
    ind1 = find(:test_throw_commoning .== map(b->code_loc(b)[1], b1))
    ind2 = find(:test_throw_commoning .== map(b->code_loc(b)[1], b2))
    @test !isempty(ind1)
    @test !isempty(ind2)
    @test (code_loc(b1[ind1[1]])[3]::Int == code_loc(b2[ind2[1]])[3]::Int) != # source line, for example: essentials.jl:58
          (code_loc(b1[ind1[1]])[5]::Int == code_loc(b2[ind2[1]])[5]::Int)    # inlined line, for example: backtrace.jl:82
end
