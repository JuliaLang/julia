# This file is a part of Julia. License is MIT: http://julialang.org/license

# note: when modifying this file, update TESTLINE below or the inline test will fail!

bt = backtrace()
have_backtrace = false
for l in bt
    lkup = ccall(:jl_lookup_code_address, Any, (Ptr{Void},), l)
    if lkup[1] == :backtrace
        @test lkup[6] == false # fromC
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
        lkup = ccall(:jl_lookup_code_address, Any, (Ptr{Void}, Cint), bt[i], true)
        # find the function frame
        lkup[1] == functionname && (return lkup)
    end
end

# same-file inline
eval(Expr(:function, Expr(:call, :test_inline_1),
                     Expr(:block, LineNumberNode(symbol("backtrace.jl"), 42),
                     LineNumberNode(symbol("backtrace.jl"), 37),
                     Expr(:call, :throw, "foo"))))

# different-file inline
eval(Expr(:function, Expr(:call, :test_inline_2),
                     Expr(:block, LineNumberNode(symbol("backtrace.jl"), 99),
                     LineNumberNode(symbol("/foo/bar/baz.jl"), 111),
                     Expr(:call, :throw, "foo"))))

try
    eval(:(test_inline_1()))
catch err
    lkup = get_bt_frame(:test_inline_1, catch_backtrace())
    if is(lkup, nothing)
        throw(Test.Failure("Missing backtrace in inlining test"))
    end

    fname, file, line, inlinedfile, inlinedline, fromC = lkup
    @test endswith(string(inlinedfile), "backtrace.jl")
    @test inlinedline == 42
end
try
    eval(:(test_inline_2()))
catch err
    lkup = get_bt_frame(:test_inline_2, catch_backtrace())
    if is(lkup, nothing)
        throw(Test.Failure("Missing backtrace in inlining test"))
    end

    fname, file, line, inlinedfile, inlinedline, fromC = lkup
    @test string(file) == "/foo/bar/baz.jl"
    @test line == 111
    @test endswith(string(inlinedfile), "backtrace.jl")
    @test inlinedline == 99
end

end # module
