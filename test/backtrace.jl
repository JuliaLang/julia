# This file is a part of Julia. License is MIT: http://julialang.org/license

bt = backtrace()
have_backtrace = false
for l in bt
    lkup = ccall(:jl_lookup_code_address, Any, (Ptr{Void}, Cint), l, true)
    if lkup[1][1] == :backtrace
        @test lkup[1][5] == false # fromC
        have_backtrace = true
        break
    end
end

@test have_backtrace

# Test location information for inlined code (ref issues #1334 #12544)
module test_inline_bt
using Base.Test

function get_bt_frames(functionname, bt)
    for i = 1:length(bt)
        lkup = Base.StackTraces.lookup(bt[i])
        lkup[end].func == functionname && (return lkup)
    end
end

# same-file inline
eval(Expr(:function, Expr(:call, :test_inline_1),
          Expr(:block, Expr(:line, 42, Symbol("backtrace.jl")),
                       Expr(:block, Expr(:meta, :push_loc, Symbol("backtrace.jl"), :inlfunc),
                                    Expr(:line, 37),
                                    Expr(:call, :throw, "foo"),
                                    Expr(:meta, :pop_loc)))))

try
    test_inline_1()
    error("unexpected")
catch err
    lkup = get_bt_frames(:test_inline_1, catch_backtrace())
    @test length(lkup) == 2
    @test endswith(string(lkup[2].file), "backtrace.jl")
    @test lkup[2].line == 42
    @test lkup[1].func == :inlfunc
    @test endswith(string(lkup[1].file), "backtrace.jl")
    @test lkup[1].line == 37
end

# different-file inline
const absfilepath = is_windows() ? "C:\\foo\\bar\\baz.jl" : "/foo/bar/baz.jl"
eval(Expr(:function, Expr(:call, :test_inline_2),
          Expr(:block, Expr(:line, 99, Symbol("backtrace.jl")),
                       Expr(:block, Expr(:meta, :push_loc, Symbol(absfilepath)),
                                    Expr(:line, 111),
                                    Expr(:call, :throw, "foo"),
                                    Expr(:meta, :pop_loc)))))

try
    test_inline_2()
    error("unexpected")
catch err
    lkup = get_bt_frames(:test_inline_2, catch_backtrace())
    @test length(lkup) == 2
    @test endswith(string(lkup[2].file), "backtrace.jl")
    @test lkup[2].line == 99
    @test string(lkup[1].file) == absfilepath
    @test lkup[1].line == 111
end

end # module

#issue 12977: line numbers for kwarg methods.
linenum = @__LINE__; f12977(; args...) = ()
loc = functionloc(f12977)
@test endswith(loc[1], "backtrace.jl")
@test loc[2] == linenum

@noinline function test_throw_commoning(x)
    if x==1; throw(AssertionError()); end
    if x==2; throw(AssertionError()); end
end

let
    local b1, b2
    try
        test_throw_commoning(1)
    catch
        b1 = stacktrace(catch_backtrace())
    end
    try
        test_throw_commoning(2)
    catch
        b2 = stacktrace(catch_backtrace())
    end
    i1 = findfirst(frame -> frame.func === :test_throw_commoning, b1)
    i2 = findfirst(frame -> frame.func === :test_throw_commoning, b2)
    @test i1 > 0 && i2 > 0
    @test b1[i1].line != b2[i2].line
end
