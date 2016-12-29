# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

ix86 = r"i[356]86"

if Sys.ARCH === :x86_64 || ismatch(ix86, string(Sys.ARCH))
    function linear_foo()
        x = 4
        y = 5
    end

    rgx = r"%"
    buf = IOBuffer()
    output=""
    #test that the string output is at&t syntax by checking for occurrences of '%'s
    code_native(buf,linear_foo,(),:att)
    output=String(take!(buf))

    @test ismatch(rgx,output)

    #test that the code output is intel syntax by checking it has no occurrences of '%'
    code_native(buf,linear_foo,(),:intel)
    output=String(take!(buf))

    @test !(ismatch(rgx,output))

    code_native(buf,linear_foo,())
    output=String(take!(buf))

    @test ismatch(rgx, output)
end
