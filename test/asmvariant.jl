# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

ix86 = r"i[356]86"

if Sys.ARCH === :x86_64 || contains(string(Sys.ARCH), ix86)
    function linear_foo()
        x = 4
        y = 5
    end

    rgx = r"%"
    buf = IOBuffer()
    output=""
    #test that the string output is at&t syntax by checking for occurrences of '%'s
    code_native(buf,linear_foo,(), syntax = :att)
    output=String(take!(buf))

    @test contains(output,rgx)

    #test that the code output is intel syntax by checking it has no occurrences of '%'
    code_native(buf,linear_foo,(), syntax = :intel)
    output=String(take!(buf))

    @test !contains(output,rgx)

    code_native(buf,linear_foo,())
    output=String(take!(buf))

    @test contains(output,rgx)
end
