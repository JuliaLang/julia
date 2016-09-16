# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

if Sys.ARCH === :x86_64 || Sys.Arch == :i386
  let
    function linear_foo()
         x = 4
         y = 5
    end


    rgx = r"%"
    buf = IOBuffer()
    output=""

    code_native(buf,linear_foo,(),"att")
    output=takebuf_string(buf)

    @test ismatch(rgx,output)

    code_native(buf,linear_foo,(),"intel")
    output=takebuf_string(buf)

    @test !(ismatch(rgx,output))

    code_native(buf,linear_foo,())
    output=takebuf_string(buf)

    @test ismatch(rgx, output)
  end
end
