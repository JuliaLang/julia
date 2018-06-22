# This file is a part of Julia. License is MIT: https://julialang.org/license

let buf = IOBuffer()
    printstyled(IOContext(buf, :color=>true), "foo", color=:red)
    @test startswith(String(take!(buf)), Base.text_colors[:red])
end

# Test that `printstyled` accepts non-string values, just as `print` does
let buf_color = IOBuffer()
    args = (3.2, "foo", :testsym)
    printstyled(IOContext(buf_color, :color=>true), args..., color=:red)
    buf_plain = IOBuffer()
    print(buf_plain, args...)
    expected_str = string(Base.text_colors[:red],
                          String(take!(buf_plain)),
                          Base.text_colors[:default])
    @test expected_str == String(take!(buf_color))
end

# Test that `printstyled` on multiline input prints the ANSI codes
# on each line
let buf_color = IOBuffer()
    str = "Two\nlines"
    printstyled(IOContext(buf_color, :color=>true), str; bold=true, color=:red)
    @test String(take!(buf_color)) == "\e[31m\e[1mTwo\e[22m\e[39m\n\e[31m\e[1mlines\e[22m\e[39m"
end

let
    @test haskey(stdout, :color) == false
    @test haskey(stdout, :bar) == false
    @test (:color=>true) ∉ stdout
    @test (:color=>false) ∉ stdout
    @test get(stdout, :color, nothing) === nothing
    @test get(stdout, :bar, nothing) === nothing
    @test_throws KeyError stdout[:color]
    @test_throws KeyError stdout[:bar]
end

let
    global c_18711 = 0
    buf = IOContext(IOBuffer(), :hascontext => true)
    Base.with_output_color(:red, buf) do buf
        global c_18711
        get(buf, :hascontext, false) && (c_18711 += 1)
    end
    @test c_18711 == 1
end

let buf = IOBuffer()
    buf_color = IOContext(buf, :color => true)
    printstyled(buf_color, "foo", color=:red)
    # Check that we get back to normal text color in the end
    @test String(take!(buf)) == "\e[31mfoo\e[39m"

    # Check that boldness is turned off
    printstyled(buf_color, "foo"; bold=true, color=:red)
    @test String(take!(buf)) == "\e[31m\e[1mfoo\e[22m\e[39m"
end

let buf = IOBuffer()
    Base.with_output_color(:red, IOContext(buf, :color => true)) do io; print(io, "red",
        Base.text_colors[:underline], " underline")
    Base.with_output_color(:blue, io) do io; print(io, " blue")
    Base.with_output_color(:bold, io) do io; print(io, " bold")
    Base.with_output_color(:green, io) do io; print(io, " green\ngreen")
    end; print(io, " bold",
        Base.text_colors[:underline], " underline")
    end; print(io, " blue")
    end; print(io, " red")
    end
    @test String(take!(buf)) == "\e[31mred\e[4m underline\e[34m blue\e[1m bold\e[32m green\e[22m\e[39m\n\e[32m\e[1mgreen\e[34m bold\e[4m underline\e[22m blue\e[31m red\e[39m"
end

let io = convert(IOContext, Base.IOFormatBuffer())
    printstyled(io, "red", color = :red)
    print(io, ", ")
    printstyled(io, "blue", color = :blue)
    buf = IOBuffer()
    write(IOContext(buf, :color => true), io.io)
    @test String(take!(buf)) == "\e[31mred\e[39m, \e[34mblue\e[39m"
end

