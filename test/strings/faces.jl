# This file is a part of Julia. License is MIT: https://julialang.org/license

@testset "SimpleColor" begin
    @test Base.SimpleColor(:hey).value == :hey # no error
    @test Base.SimpleColor(0x01, 0x02, 0x03).value == (r=0x01, g=0x02, b=0x03)
    @test Base.SimpleColor((r=0x01, g=0x02, b=0x03)).value == (r=0x01, g=0x02, b=0x03)
    @test Base.SimpleColor(0x010203).value == (r=0x01, g=0x02, b=0x03)
    @test tryparse(Base.SimpleColor, "hey") == Base.SimpleColor(:hey)
    @test tryparse(Base.SimpleColor, "#010203") == Base.SimpleColor(0x010203)
    @test tryparse(Base.SimpleColor, "#12345g") === nothing
    @test tryparse(Base.SimpleColor, "!not a color") === nothing
    @test_throws ArgumentError parse(Base.SimpleColor, "!not a color")
end

@testset "Faces" begin
    # Construction
    @test Base.Face() ==
        Base.Face(nothing, nothing, nothing, nothing, nothing,
             nothing, nothing, nothing, nothing, Symbol[])
    @test Base.Face(font="font") ==
        Base.Face("font", nothing, nothing, nothing, nothing,
             nothing, nothing, nothing, nothing, Symbol[])
    @test Base.Face(height=1) ==
        Base.Face(nothing, 1, nothing, nothing, nothing,
             nothing, nothing, nothing, nothing, Symbol[])
    @test Base.Face(weight=:bold) ==
        Base.Face(nothing, nothing, :bold, nothing, nothing,
             nothing, nothing, nothing, nothing, Symbol[])
    @test Base.Face(slant=:italic) ==
        Base.Face(nothing, nothing, nothing, :italic, nothing,
             nothing, nothing, nothing, nothing, Symbol[])
    @test Base.Face(foreground=Base.SimpleColor(:red)) ==
        Base.Face(nothing, nothing, nothing, nothing, Base.SimpleColor(:red),
             nothing, nothing, nothing, nothing, Symbol[])
    @test Base.Face(foreground=:red) ==
        Base.Face(nothing, nothing, nothing, nothing, Base.SimpleColor(:red),
             nothing, nothing, nothing, nothing, Symbol[])
    @test Base.Face(foreground=0xff0000) ==
        Base.Face(nothing, nothing, nothing, nothing, Base.SimpleColor(0xff0000),
             nothing, nothing, nothing, nothing, Symbol[])
    @test Base.Face(background=Base.SimpleColor(:red)) ==
        Base.Face(nothing, nothing, nothing, nothing, nothing,
             Base.SimpleColor(:red), nothing, nothing, nothing, Symbol[])
    @test Base.Face(background=:red) ==
        Base.Face(nothing, nothing, nothing, nothing, nothing,
             Base.SimpleColor(:red), nothing, nothing, nothing, Symbol[])
    @test Base.Face(background=0xff0000) ==
        Base.Face(nothing, nothing, nothing, nothing, nothing,
             Base.SimpleColor(0xff0000), nothing, nothing, nothing, Symbol[])
    @test Base.Face(underline=true) ==
        Base.Face(nothing, nothing, nothing, nothing, nothing,
             nothing, true, nothing, nothing, Symbol[])
    @test Base.Face(underline=:red) ==
        Base.Face(nothing, nothing, nothing, nothing, nothing,
             nothing, Base.SimpleColor(:red), nothing, nothing, Symbol[])
    @test Base.Face(underline=(nothing, :curly)) ==
        Base.Face(nothing, nothing, nothing, nothing, nothing,
             nothing, (nothing, :curly), nothing, nothing, Symbol[])
    @test Base.Face(underline=(:red, :curly)) ==
        Base.Face(nothing, nothing, nothing, nothing, nothing,
             nothing, (Base.SimpleColor(:red), :curly), nothing, nothing, Symbol[])
    @test Base.Face(strikethrough=true) ==
        Base.Face(nothing, nothing, nothing, nothing, nothing,
             nothing, nothing, true, nothing, Symbol[])
    @test Base.Face(inverse=true) ==
        Base.Face(nothing, nothing, nothing, nothing, nothing,
             nothing, nothing, nothing, true, Symbol[])
    @test Base.Face(inherit=:singleface) ==
        Base.Face(nothing, nothing, nothing, nothing, nothing,
             nothing, nothing, nothing, nothing, [:singleface])
    @test Base.Face(inherit=[:many, :faces]) ==
        Base.Face(nothing, nothing, nothing, nothing, nothing,
             nothing, nothing, nothing, nothing, [:many, :faces])
    @test Base.Face() == Base.Face()
    @test Base.Face(height=1) == Base.Face(height=1)
    @test Base.Face(height=1) != Base.Face(height=2)
    @test Base.Face(inherit=:a) != Base.Face(inherit=:b)
    # Adding a face then resetting
    @test Base.loadfaces!(:testface => Base.Face(font="test")) == Base.Face(font="test")
    @test get(Base.FACES.current[], :testface, nothing) == Base.Face(font="test")
    @test Base.loadfaces!(:bold => Base.Face(weight=:extrabold)) == Base.Face(weight=:extrabold)
    @test get(Base.FACES.current[], :bold, nothing) == Base.Face(weight=:extrabold)
    @test Base.loadfaces!(:testface => Base.Face(height=2.0)) == Base.Face(font="test", height=2.0)
    @test get(Base.FACES.current[], :testface, nothing) == Base.Face(font="test", height=2.0)
    # Loading from TOML (a Dict)
    @test Base.loadfaces!(Dict{String, Any}("anotherface" =>
        Dict{String, Any}("font" => "afont",
                          "height" => 123,
                          "weight" => "semibold",
                          "slant" => "oblique",
                          "foreground" => "green",
                          "background" => "magenta",
                          "underline" => ["blue", "curly"],
                          "strikethrough" => true,
                          "inverse" => true,
                          "inherit" => ["iface"]))) isa Any
    @test get(Base.FACES.current[], :anotherface, nothing) ==
        Base.Face(font = "afont", height = 123, weight = :semibold,
             slant = :oblique, foreground = :green, background = :magenta,
             underline = (:blue, :curly), strikethrough = true,
             inverse = true, inherit = [:iface])
    Base.resetfaces!()
    @test get(Base.FACES.current[], :bold, nothing) == Base.Face(weight=:bold)
    @test haskey(Base.FACES.current[], :testface) == false
    @test haskey(Base.FACES.current[], :anotherface) == false
    # `withfaces`
    @test Base.withfaces(() -> get(Base.FACES.current[], :testface, nothing),
                    :testface => Base.Face(font="test")) == Base.Face(font="test")
    @test haskey(Base.FACES.current[], :testface) == false
    # Basic merging
    let f1 = Base.Face(height=140, weight=:bold, inherit=[:a])
        f2 = Base.Face(height=1.5, weight=:light, inherit=[:b])
        f3 = Base.Face(height=1.2, slant=:italic)
        @test merge(f1, f2, f3) == Base.Face(height=252, weight=:light, slant=:italic)
        @test merge(f3, f2, f1) == Base.Face(height=140, weight=:bold, slant=:italic, inherit=[:a])
        @test merge(f3, f1) == Base.Face(height=140, weight=:bold, slant=:italic, inherit=[:a])
        @test merge(f3, f2) == Base.Face(height=1.5*1.2, weight=:light, slant=:italic, inherit=[:b])
    end
    # Merging, inheritence, and canonicalisation
    let aface = Base.Face(font="a", height=1.2)
        bface = Base.Face(font="b", height=1.1, weight=:light, inherit=:a)
        cface = Base.Face(font="c", foreground=:red, inherit=:b)
        dface = Base.Face(font="d", foreground=:blue, weight=:bold)
        eface = Base.Face(font="e", inherit = [:c, :d])
        fface = Base.Face(font="f", inherit = [:d, :c])
        Base.loadfaces!(:a => aface)
        Base.loadfaces!(:b => bface)
        Base.loadfaces!(:c => cface)
        Base.loadfaces!(:d => dface)
        Base.loadfaces!(:e => eface)
        Base.loadfaces!(:f => fface)
        @test Base.getface(:c) == merge(Base.FACES.current[][:default], aface, bface, cface, Base.Face())
        @test Base.getface(:b) == merge(Base.FACES.current[][:default], aface, bface, Base.Face())
        @test Base.getface(:a) == merge(Base.FACES.current[][:default], aface, Base.Face())
        @test Base.getface([:c]) == Base.getface(:c)
        @test Base.getface(bface) == Base.getface(:b)
        @test Base.getface(cface) == Base.getface(:c)
        @test Base.getface([:c, :d]).foreground.value == :red
        @test Base.getface(:e).foreground.value == :red
        @test Base.getface([:d, :c]).foreground.value == :blue
        @test Base.getface(:f).foreground.value == :blue
        Base.resetfaces!()
    end
end

@testset "Styled string macro" begin
    # Preservation of an unstyled string
    @test S"some string" == StyledString("some string")
    # Basic styled constructs
    @test S"{thing=val:some} string" == StyledString("some string", [(1:4, :thing => "val")])
    @test S"some {thing=val:string}" == StyledString("some string", [(6:11, :thing => "val")])
    @test S"some {a=1:s}trin{b=2:g}" == StyledString("some string", [(6:6, :a => "1"), (11:11, :b => "2")])
    @test S"{thing=val with spaces:some} string" == StyledString("some string", [(1:4, :thing => "val with spaces")])
    @test S"{aface:some} string" == StyledString("some string", [(1:4, :face => :aface)])
    @test S"{aface,bface:some} string" ==
        StyledString("some string", [(1:4, :face => :aface), (1:4, :face => :bface)])
    # Inline face attributes
    @test S"{(slant=italic):some} string" ==
        StyledString("some string", [(1:4, :face => Base.Face(slant=:italic))])
    @test S"{(foreground=magenta,background=#555555):some} string" ==
        StyledString("some string", [(1:4, :face => Base.Face(foreground=:magenta, background=0x555555))])
    # Curly bracket escaping
    @test S"some \{string" == StyledString("some {string")
    @test S"some string\}" == StyledString("some string}")
    @test S"some \{string\}" == StyledString("some {string}")
    @test S"some \{str:ing\}" == StyledString("some {str:ing}")
    @test S"some \{{bold:string}\}" == StyledString("some {string}", [(7:12, :face => :bold)])
    @test S"some {bold:string \{other\}}" == StyledString("some string {other}", [(6:19, :face => :bold)])
    # Nesting
    @test S"{bold:nest{italic:ed st{red:yling}}}" ==
        StyledString("nested styling", [(1:14, :face => :bold), (5:14, :face => :italic), (10:14, :face => :red)])
    # Production of a `StyledString` value instead of an expression when possible
    @test StyledString("val") == @macroexpand S"val"
    @test StyledString("val", :face => :style) == @macroexpand S"{style:val}"
    # Interpolation
    @test :($styledstring(val)) == @macroexpand S"$val"
    @test :($styledstring("a", val)) == @macroexpand S"a$val"
    @test :($styledstring("a", val, "b")) == @macroexpand S"a$(val)b"
    @test :($styledstring($StyledString(string(val), $(Pair{Symbol, Any}(:face, :style))))) ==
        @macroexpand S"{style:$val}"
    @test :($styledstring($StyledString("val", [($(1:3), Pair{Symbol, Any}(:face, face))]))) ==
        @macroexpand S"{$face:val}"
    @test :($styledstring($StyledString("val", [($(1:3), Pair{Symbol, Any}(key, "val"))]))) ==
        @macroexpand S"{$key=val:val}"
    @test :($styledstring($StyledString("val", [($(1:3), Pair{Symbol, Any}(key, val))]))) ==
        @macroexpand S"{$key=$val:val}"
    @test :($styledstring($StyledString(string(val), Pair{Symbol, Any}(key, val)))) ==
        @macroexpand S"{$key=$val:$val}"
    @test :($styledstring($StyledString("val", [($(1:3), Pair{Symbol, Any}(:face, $(Base.Face)(foreground = color)))]))) ==
        @macroexpand S"{(foreground=$color):val}"
end
