@testset "Cycler" begin
    cycl = Cycler([:a, :b, :c])
    @test length(cycl) == 3
    @test !isempty(cycl)
    @test cycl[1] == :a
    @test cycl[5] == :b
    @test cycl[3:7] == [:c, :a, :b, :c, :a]
    @test cycl[0] == :c
    @test cycl[-1] == :b
    @test cycl[-2:1] == [:a, :b, :c, :a]
end

ps1 = M.Style(layout=M.Layout(font_size=10))
ps2 = M.Style(layout=M.Layout(font_family="Helvetica"))

@testset "Style(::Style, ::Style)" begin
    ps3 = M.Style(ps1, ps2)
    @test ps3.layout[:font] == Dict(:family=>"Helvetica", :size=>10)
    @test isempty(ps3.global_trace)
    @test isempty(ps3.trace)

    gg = M.ggplot_style()
    new_font = M.Style(layout=M.Layout(font_family="Source Code Pro"))
    ps4 = M.Style(gg, new_font)
    ps5 = M.Style(new_font, gg)

    # make sure things were set properly
    for (k, v) in gg.layout
        if k == :font
            want = gg.layout[:font]
            want[:family] = "Source Code Pro"
            @test ps4.layout[k] == ps5.layout[k] == want
        else
            @test ps4.layout[k] == ps5.layout[k] == gg.layout[k]
        end
    end

end

@testset "setting plot attributes" begin

    goofy = Style(
        global_trace=attr(
            marker_color="red", line_dash=Cycler(["dash", "dot"]),
        ),
        trace=Dict(:scatter => attr(mode="markers")),
    )

    p1 = Plot(scatter(y=1:3, mode="lines", marker_symbol="square"), style=goofy)
    p2 = Plot(scatter(y=1:3, marker_color="green", line_dash="dot"), style=goofy)

    # now call JSON.lower to enforce style
    PlotlyBase.JSON.lower(p1)
    PlotlyBase.JSON.lower(p2)

    @test p1.data[1]["marker_color"] == "red"
    @test p2.data[1]["marker_color"] == "green"

    @test p1.data[1]["line_dash"] == "dash"
    @test p2.data[1]["line_dash"] == "dot"

    p3 = Plot(rand(5, 6), style=goofy)
    restyle!(p3, 4, line_dash="something_else")
    PlotlyBase.JSON.lower(p3)
    @test p3.data[1]["line_dash"] == "dash"
    @test p3.data[2]["line_dash"] == "dot"
    @test p3.data[3]["line_dash"] == "dash"
    @test p3.data[4]["line_dash"] == "something_else"
    @test p3.data[5]["line_dash"] == "dot"
    @test p3.data[6]["line_dash"] == "dash"
end
