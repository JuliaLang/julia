
gt = M.GenericTrace("scatter"; x=1:10, y=sin.(1:10))

@testset "test constructors" begin
    @test sort(collect(keys(gt.fields))) == [:type, :x, :y]
end

@testset "_UNDERSCORE_ATTRS" begin
    l = M.Layout()

    # test setindex!
    l[:paper_bgcolor] = "grey"
    @test haskey(l.fields, :paper_bgcolor)
    @test l.fields[:paper_bgcolor] == "grey"

    # test getindex
    @test l[:paper_bgcolor] == "grey"
    @test l["paper_bgcolor"] == "grey"

    # now do it again with the string form of setindex!
    l = M.Layout()

    # test setindex!
    l["paper_bgcolor"] = "grey"
    @test haskey(l.fields, :paper_bgcolor)
    @test l.fields[:paper_bgcolor] == "grey"

    # test getindex
    @test l[:paper_bgcolor] == "grey"
    @test l["paper_bgcolor"] == "grey"
end

@testset "test setindex!, getindex methods" begin
    gt[:visible] = true
    @test length(gt.fields) == 4
    @test haskey(gt.fields, :visible)
    @test gt.fields[:visible] == true

    # now try with string. Make sure it updates inplace
    gt["visible"] = false
    @test length(gt.fields) == 4
    @test haskey(gt.fields, :visible)
    @test gt.fields[:visible] == false

    # -------- #
    # 2 levels #
    # -------- #
    gt[:line, :color] = "red"
    @test length(gt.fields) == 5
    @test haskey(gt.fields, :line)
    @test isa(gt.fields[:line], Dict)
    @test gt.fields[:line][:color] == "red"
    @test gt["line.color"] == "red"

    # now try string version
    gt["line", "color"] = "blue"
    @test length(gt.fields) == 5
    @test haskey(gt.fields, :line)
    @test isa(gt.fields[:line], Dict)
    @test gt.fields[:line][:color] == "blue"
    @test gt["line_color"] == "blue"

    # now try convenience string dot notation
    gt["line.color"] = "green"
    @test length(gt.fields) == 5
    @test haskey(gt.fields, :line)
    @test isa(gt.fields[:line], Dict)
    @test gt.fields[:line][:color] == "green"
    @test gt[:line_color] == "green"

    # now try symbol with underscore
    gt[:(line_color)] = "orange"
    @test length(gt.fields) == 5
    @test haskey(gt.fields, :line)
    @test isa(gt.fields[:line], Dict)
    @test gt.fields[:line][:color] == "orange"
    @test gt["line.color"] == "orange"

    # now try string with underscore
    gt["line_color"] = "magenta"
    @test length(gt.fields) == 5
    @test haskey(gt.fields, :line)
    @test isa(gt.fields[:line], Dict)
    @test gt.fields[:line][:color] == "magenta"
    @test gt["line.color"] == "magenta"

    # -------- #
    # 3 levels #
    # -------- #
    gt[:marker, :line, :color] = "red"
    @test length(gt.fields) == 6
    @test haskey(gt.fields, :marker)
    @test isa(gt.fields[:marker], Dict)
    @test haskey(gt.fields[:marker], :line)
    @test isa(gt.fields[:marker][:line], Dict)
    @test haskey(gt.fields[:marker][:line], :color)
    @test gt.fields[:marker][:line][:color] == "red"
    @test gt["marker.line.color"] == "red"

    # now try string version
    gt["marker", "line", "color"] = "blue"
    @test length(gt.fields) == 6
    @test haskey(gt.fields, :marker)
    @test isa(gt.fields[:marker], Dict)
    @test haskey(gt.fields[:marker], :line)
    @test isa(gt.fields[:marker][:line], Dict)
    @test haskey(gt.fields[:marker][:line], :color)
    @test gt.fields[:marker][:line][:color] == "blue"
    @test gt["marker.line.color"] == "blue"

    # now try convenience string dot notation
    gt["marker.line.color"] = "green"
    @test length(gt.fields) == 6
    @test haskey(gt.fields, :marker)
    @test isa(gt.fields[:marker], Dict)
    @test haskey(gt.fields[:marker], :line)
    @test isa(gt.fields[:marker][:line], Dict)
    @test haskey(gt.fields[:marker][:line], :color)
    @test gt.fields[:marker][:line][:color] == "green"
    @test gt["marker.line.color"] == "green"

    # now string with underscore notation
    gt["marker_line_color"] = "orange"
    @test length(gt.fields) == 6
    @test haskey(gt.fields, :marker)
    @test isa(gt.fields[:marker], Dict)
    @test haskey(gt.fields[:marker], :line)
    @test isa(gt.fields[:marker][:line], Dict)
    @test haskey(gt.fields[:marker][:line], :color)
    @test gt.fields[:marker][:line][:color] == "orange"
    @test gt["marker.line.color"] == "orange"

    # now symbol with underscore notation
    gt[:(marker_line_color)] = "magenta"
    @test length(gt.fields) == 6
    @test haskey(gt.fields, :marker)
    @test isa(gt.fields[:marker], Dict)
    @test haskey(gt.fields[:marker], :line)
    @test isa(gt.fields[:marker][:line], Dict)
    @test haskey(gt.fields[:marker][:line], :color)
    @test gt.fields[:marker][:line][:color] == "magenta"
    @test gt["marker.line.color"] == "magenta"

    # -------- #
    # 4 levels #
    # -------- #
    gt[:marker, :colorbar, :tickfont, :family] = "Hasklig-ExtraLight"
    @test length(gt.fields) == 6  # notice we didn't add another top level key
    @test haskey(gt.fields, :marker)
    @test isa(gt.fields[:marker], Dict)
    @test length(gt.fields[:marker]) == 2  # but we did add a key at this level
    @test haskey(gt.fields[:marker], :colorbar)
    @test isa(gt.fields[:marker][:colorbar], Dict)
    @test haskey(gt.fields[:marker][:colorbar], :tickfont)
    @test isa(gt.fields[:marker][:colorbar][:tickfont], Dict)
    @test haskey(gt.fields[:marker][:colorbar][:tickfont], :family)
    @test gt.fields[:marker][:colorbar][:tickfont][:family] == "Hasklig-ExtraLight"
    @test gt["marker.colorbar.tickfont.family"] == "Hasklig-ExtraLight"

    # now try string version
    gt["marker", "colorbar", "tickfont", "family"] = "Hasklig-Light"
    @test length(gt.fields) == 6
    @test haskey(gt.fields, :marker)
    @test isa(gt.fields[:marker], Dict)
    @test length(gt.fields[:marker]) == 2
    @test haskey(gt.fields[:marker], :colorbar)
    @test isa(gt.fields[:marker][:colorbar], Dict)
    @test haskey(gt.fields[:marker][:colorbar], :tickfont)
    @test isa(gt.fields[:marker][:colorbar][:tickfont], Dict)
    @test haskey(gt.fields[:marker][:colorbar][:tickfont], :family)
    @test gt.fields[:marker][:colorbar][:tickfont][:family] == "Hasklig-Light"
    @test gt["marker.colorbar.tickfont.family"] == "Hasklig-Light"

    # now try convenience string dot notation
    gt["marker.colorbar.tickfont.family"] = "Hasklig-Medium"
    @test length(gt.fields) == 6  # notice we didn't add another top level key
    @test haskey(gt.fields, :marker)
    @test isa(gt.fields[:marker], Dict)
    @test length(gt.fields[:marker]) == 2  # but we did add a key at this level
    @test haskey(gt.fields[:marker], :colorbar)
    @test isa(gt.fields[:marker][:colorbar], Dict)
    @test haskey(gt.fields[:marker][:colorbar], :tickfont)
    @test isa(gt.fields[:marker][:colorbar][:tickfont], Dict)
    @test haskey(gt.fields[:marker][:colorbar][:tickfont], :family)
    @test gt.fields[:marker][:colorbar][:tickfont][:family] == "Hasklig-Medium"
    @test gt["marker.colorbar.tickfont.family"] == "Hasklig-Medium"

    # now string with underscore notation
    gt["marker_colorbar_tickfont_family"] = "Webdings"
    @test length(gt.fields) == 6  # notice we didn't add another top level key
    @test haskey(gt.fields, :marker)
    @test isa(gt.fields[:marker], Dict)
    @test length(gt.fields[:marker]) == 2  # but we did add a key at this level
    @test haskey(gt.fields[:marker], :colorbar)
    @test isa(gt.fields[:marker][:colorbar], Dict)
    @test haskey(gt.fields[:marker][:colorbar], :tickfont)
    @test isa(gt.fields[:marker][:colorbar][:tickfont], Dict)
    @test haskey(gt.fields[:marker][:colorbar][:tickfont], :family)
    @test gt.fields[:marker][:colorbar][:tickfont][:family] == "Webdings"
    @test gt["marker.colorbar.tickfont.family"] == "Webdings"

    # now symbol with underscore notation
    gt[:marker_colorbar_tickfont_family] = "Webdings42"
    @test length(gt.fields) == 6  # notice we didn't add another top level key
    @test haskey(gt.fields, :marker)
    @test isa(gt.fields[:marker], Dict)
    @test length(gt.fields[:marker]) == 2  # but we did add a key at this level
    @test haskey(gt.fields[:marker], :colorbar)
    @test isa(gt.fields[:marker][:colorbar], Dict)
    @test haskey(gt.fields[:marker][:colorbar], :tickfont)
    @test isa(gt.fields[:marker][:colorbar][:tickfont], Dict)
    @test haskey(gt.fields[:marker][:colorbar][:tickfont], :family)
    @test gt.fields[:marker][:colorbar][:tickfont][:family] == "Webdings42"
    @test gt["marker.colorbar.tickfont.family"] == "Webdings42"

    # error on 5 levels
    @test_throws MethodError gt["marker.colorbar.tickfont.family.foo"] = :bar
end

@testset "testing underscore constructor" begin
    # now test underscore constructor and see if it matches gt
    gt2 = M.scatter(;x=1:10, y=sin.(1:10),
                     marker_colorbar_tickfont_family="Webdings42",
                     marker_line_color="magenta",
                     line_color="magenta",
                     visible=false)
    @test sort(collect(keys(gt.fields))) == sort(collect(keys(gt2.fields)))
    for k in keys(gt.fields)
        @test gt[k] == gt2[k]
    end
end

@testset "testing vline hline" begin

    vl = vline(0)
    @test vl["x0"] == 0
    @test vl["y0"] == 0
    @test vl["y1"] == 1
    @test vl["type"] == "line"
    @test vl["xref"] == "x"
    @test vl["yref"] == "paper"

    hl = hline(0)
    @test hl["y0"] == 0
    @test hl["x0"] == 0
    @test hl["x1"] == 1
    @test hl["type"] == "line"
    @test hl["yref"] == "y"
    @test hl["xref"] == "paper"

    av, b1v, b2v = 1:3, 5:7, 9:11
    _want_i(x::Number, i) = x
    _want_i(x::AbstractVector, i) = x[i]
    for (a, b1, b2) in [(av, 1, 2),
                        (av, b1v, 2),
                        (av, b1v, b2v),
                        (av, 1, b2v),
                        (0, b1v, 2),
                        (0, b1v, b2v),
                        (0, 1, b2v)]

        vl = vline(a, b1, b2)
        for (i, v) in enumerate(vl)
            @test v["x0"] == _want_i(a, i)
            @test v["y0"] == _want_i(b1, i)
            @test v["y1"] == _want_i(b2, i)
        end

        hl = hline(a, b1, b2)
        for (i, h) in enumerate(hl)
            @test h["y0"] == _want_i(a, i)
            @test h["x0"] == _want_i(b1, i)
            @test h["x1"] == _want_i(b2, i)
        end
    end
end

@testset "test other shapes" begin
    # constructor logic was taken care of above. Just need to test
    # rect/path/circle specfic things here
    r = rect(1, 2, 3, 4)
    @test r["type"] == "rect"

    c = circle(1, 2, 3, 4)
    @test c["type"] == "circle"

    _path = "M 1 1 L 1 3 L 4 1 Z"
    p = path(_path)
    @test p["type"] == "path"
    @test p["path"] == _path
end

@testset "test setting nested attr" begin
    l = Layout()
    times20 = attr(name="Times", size=20)
    l[:xaxis_titlefont] = times20
    @test isa(l[:xaxis], Dict)
    @test l[:xaxis][:titlefont][:name] == "Times"
    @test l[:xaxis][:titlefont][:size] == 20
end
