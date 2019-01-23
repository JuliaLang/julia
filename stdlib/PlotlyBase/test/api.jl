function fresh_data()
    t1 = scatter(;y=[1, 2, 3])
    t2 = scatter(;y=[10, 20, 30])
    t3 = scatter(;y=[100, 200, 300])
    l = Layout(;title="Foo")
    p = Plot([copy(t1), copy(t2), copy(t3)], copy(l))
    t1, t2, t3, l, p
end

@testset "Test api methods on Plot" begin

    @testset "test helpers" begin
        @test PlotlyBase._prep_restyle_vec_setindex([1, 2], 2) == [1, 2]
        @test PlotlyBase._prep_restyle_vec_setindex([1, 2], 3) == [1, 2, 1]
        @test PlotlyBase._prep_restyle_vec_setindex([1, 2], 4) == [1, 2, 1, 2]

        @test PlotlyBase._prep_restyle_vec_setindex((1, [42, 4]), 2) == Any[1, [42, 4]]
        @test PlotlyBase._prep_restyle_vec_setindex((1, [42, 4]), 3) == Any[1, [42, 4], 1]
        @test PlotlyBase._prep_restyle_vec_setindex((1, [42, 4]), 4) == Any[1, [42, 4], 1, [42, 4]]
    end

    @testset "test _update_fields" begin
        t1, t2, t3, l, p = fresh_data()
        # test dict version
        o = copy(t1)
        PlotlyBase._update_fields(o, 1, Dict{Symbol,Any}(:foo=>"Bar"))
        @test o["foo"] == "Bar"
        # kwarg version
        PlotlyBase._update_fields(o, 1; foo="Foo")
        @test o["foo"] == "Foo"

        # dict + kwarg version. Make sure dict gets through w/out replacing _
        PlotlyBase._update_fields(o, 1, Dict{Symbol,Any}(:fuzzy_wuzzy=>"Bear");
                                fuzzy_wuzzy="?")
        @test o.fields[:fuzzy_wuzzy] == "Bear"
        @test isa(o.fields[:fuzzy], Dict)
        @test o["fuzzy.wuzzy"] == "?"
    end

    @testset "test relayout!" begin
        t1, t2, t3, l, p = fresh_data()
        # test on plot object
        relayout!(p, Dict{Symbol,Any}(:title=>"Fuzzy"); xaxis_title="wuzzy")
        @test p.layout["title"] == "Fuzzy"
        @test p.layout["xaxis.title"] == "wuzzy"

        # test on layout object
        relayout!(l, Dict{Symbol,Any}(:title=>"Fuzzy"); xaxis_title="wuzzy")
        @test l["title"] == "Fuzzy"
        @test l["xaxis.title"] == "wuzzy"
    end

    @testset "test react!" begin
        t1, t2, t3, l, p = fresh_data()
        t4 = bar(x=[1, 2, 3], y=[42, 242, 142])
        l2 = Layout(xaxis_title="wuzzy")
        react!(p, [t4], l2)

        @test length(p.data) == 1
        @test p.data[1] == t4
        @test p.layout == l2
    end

    @testset "test purge!" begin
        t1, t2, t3, l, p = fresh_data()
        purge!(p)
        @test p.data == []
        @test p.layout == Layout()
    end

    @testset "test restyle!" begin
        t1, t2, t3, l, p = fresh_data()
        # test on trace object
        restyle!(t1, 1, Dict{Symbol,Any}(:opacity=>0.4); marker_color="red")
        @test t1["opacity"] == 0.4
        @test t1["marker.color"] == "red"

        # test for single trace in plot
        restyle!(p, 2, Dict{Symbol,Any}(:opacity=>0.4); marker_color="red")
        @test p.data[2]["opacity"] == 0.4
        @test p.data[2]["marker.color"] == "red"

        # test for multiple trace in plot
        restyle!(p, [1, 3], Dict{Symbol,Any}(:opacity=>0.9); marker_color="blue")
        @test p.data[1]["opacity"] == 0.9
        @test p.data[1]["marker.color"] == "blue"
        @test p.data[3]["opacity"] == 0.9
        @test p.data[3]["marker.color"] == "blue"

        # test for all traces in plot
        restyle!(p, 1:3, Dict{Symbol,Any}(:opacity=>0.42); marker_color="white")
        for i in 1:3
            @test p.data[i]["opacity"] == 0.42
            @test p.data[i]["marker.color"] == "white"
        end

        @testset "test restyle with vector attributes applied to all traces" begin
            # test that short arrays repeat
            restyle!(p, marker_color=["red", "green"])
            @test p.data[1]["marker.color"] == "red"
            @test p.data[2]["marker.color"] == "green"
            @test p.data[3]["marker.color"] == "red"

            # test that array of arrays is repeated and applied everywhere
            restyle!(p, marker_color=(["red", "green"],))
            @test p.data[1]["marker.color"] == ["red", "green"]
            @test p.data[2]["marker.color"] == ["red", "green"]
            @test p.data[3]["marker.color"] == ["red", "green"]

            # test that array of arrays is repeated and applied everywhere
            restyle!(p, marker_color=(["red", "green"], "blue"))
            @test p.data[1]["marker.color"] == ["red", "green"]
            @test p.data[2]["marker.color"] == "blue"
            @test p.data[3]["marker.color"] == ["red", "green"]
        end

        @testset "test restyle with vector attributes applied to vector of traces" begin
            # test that short arrays repeat
            restyle!(p, 1:3, marker_color=["red", "green"])
            @test p.data[1]["marker.color"] == "red"
            @test p.data[2]["marker.color"] == "green"
            @test p.data[3]["marker.color"] == "red"

            # test that array of arrays is repeated and applied everywhere
            restyle!(p, 1:3, marker_color=(["red", "green"],))
            @test p.data[1]["marker.color"] == ["red", "green"]
            @test p.data[2]["marker.color"] == ["red", "green"]
            @test p.data[3]["marker.color"] == ["red", "green"]

            # test that array of arrays is repeated and applied everywhere
            restyle!(p, 1:3, marker_color=(["red", "green"], "blue"))
            @test p.data[1]["marker.color"] == ["red", "green"]
            @test p.data[2]["marker.color"] == "blue"
            @test p.data[3]["marker.color"] == ["red", "green"]
        end

        @testset "test restyle with vector attributes applied to trace object" begin
            restyle!(t1, 1, x=[1, 2, 3])
            @test t1["x"] == 1

            restyle!(t1, 1, x=([1, 2, 3],))
            @test t1["x"] == [1, 2, 3]
        end

        @testset "test restyle with vector attributes applied to single trace " begin
            restyle!(p, 2, x=[1, 2, 3])
            @test p.data[2]["x"] == 1

            restyle!(p, 2, x=([1, 2, 3],))
            @test p.data[2]["x"] == [1, 2, 3]
        end
    end

    @testset "test addtraces!" begin
        t1, t2, t3, l, p = fresh_data()
        p2 = Plot()

        # test add one trace to end
        addtraces!(p2, t1)
        @test length(p2.data) == 1
        @test p2.data[1] == t1

        # test add two traces to end
        addtraces!(p2, t2, t3)
        @test length(p2.data) == 3
        @test p2.data[2] == t2
        @test p2.data[3] == t3

        # test add one trace middle
        t4 = scatter()
        addtraces!(p2, 2, t4)
        @test length(p2.data) == 4
        @test p2.data[1] == t1
        @test p2.data[2] == t4
        @test p2.data[3] == t2
        @test p2.data[4] == t3

        # test add multiple trace middle
        t5 = scatter()
        t6 = scatter()
        addtraces!(p2, 2, t5, t6)
        @test length(p2.data) == 6
        @test p2.data[1] == t1
        @test p2.data[2] == t5
        @test p2.data[3] == t6
        @test p2.data[4] == t4
        @test p2.data[5] == t2
        @test p2.data[6] == t3
    end

    @testset "test deletetraces!" begin
        t1, t2, t3, l, p = fresh_data()

        # test delete one trace
        deletetraces!(p, 2)
        @test length(p.data) == 2
        @test p.data[1]["y"] == t1["y"]
        @test p.data[2]["y"] == t3["y"]

        # test delete multiple traces
        deletetraces!(p, 1, 2)
        @test length(p.data) == 0
    end

    @testset "test movetraces!" begin
        t1, t2, t3, l, p = fresh_data()

        # test move one trace to end
        movetraces!(p, 2)  # now 1 3 2
        @test p.data[1]["y"] == t1["y"]
        @test p.data[2]["y"] == t3["y"]
        @test p.data[3]["y"] == t2["y"]

        # test move two traces to end
        movetraces!(p, 1, 2) # now 2 1 3
        @test p.data[1]["y"] == t2["y"]
        @test p.data[2]["y"] == t1["y"]
        @test p.data[3]["y"] == t3["y"]

        # test move from/to
        movetraces!(p, [1, 3], [2, 1])  # 213 -> 123 -> 312
        @test p.data[1]["y"] == t3["y"]
        @test p.data[2]["y"] == t1["y"]
        @test p.data[3]["y"] == t2["y"]
    end
end
