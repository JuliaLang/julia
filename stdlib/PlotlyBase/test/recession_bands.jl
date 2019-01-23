@testset "recession_bands" begin
    p1 = Plot(scatter(x=1:10, y=rand(10)))
    @test PlotlyBase._recession_band_shapes(p1) === nothing

    _path = joinpath(@__DIR__, "data", "us_manu_unemp.csv")
    _with_dates_raw = readdlm(_path, ',')
    with_dates = _with_dates_raw[2:end, :]
    with_dates[:, 1] = map(Date, with_dates[:, 1])

    p2 = Plot(scatter(x=with_dates[:, 1], y=with_dates[:, 2]))
    @test PlotlyBase._recession_band_shapes(p2) !== nothing
    @test length(PlotlyBase._recession_band_shapes(p2)) == 7

    p3 = Plot(scatter(x=with_dates[:, 1], y=with_dates[:, 3]))
    @test PlotlyBase._recession_band_shapes(p3) !== nothing
    @test length(PlotlyBase._recession_band_shapes(p3)) == 7

    p12 = [p1; p2]
    @test PlotlyBase._recession_band_shapes(p12) !== nothing
    @test length(PlotlyBase._recession_band_shapes(p12)) == 7

    p123 = [p1; p2; p3]
    @test PlotlyBase._recession_band_shapes(p123) !== nothing
    @test length(PlotlyBase._recession_band_shapes(p123)) == 14

    add_recession_bands!(p1)
    @test length(p1.layout["shapes"]) == 0

    add_recession_bands!(p123)
    @test length(p123.layout["shapes"]) == 14

    p23 = [p2; p3]
    china_WTO_date = Date(2001, 12, 11)
    relayout!(
        p23,
        shapes=vline([china_WTO_date], line_color="red", opacity=0.4),
    )
    add_recession_bands!(p23)
    @test length(p23.layout["shapes"]) == 15

end
