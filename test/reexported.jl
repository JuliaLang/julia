module testmod
    exported = true
    private = true
    export exported
end

module usingmod
    using testmod reexported, Base.Test
    @test exported == true
    @test !isdefined(:private)
end

module usingtestmod
    using usingmod, Base.Test
    @test exported == true
    @test !isdefined(:private)
end

module importmod
    import testmod.private reexported
    using Base.Test
    @test !isdefined(:exported)
    @test private == true
end

module importtestmod
    using importmod, Base.Test
    @test !isdefined(:exported)
    @test private == true
end

module importallmod
    importall testmod reexported
    using Base.Test
    @test exported == true
    @test !isdefined(:private)
end

module importalltestmod
    using importallmod, Base.Test
    @test exported == true
    @test !isdefined(:private)
end
