using Test
cmd = Base.julia_cmd()
Base.julia_exename()

if Sys.which("cc") === nothing
    println("cc required for juliac tests")
    exit(1)
end
root = @__DIR__
tmp = mktempdir()
@testset "juliac" begin
    @test success(`$(cmd) --project=$(root)/../. $(root)/../juliac.jl --output-exe $(tmp)/hello --static-call-graph $(root)/../exe_examples/hello_world.jl`)
    @test readchomp(`$(tmp)/hello`) == "Hello, world!"
    @test success(`$(cmd) --project=$(root)/../. $(root)/../juliac.jl --output-exe $(tmp)/lu --static-call-graph $(root)/../exe_examples/simple_lu.jl`)
    @test success(`$(tmp)/lu`)
end

