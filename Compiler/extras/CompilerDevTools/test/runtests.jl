using Test
using Compiler: code_cache, SOURCE_MODE_ABI, typeinf_ext_toplevel
using Base: inferencebarrier, get_world_counter
using CompilerDevTools
using CompilerDevTools: lookup_method_instance, SplitCacheInterp

@testset "CompilerDevTools" begin
  do_work(x, y) = x + y
  f1() = do_work(inferencebarrier(1), inferencebarrier(2))
  interp = SplitCacheInterp()
  cache = code_cache(interp)
  mi = lookup_method_instance(f1)
  @test !haskey(cache, mi)
  @test with_new_compiler(f1, interp.owner) === 3
  @test haskey(cache, mi)
  # Here `do_work` is compiled at runtime, and so must have
  # required extra work to be cached under the same cache owner.
  mi = lookup_method_instance(do_work, 1, 2)
  @test haskey(cache, mi)

  # Should not error with a builtin whose type we do not know
  f_unknown_builtin() = Base.compilerbarrier(:type, isa)(1, Int)
  with_new_compiler(f_unknown_builtin, interp.owner) === true
end;

const cinst = let world = get_world_counter()
    sig = Tuple{typeof(sin), Float64}
    mi = lookup_method_instance(sin, 1.0)
    typeinf_ext_toplevel(SplitCacheInterp(; world), mi, SOURCE_MODE_ABI)
end
@testset "No allocations when invoking CodeInstance" begin
    f(x) = invoke(sin, cinst, x)
    @test any(1:3) do _
        @allocated(f(rand())) == 0
    end
end

@testset "worldage checks" begin
    this_world = Base.get_world_counter()
    f(x) = invoke(sin, cinst, x)
    @atomic cinst.min_world = this_world + 10
    @test_throws Exception f(1.0)
end
