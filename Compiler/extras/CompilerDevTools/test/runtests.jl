using Test
using Compiler: Compiler, code_cache
using Base: get_world_counter, inferencebarrier
using CompilerDevTools
using CompilerDevTools: lookup_method_instance, SplitCacheOwner, SplitCacheInterp

!@isdefined(cache_version) && (cache_version = 1)
new_cache_version() = SplitCacheOwner(global cache_version += 1)

do_work(x, y) = x + y
f1() = do_work(inferencebarrier(1), inferencebarrier(2))

@testset "CompilerDevTools" begin
  owner = new_cache_version()
  world = get_world_counter()
  interp = SplitCacheInterp(; world, owner)
  cache = code_cache(interp)
  mi = lookup_method_instance(f1)
  @test !haskey(cache, mi)
  @test with_new_compiler(f1; owner) === 3
  @test haskey(cache, mi)
  # Here `do_work` is compiled at runtime, and so must have
  # required extra work to be cached under the same cache owner.
  mi = lookup_method_instance(do_work, 1, 2)
  @test haskey(cache, mi)
end;
