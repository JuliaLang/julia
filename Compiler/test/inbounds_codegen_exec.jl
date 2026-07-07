# This file is a part of Julia. License is MIT: https://julialang.org/license

# (Executed by inbounds_codegen.jl in a subprocess with default bounds checking.)
# Generated regression tests for the @inbounds removal in base/: for
# representative kernels per touched file, assert that the emitted LLVM
# contains no bounds-error throw - either because LLVM elides the checks
# (mechanism 1) or because the effect split hoists them and outlines the
# throwing fallback (mechanism 2). Entries were verified against the built
# image at generation time; regenerate with the audit tooling when kernels
# change.

using Test
using InteractiveUtils: code_llvm

# NB: deliberately no setup_Compiler.jl here - these tests assert NATIVE
# compiler codegen; a previously @activate'd reflection Compiler (e.g. by
# another test group in the same worker) would change what code_llvm reflects,
# so skip in that case.

const CHECK_BOUNDS_OFF = Base.JLOptions().check_bounds == 2
const COVERAGE = (Base.JLOptions().code_coverage > 0) || (Base.JLOptions().malloc_log > 0)

function nb_throws(@nospecialize(f), @nospecialize(tt))
    ir = sprint((io, a...)->code_llvm(io, a...; debuginfo=:none), f, tt)
    return count("throw_boundserror", ir) + count("bounds_error", ir)
end

if Base.REFLECTION_COMPILER[] === nothing && !CHECK_BOUNDS_OFF && !COVERAGE
@testset "inbounds removal codegen" begin
    # base/abstractarray.jl
    isdefined(Base, Symbol("copyto_unaliased!")) && @test nb_throws(Base.copyto_unaliased!, Tuple{IndexLinear, Vector{Int64}, IndexLinear, Vector{Int64}}) == 0
    isdefined(Base, Symbol("unsafe_getindex")) && @test nb_throws(Base.unsafe_getindex, Tuple{Vector{Int64}, Int64}) == 0
    isdefined(Base, Symbol("unsafe_setindex!")) && @test nb_throws(Base.unsafe_setindex!, Tuple{Vector{Int64}, Int64, Int64}) == 0
    # base/abstractarraymath.jl
    isdefined(Base, Symbol("conj!")) && @test nb_throws(Base.conj!, Tuple{Vector{ComplexF64}}) == 0
    # base/accumulate.jl
    isdefined(Base, Symbol("_accumulate_pairwise!")) && @test nb_throws(Base._accumulate_pairwise!, Tuple{typeof(Base.add_sum),Vector{Float64},Vector{Float64},Float64,Int,Int}) == 0
    isdefined(Base, Symbol("accumulate_pairwise!")) && @test nb_throws(Base.accumulate_pairwise!, Tuple{typeof(Base.add_sum),Vector{Float64},Vector{Float64}}) == 0
    # base/array.jl
    isdefined(Base, Symbol("isassigned")) && @test nb_throws(Base.isassigned, Tuple{Matrix{Int64}, Int64, Int64}) == 0
    isdefined(Base, Symbol("isassigned")) && @test nb_throws(Base.isassigned, Tuple{Vector{Int64}, Int64}) == 0
    isdefined(Base, Symbol("setindex_widen_up_to")) && @test nb_throws(Base.setindex_widen_up_to, Tuple{Vector{Real}, Int64, Float64}) == 0
    # base/arraymath.jl
    isdefined(Base, Symbol("_reverse!")) && @test nb_throws(Base._reverse!, Tuple{Matrix{Int64},Tuple{Int64}}) == 0
    # base/bitarray.jl
    isdefined(Base, Symbol("glue_src_bitchunks")) && @test nb_throws(Base.glue_src_bitchunks, Tuple{Vector{UInt64},Int,Int,UInt64,Int}) == 0
    isdefined(Base, Symbol("fill_chunks!")) && @test nb_throws(Base.fill_chunks!, Tuple{Vector{UInt64},Bool,Int,Int}) == 0
    isdefined(Base, Symbol("copy_to_bitarray_chunks!")) && @test nb_throws(Base.copy_to_bitarray_chunks!, Tuple{Vector{UInt64},Int,Vector{Bool},Int,Int}) == 0
    # base/bitset.jl
    isdefined(Base, Symbol("iterate")) && @test nb_throws(Base.iterate, Tuple{BitSet, Tuple{UInt64,Int}}) == 0
    isdefined(Base, Symbol("_check0")) && @test nb_throws(Base._check0, Tuple{Vector{UInt64},Int,Int}) == 0
    isdefined(Base, Symbol("issubset")) && @test nb_throws(Base.issubset, Tuple{BitSet,BitSet}) == 0
    # base/broadcast.jl
    isdefined(Base.Broadcast, Symbol("chunkedcopyto!")) && @test nb_throws(Base.Broadcast.chunkedcopyto!, Tuple{BitVector, Base.Broadcast.Broadcasted{Base.Broadcast.DefaultArrayStyle{1}, Tuple{Base.OneTo{Int64}}, typeof(&), Tuple{BitVector, BitVector}}}) == 0
    isdefined(Base.Broadcast, Symbol("copy")) && @test nb_throws(Base.Broadcast.copy, Tuple{Base.Broadcast.Broadcasted{Base.Broadcast.Style{Tuple}, Nothing, typeof(+), Tuple{Tuple{Int64,Int64,Int64}, Tuple{Int64,Int64,Int64}}}}) == 0
    isdefined(Base.Broadcast, Symbol("fill!")) && @test nb_throws(Base.Broadcast.fill!, Tuple{Base.Broadcast.BitMaskedBitArray{1,1}, Bool}) == 0
    # base/combinatorics.jl
    isdefined(Base, Symbol("swapcols!")) && @test nb_throws(Base.swapcols!, Tuple{Matrix{Float64}, Int, Int}) == 0
    isdefined(Base, Symbol("swaprows!")) && @test nb_throws(Base.swaprows!, Tuple{Matrix{Float64}, Int, Int}) == 0
    # base/dict.jl
    isdefined(Base, Symbol("skip_deleted")) && @test nb_throws(Base.skip_deleted, Tuple{Dict{Int,Int}, Int}) == 0
    # base/genericmemory.jl
    isdefined(Base, Symbol("isassigned")) && @test nb_throws(Base.isassigned, Tuple{Memory{Int64}, Int64}) == 0
    isdefined(Base, Symbol("copy")) && @test nb_throws(Base.copy, Tuple{Memory{Int64}}) == 0
    # base/intfuncs.jl
    isdefined(Base, Symbol("append_c_digits")) && @test nb_throws(Base.append_c_digits, Tuple{Int64, UInt64, Vector{UInt8}, Int64}) == 0
    isdefined(Base, Symbol("append_nine_digits")) && @test nb_throws(Base.append_nine_digits, Tuple{UInt64, Vector{UInt8}, Int64}) == 0
    isdefined(Base, Symbol("dec")) && @test nb_throws(Base.dec, Tuple{UInt64, Int64, Bool}) == 0
    # base/io.jl
    isdefined(Base, Symbol("countlines")) && @test nb_throws(Base.countlines, Tuple{IOBuffer}) == 0
    # base/iterators.jl
    isdefined(Base.Iterators, Symbol("iterate")) && @test nb_throws(Base.Iterators.iterate, Tuple{Base.Iterators.PartitionIterator{UnitRange{Int}}, Int}) == 0
    isdefined(Base.Iterators, Symbol("iterate")) && @test nb_throws(Base.Iterators.iterate, Tuple{Base.Iterators.PartitionIterator{Vector{Int}}, Int}) == 0
    # base/missing.jl
    isdefined(Base, Symbol("eachindex")) && @test nb_throws(Base.eachindex, Tuple{Base.SkipMissing{Vector{Union{Missing, Int64}}}}) == 0
    isdefined(Base, Symbol("keys")) && @test nb_throws(Base.keys, Tuple{Base.SkipMissing{Dict{Int64, Union{Missing, Int64}}}}) == 0
    isdefined(Base, Symbol("_mapreduce")) && @test nb_throws(Base._mapreduce, Tuple{typeof(identity), typeof(+), IndexLinear, Base.SkipMissing{Vector{Union{Missing, Int64}}}}) == 0
    # base/mpfr.jl
    isdefined(Base, Symbol("getindex")) && @test nb_throws(Base.getindex, Tuple{Base.MPFR.BigFloatData{UInt64}, Int}) == 0
    isdefined(Base, Symbol("BigFloat")) && @test nb_throws(Base.BigFloat, Tuple{Float64}) == 0
    # base/multidimensional.jl
    isdefined(Base, Symbol("_unsafe_getindex!")) && @test nb_throws(Base._unsafe_getindex!, Tuple{BitVector, BitMatrix, Vector{Int}, Int}) == 0
    isdefined(Base, Symbol("isassigned")) && @test nb_throws(Base.isassigned, Tuple{Matrix{Int64}, Int, Int}) == 0
    # base/permuteddimsarray.jl
    isdefined(Base, Symbol("setindex!")) && @test nb_throws(Base.setindex!, Tuple{PermutedDimsArray{Float64,2,(2,1),(2,1),Matrix{Float64}}, Float64, Int64, Int64}) == 0
    isdefined(Base, Symbol("isassigned")) && @test nb_throws(Base.isassigned, Tuple{PermutedDimsArray{Float64,2,(2,1),(2,1),Matrix{Float64}}, Int64, Int64}) == 0
    # base/reduce.jl
    isdefined(Base, Symbol("mapreduce_impl")) && @test nb_throws(Base.mapreduce_impl, Tuple{typeof(identity), typeof(Base.add_sum), Vector{Float64}, Int, Int, Int}) == 0
    isdefined(Base, Symbol("_mapreduce")) && @test nb_throws(Base._mapreduce, Tuple{typeof(identity), typeof(Base.add_sum), IndexLinear, Vector{Float64}}) == 0
    # base/regex.jl
    isdefined(Base, Symbol("count")) && @test nb_throws(Base.count, Tuple{Char, String}) == 0
    # base/reshapedarray.jl
    isdefined(Base, Symbol("isassigned")) && @test nb_throws(Base.isassigned, Tuple{Base.ReshapedArray{Int,2,UnitRange{Int},Tuple{}}, Int}) == 0
    isdefined(Base, Symbol("getindex")) && @test nb_throws(Base.getindex, Tuple{Base.ReshapedArray{Int,2,UnitRange{Int},Tuple{}}, Int}) == 0
    isdefined(Base, Symbol("getindex")) && @test nb_throws(Base.getindex, Tuple{Base.ReshapedArray{Int,2,UnitRange{Int},Tuple{}}, Base.ReshapedIndex{Int}}) == 0
    # base/runtime_internals.jl
    isdefined(Base, Symbol("fieldname")) && @test nb_throws(Base.fieldname, Tuple{DataType, Int}) == 0
    # base/ryu/exp.jl
    isdefined(Base.Ryu, Symbol("writeexp")) && @test nb_throws(Base.Ryu.writeexp, Tuple{Vector{UInt8}, Int, Float64, Int, Bool, Bool, UInt8, UInt8}) == 0
    # base/set.jl
    isdefined(Base, Symbol("unique!")) && @test nb_throws(Base.unique!, Tuple{typeof(identity), Vector{Int64}}) == 0
    isdefined(Base, Symbol("_unique!")) && @test nb_throws(Base._unique!, Tuple{typeof(identity), Vector{Int64}, Set{Int64}, Int64, Int64}) == 0
    isdefined(Base, Symbol("_indexed_allunique")) && @test nb_throws(Base._indexed_allunique, Tuple{Vector{Int64}}) == 0
    # base/sort.jl
    isdefined(Base, Symbol("searchsortedfirst")) && @test nb_throws(Base.searchsortedfirst, Tuple{Vector{Int64},Int64,Int64,Int64,Base.Order.ForwardOrdering}) == 0
    isdefined(Base, Symbol("searchsortedlast")) && @test nb_throws(Base.searchsortedlast, Tuple{Vector{Int64},Int64,Int64,Int64,Base.Order.ForwardOrdering}) == 0
    isdefined(Base.Sort, Symbol("send_to_end!")) && @test nb_throws(Base.Sort.send_to_end!, Tuple{typeof(iszero),Vector{Int64}}) == 0
    # base/strings/basic.jl
    isdefined(Base, Symbol("getindex")) && @test nb_throws(Base.getindex, Tuple{String,Int}) == 0
    isdefined(Base, Symbol("get")) && @test nb_throws(Base.get, Tuple{String,Int,Nothing}) == 0
    isdefined(Base, Symbol("length")) && @test nb_throws(Base.length, Tuple{String}) == 0
    # base/strings/search.jl
    isdefined(Base, Symbol("findnext")) && @test nb_throws(Base.findnext, Tuple{Base.Fix2{typeof(isequal), Char}, String, Int64}) == 0
    isdefined(Base, Symbol("findall")) && @test nb_throws(Base.findall, Tuple{String, String}) == 0
    isdefined(Base, Symbol("findprev")) && @test nb_throws(Base.findprev, Tuple{Base.Fix2{typeof(isequal), Char}, String, Int64}) == 0
    # base/strings/string.jl
    isdefined(Base, Symbol("_thisind_str")) && @test nb_throws(Base._thisind_str, Tuple{String,Int}) == 0
    isdefined(Base, Symbol("_nextind_str")) && @test nb_throws(Base._nextind_str, Tuple{String,Int}) == 0
    isdefined(Base, Symbol("_utf_dfa_step")) && @test nb_throws(Base._utf_dfa_step, Tuple{UInt32,UInt8}) == 0
    # base/strings/substring.jl
    isdefined(Base, Symbol("SubString")) && @test nb_throws(Base.SubString, Tuple{String}) == 0
    isdefined(Base, Symbol("codeunit")) && @test nb_throws(Base.codeunit, Tuple{SubString{String},Int64}) == 0
    isdefined(Base, Symbol("getindex")) && @test nb_throws(Base.getindex, Tuple{SubString{String},Int64}) == 0
    # base/strings/util.jl
    isdefined(Base, Symbol("chopprefix")) && @test nb_throws(Base.chopprefix, Tuple{Base.AnnotatedString{String},String}) == 0
    isdefined(Base, Symbol("chopsuffix")) && @test nb_throws(Base.chopsuffix, Tuple{String,String}) == 0
    isdefined(Base, Symbol("chomp")) && @test nb_throws(Base.chomp, Tuple{String}) == 0
    # base/subarray.jl
    isdefined(Base, Symbol("_maybe_reindex")) && @test nb_throws(Base._maybe_reindex, Tuple{SubArray{Float64,2,Matrix{Float64},Tuple{UnitRange{Int64},UnitRange{Int64}},false}, Tuple{Int64,Int64}, Tuple{}}) == 0
    isdefined(Base, Symbol("getindex")) && @test nb_throws(Base.getindex, Tuple{SubArray{Float64,2,Matrix{Float64},Tuple{Base.Slice{Base.OneTo{Int64}},UnitRange{Int64}},true}, Int64}) == 0
    isdefined(Base, Symbol("getindex")) && @test nb_throws(Base.getindex, Tuple{SubArray{Float64,1,Vector{Float64},Tuple{UnitRange{Int64}},true}, Int64}) == 0
    # base/summarysize.jl
    isdefined(Base, Symbol("summarysize")) && @test nb_throws(Base.summarysize, Tuple{Any}) == 0
end
end # native compiler && !CHECK_BOUNDS_OFF && !COVERAGE
