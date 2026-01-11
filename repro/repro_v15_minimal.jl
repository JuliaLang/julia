# Reproducer v15 - MINIMAL: just :limit => true triggers the bug

using BlockArrays, LazyBandedMatrices, FillArrays, InfiniteArrays
using LazyBandedMatrices: BlockBroadcastVector

m = 1.0
T = Float64

B = BlockVcat(
    BlockBroadcastVector{T}(vcat,
        unitblocks(-(1:∞)/2),
        unitblocks(2 * (2:∞) ./ ((m+3):2:∞)))',
    BlockBroadcastVector{T}(vcat,
        unitblocks(-(1:∞) .* ((m+1):∞) .* ((3m+2+6):6:∞) ./ (10*(1:∞).^2 .+ 4m*(m+1) .+ (13m+6) * (1:∞))),
        unitblocks(((m+2):∞) .* (2:∞) ./ (((m+3):2:∞) .* ((m+4):2:∞))))',
    BlockBroadcastVector{T}(vcat,
        unitblocks((1:∞) .* ((m+2):2:∞) .* ((m+2):∞) ./ (2*(10*(1:∞).^2 .+ 4m*(m+1) .+ (13m+6) * (1:∞)))),
        unitblocks(-2 * (2:∞).^2 ./ (((m+3):2:∞) .* ((m+4):2:∞))))',
    BlockBroadcastVector{T}(vcat,
        unitblocks(-(2:∞) .* ((m+2):2:∞) .* (1:∞) ./ (10*(1:∞).^2 .+ 4m*(m+1) .+ (13m+6) * (1:∞))),
        unitblocks(Zeros{T}(∞)))'
)

# MINIMAL reproducer: :limit => true causes GC segfault
# Without :limit, we get ArgumentError (safe)
# With :limit, we get GC corruption (unsafe)
io = IOContext(stdout, :limit => true)
show(io, MIME"text/plain"(), B)
println()
