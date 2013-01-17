## -*-Julia-*-
## Test suite for Julia's sound module

require("sound")
require("options")

using Sound
using OptionsMod

# These float array comparison functions are from dists.jl
function absdiff{T<:Real}(current::AbstractArray{T}, target::AbstractArray{T})
    @test all(size(current) == size(target))
    max(abs(current - target))
end

function reldiff{T<:Real}(current::T, target::T)
    abs((current - target)/(bool(target) ? target : 1))
end

function reldiff{T<:Real}(current::AbstractArray{T}, target::AbstractArray{T})
    @test all(size(current) == size(target))
    max([reldiff(current[i], target[i]) for i in 1:length(target)])
end

## Test wavread and wavwrite
## Generate some wav files for writing and reading
for fs = (8000,11025,22050,44100,48000,96000,192000), nbits = (1,7,8,9,12,16,20,24,32,64), nsamples = convert(Array{Int}, [0, logspace(1, 4, 4)]), nchans = 1:4
    ## Test wav files
    ## The tolerance is based on the number of bits used to encode the file in wavwrite
    tol = 2.0 / (2.0^nbits - 1)

    in_data = rand(nsamples, nchans)
    @test max(in_data) <= 1.0
    @test min(in_data) >= -1.0
    io = memio()
    wavwrite(in_data, io, @options Fs=fs nbits=nbits compression=WAVE_FORMAT_PCM)
    flush(io)
    file_size = position(io)

    ## Check for the common header identifiers
    seek(io, 0)
    @test read(io, Uint8, 4) == b"RIFF"
    @test read(io, Uint32) == file_size - 8
    @test read(io, Uint8, 4) == b"WAVE"

    ## Check that wavread works on the wavwrite produced memory
    seek(io, 0)
    sz = wavread(io, @options format="size")
    @test sz == (nsamples, nchans)

    seek(io, 0)
    out_data, out_fs, out_nbits, out_extra = wavread(io)
    @test length(out_data) == nsamples * nchans
    @test size(out_data, 1) == nsamples
    @test size(out_data, 2) == nchans
    @test typeof(out_data) == Array{Float64, 2}
    @test out_fs == fs
    @test out_nbits == nbits
    @test out_extra == None
    @test absdiff(out_data, in_data) < tol

    ## test the "subrange" option.
    if nsamples > 0
        seek(io, 0)
        # Don't convert to Int, test if passing a float (nsamples/2) behaves as expected
        subsamples = min(10, nsamples / 2)
        out_data, out_fs, out_nbits, out_extra = wavread(io, @options subrange=subsamples)
        @test length(out_data) == subsamples * nchans
        @test size(out_data, 1) == subsamples
        @test size(out_data, 2) == nchans
        @test typeof(out_data) == Array{Float64, 2}
        @test out_fs == fs
        @test out_nbits == nbits
        @test out_extra == None
        @test absdiff(out_data, in_data[1:int(subsamples), :]) < tol

        seek(io, 0)
        sr = convert(Int, min(5, nsamples / 2)):convert(Int, min(23, nsamples - 1))
        out_data, out_fs, out_nbits, out_extra = wavread(io, @options subrange=sr)
        @test length(out_data) == length(sr) * nchans
        @test size(out_data, 1) == length(sr)
        @test size(out_data, 2) == nchans
        @test typeof(out_data) == Array{Float64, 2}
        @test out_fs == fs
        @test out_nbits == nbits
        @test out_extra == None
        @test absdiff(out_data, in_data[sr, :]) < tol
    end
end

## Test native encoding of 8 bits
for nchans = (1,2,4)
    in_data_8 = reshape(typemin(Uint8):typemax(Uint8), int(256 / nchans), nchans)
    io = memio()
    wavwrite(in_data_8, io)
    flush(io)

    seek(io, 0)
    out_data_8, fs, nbits, extra = wavread(io, @options format="native")
    @test fs == 8000
    @test nbits == 8
    @test extra == None
    @test in_data_8 == out_data_8
end

## Test native encoding of 16 bits
for nchans = (1,2,4)
    in_data_16 = reshape(typemin(Int16):typemax(Int16), int(65536 / nchans), nchans)
    io = memio()
    wavwrite(in_data_16, io)
    flush(io)

    seek(io, 0)
    out_data_16, fs, nbits, extra = wavread(io, @options format="native")
    @test fs == 8000
    @test nbits == 16
    @test extra == None
    @test in_data_16 == out_data_16
end

## Test native encoding of 24 bits
for nchans = (1,2,4)
    in_data_24 = convert(Array{Int32}, reshape(-63:64, int(128 / nchans), nchans))
    io = memio()
    wavwrite(in_data_24, io)
    flush(io)

    seek(io, 0)
    out_data_24, fs, nbits, extra = wavread(io, @options format="native")
    @test fs == 8000
    @test nbits == 24
    @test extra == None
    @test in_data_24 == out_data_24
end

## Test encoding 32 bit values
for nchans = (1,2,4)
    in_data_single = convert(Array{Float32}, reshape(linspace(-1.0, 1.0, 128), int(128 / nchans), nchans))
    io = memio()
    wavwrite(in_data_single, io)
    flush(io)

    seek(io, 0)
    out_data_single, fs, nbits, extra = wavread(io, @options format="native")
    @test fs == 8000
    @test nbits == 32
    @test extra == None
    @test in_data_single == out_data_single
end

## Test encoding 32 bit values outside the valid range
for nchans = (1,2,4)
    nsamps = int(128 / nchans)
    in_data_single = convert(Array{Float32}, reshape(-63:64, nsamps, nchans))
    io = memio()
    wavwrite(in_data_single, io)
    flush(io)

    seek(io, 0)
    out_data_single, fs, nbits, extra = wavread(io, @options format="native")
    @test fs == 8000
    @test nbits == 32
    @test extra == None
    @test [clamp(in_data_single[i, j], float32(-1), float32(1)) for i = 1:nsamps, j = 1:nchans] == out_data_single
end
