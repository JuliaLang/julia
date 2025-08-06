# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Random
using CRC32c

const BASE_TEST_PATH = joinpath(Sys.BINDIR, "..", "share", "julia", "test")
isdefined(Main, :OffsetArrays) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "OffsetArrays.jl"))
using .Main.OffsetArrays: Origin

isdefined(Main, :FillArrays) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "FillArrays.jl"))
using .Main.FillArrays: Fill

function test_crc32c(crc32c)
    # CRC32c checksum (test data generated from @andrewcooke's CRC.jl package)
    for (n,crc) in [(0,0x00000000),(1,0xa016d052),(2,0x03f89f52),(3,0xf130f21e),(4,0x29308cf4),(5,0x53518fab),(6,0x4f4dfbab),(7,0xbd3a64dc),(8,0x46891f81),(9,0x5a14b9f9),(10,0xb219db69),(11,0xd232a91f),(12,0x51a15563),(13,0x9f92de41),(14,0x4d8ae017),(15,0xc8b74611),(16,0xa0de6714),(17,0x672c992a),(18,0xe8206eb6),(19,0xc52fd285),(20,0x327b0397),(21,0x318263dd),(22,0x08485ccd),(23,0xea44d29e),(24,0xf6c0cb13),(25,0x3969bba2),(26,0x6a8810ec),(27,0x75b3d0df),(28,0x82d535b1),(29,0xbdf7fc12),(30,0x1f836b7d),(31,0xd29f33af),(32,0x8e4acb3e),(33,0x1cbee2d1),(34,0xb25f7132),(35,0xb0fa484c),(36,0xb9d262b4),(37,0x3207fe27),(38,0xa024d7ac),(39,0x49a2e7c5),(40,0x0e2c157f),(41,0x25f7427f),(42,0x368c6adc),(43,0x75efd4a5),(44,0xa84c5c31),(45,0x0fc817b2),(46,0x8d99a881),(47,0x5cc3c078),(48,0x9983d5e2),(49,0x9267c2db),(50,0xc96d4745),(51,0x058d8df3),(52,0x453f9cf3),(53,0xb714ade1),(54,0x55d3c2bc),(55,0x495710d0),(56,0x3bddf494),(57,0x4f2577d0),(58,0xdae0f604),(59,0x3c57c632),(60,0xfe39bbb0),(61,0x6f5d1d41),(62,0x7d996665),(63,0x68c738dc),(64,0x8dfea7ae)]
        s = String(UInt8[1:n;])
        ss = SubString(String(UInt8[0:(n+1);]), 2:(n+1))
        @test crc32c(UInt8[1:n;]) == crc == crc32c(s) == crc32c(ss)
        @test crc == crc32c(UInt8(1):UInt8(n))
        m = Memory{UInt8}(undef, n)
        m .= 1:n
        @test crc == crc32c(m)
    end

    # test that crc parameter is equivalent to checksum of concatenated data,
    # and test crc of subarrays:
    a = UInt8[1:255;]
    crc_256 = crc32c(a)
    @views for n = 1:255
        @test crc32c(a[n+1:end], crc32c(a[1:n])) == crc_256
    end
    @test crc32c(IOBuffer(a)) == crc_256
    let buf = IOBuffer()
        write(buf, a[1:3])
        @test crc32c(seekstart(buf)) == crc32c(a[1:3])
        @test crc32c(buf) == 0x00000000
        @test crc32c(seek(buf, 1)) == crc32c(a[2:3])
        @test crc32c(seek(buf, 0), 2) == crc32c(a[1:2])
        @test crc32c(buf) == crc32c(a[3:3])
    end

    let f = tempname()
        try
            write(f, a)
            @test open(crc32c, f) == crc_256
            open(f, "r") do io
                @test crc32c(io, 16) == crc32c(a[1:16])
                @test crc32c(io, 16) == crc32c(a[17:32])
                @test crc32c(io) == crc32c(a[33:end])
                @test crc32c(io, 1000) == 0x00000000
            end
            a = rand(UInt8, 30000)
            write(f, a)
            @test open(crc32c, f) == crc32c(a) == open(io -> crc32c(io, 10^6), f)
        finally
            rm(f, force=true)
        end
    end

    # test longer arrays to cover all the code paths in crc32c.c
    LONG = 8192 # from crc32c.c
    SHORT = 256 # from crc32c.c
    n = LONG*3+SHORT*3+SHORT*2+64+7
    bigg = vcat(reinterpret(UInt8, hton.(0x74d7f887 .^ (1:n÷4))), UInt8[1:n%4;])
    for (offset,crc) in [(0, 0x13a5ecd5), (1, 0xecf34b7e), (2, 0xfa71b596), (3, 0xbfd24745), (4, 0xf0cb3370), (5, 0xb0ec88b5), (6, 0x258c20a8), (7, 0xa9bd638d)]
        @test crc == crc32c(@view bigg[1+offset:end])
    end

    # test crc of AbstractVector{UInt8}
    @test crc32c(Origin(0)(b"hello")) == crc32c(b"hello")
    weird_vectors = [
        view(rand(UInt8, 300000), 1:2:300000),
        vec(reinterpret(UInt8, collect(Int64(1):Int64(4)))),
        vec(reinterpret(UInt8, Int64(1):Int64(4))),
        view([0x01, 0x02], UInt(1):UInt(2)),
        Fill(0x00, UInt(100)),
        Fill(0x00, big(100)),
        reinterpret(UInt8, BitVector((true, false, true, false))),
    ]
    for a in weird_vectors
        @test crc32c(a) == crc32c(collect(a))
    end
end
unsafe_crc32c_sw(a, n, crc) =
    ccall(:jl_crc32c_sw, UInt32, (UInt32, Ptr{UInt8}, Csize_t), crc, a, n)
crc32c_sw(a::Union{Array{UInt8},Base.FastContiguousSubArray{UInt8,N,<:Array{UInt8}} where N},
          crc::UInt32=0x00000000) = unsafe_crc32c_sw(a, length(a), crc)

function crc32c_sw(s::Union{String, SubString{String}}, crc::UInt32=0x00000000)
    unsafe_crc32c_sw(s, sizeof(s), crc)
end

crc32c_sw(a::AbstractVector{UInt8}, crc::UInt32=0x00000000) =
    crc32c_sw(copyto!(Vector{UInt8}(undef, length(a)), a))
function crc32c_sw(io::IO, nb::Integer, crc::UInt32=0x00000000)
    nb < 0 && throw(ArgumentError("number of bytes to checksum must be ≥ 0"))
    buf = Vector{UInt8}(undef, min(nb, 24576))
    while !eof(io) && nb > 24576
        n = readbytes!(io, buf)
        crc = unsafe_crc32c_sw(buf, n, crc)
        nb -= n
    end
    return unsafe_crc32c_sw(buf, readbytes!(io, buf, min(nb, length(buf))), crc)
end
crc32c_sw(io::IO, crc::UInt32=0x00000000) = crc32c_sw(io, typemax(Int64), crc)
test_crc32c(crc32c)
test_crc32c(crc32c_sw)

@testset "Docstrings" begin
    @test isempty(Docs.undocumented_names(CRC32c))
end
