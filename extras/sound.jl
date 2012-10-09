# -*- mode: julia; -*-
# sound.jl
require("options.jl")

module Sound
import Base.*
import OptionsMod.*

export wavread, wavwrite

# Required WAV Chunk; The format chunk describes how the waveform data is stored
type WAVFormat
    compression_code::Uint16
    nchannels::Uint16
    sample_rate::Uint32
    bps::Uint32 # average bytes per second
    block_align::Uint16
    nbits::Uint16
    extra_format_bytes::Array{Uint8}

    data_length::Uint32
end
WAVFormat() = WAVFormat(uint16(0), uint16(0), uint32(0), uint32(0), uint16(0), uint16(0), Array(Uint8), uint32(0))
WAVFormat(comp, chan, fs, bytes, ba, nbits) = WAVFormat(comp, chan, fs, bytes, ba, nbits, Array(Uint8), uint32(0))

const WAV_LINEAR_PCM  = 1
const WAV_NORM_FLOAT = 3

function compression_code(code::Uint16)
    if code == WAV_LINEAR_PCM
        return "Linear PCM"
    end
    if code == WAV_NORM_FLOAT
        return "Normalized Floating Point"
    end
    return "Unknown Compression Code"
end

function read_header(io::IO)
    # check if the given file has a valid RIFF header
    riff = read(io, Uint8, 4)
    if riff !=  b"RIFF"
        error("$filename is not a valid WAV file: The RIFF header is invalid")
    end

    chunk_size = read(io, Uint32)

    # check if this is a WAV file
    format = read(io, Uint8, 4)
    if format != b"WAVE"
        error("$filename is not a valid WAV file: the format is not WAVE")
    end
    return chunk_size
end

function write_header(io::IO, fmt::WAVFormat)
    # TODO: This assumes that there are no extra_format_bytes (compression_code == 1 (PCM))
    write(io, b"RIFF") # RIFF header
    write(io, uint32(36 + fmt.data_length)) # chunk_size
    write(io, b"WAVE")
end

function read_format(io::IO, chunk_size::Uint32)
    # can I read in all of the fields at once?
    if chunk_size < 16 
        error("The WAVE Format chunk must be at least 16 bytes") 
    end 
    format = WAVFormat(read(io, Uint16), # Compression Code 
                       read(io, Uint16), # Number of Channels 
                       read(io, Uint32), # Sample Rate 
                       read(io, Uint32), # bytes per second 
                       read(io, Uint16), # block align 
                       read(io, Uint16)) # bits per sample 
    chunk_size -= 16 
    if chunk_size > 0 
        # TODO add error checking for size mismatches 
        extra_bytes = read(io, Uint16) 
        format.extra_format_bytes = read(io, Uint8, extra_bytes) 
    end 
    return format 
end

function write_format(io::IO, fmt::WAVFormat)
    # write the fmt subchunk header
    write(io, b"fmt ")
    write(io, uint32(16)) # subchunk length

    write(io, fmt.compression_code) # audio format (Uint16)
    write(io, fmt.nchannels) # number of channels (Uint16)
    write(io, fmt.sample_rate) # sample rate (Uint32)
    write(io, fmt.bps) # byte rate (Uint32)
    write(io, fmt.block_align) # byte align (Uint16)
    write(io, fmt.nbits) # number of bits per sample (UInt16)
end

number_of_samples(chunk_size::Uint32, fmt::WAVFormat) = int(chunk_size / (fmt.nbits / 8))

function native_data_type(fmt::WAVFormat)
    # WAV Files are funny;
    # Data values are signed unless the sample is encoded with 8 bits.
    if fmt.nbits == 16
        return Int16
    elseif fmt.nbits == 24
        return Int32
    elseif fmt.nbits == 32 && fmt.compression_code == WAV_NORM_FLOAT
        return Float32
    elseif fmt.nbits == 8
        return Uint8
    end
    error("$(compression_code(fmt.compression_code)) with $(int(fmt.nbits)) bits is not a supported format.")
end

function read_samples!{T<:Real}(io::IO, samples::Array{T})
    for i = 1:size(samples, 1)
        for j = 1:size(samples, 2)
            samples[i, j] = read(io, T)
        end
    end
    samples
end

# support for 24 bit values encoded in 32 bits
function read_samples!(io::IO, samples::Array{Int32})
    for i = 1:size(samples, 1)
        for j = 1:size(samples, 2)
            raw_sample = read(io, Uint8, 3)
            my_sample = uint32(0)
            my_sample |= uint32(raw_sample[1])
            my_sample |= uint32(raw_sample[2]) << 8
            my_sample |= uint32(raw_sample[3]) << 16
            # sign extend negative values
            if my_sample & 0x00800000 > 0
                my_sample |= 0xff000000
            end
            samples[i, j] = int32(my_sample)
        end
    end
    samples
end

convert_samples_to_double(samples::Array{Int16}) = convert(Array{Float64}, samples) / typemax(Int16)
convert_samples_to_double(samples::Array{Int32}) = convert(Array{Float64}, samples) / (2^23 - 1)
convert_samples_to_double(samples::Array{Float32}) = convert(Array{Float64}, samples)
convert_samples_to_double(samples::Array{Uint8}) = convert(Array{Float64}, samples) / typemax(Uint8) * 2.0 - 1.0

function read_data(io::IO, chunk_size::Uint32, fmt::WAVFormat, opts::Options)
    @defaults opts format="double"
    samps_per_channel = int(number_of_samples(chunk_size, fmt) / fmt.nchannels)
    samples = read_samples!(io, Array(native_data_type(fmt), samps_per_channel, fmt.nchannels))
    return (format == "native" ? samples : convert_samples_to_double(samples))
end

function clamp_and_write_samples{T}(io::IO, samples::Array{T, }, minval::T, maxval::T)
    # Interleave the channel samples before writing to the stream.
    for i = 1:size(samples, 1) # for each sample
        for j = 1:size(samples, 2) # for each channel
            write(io, clamp(samples[i, j], minval, maxval))
        end
    end
end

function write_data(io::IO, fmt::WAVFormat, samples::Array{Uint8, })
    if fmt.nbits != 8
        error("Uint8 arrays must be encoded with 8 bits!")
    end
    clamp_and_write_samples(io, samples, 0x0, 0xff)
end

function write_data(io::IO, fmt::WAVFormat, samples::Array{Int16, })
    if fmt.nbits != 16
        error("Int16 arrays must be encoded with 16 bits!")
    end
    clamp_and_write_samples(io, samples, typemin(Int16), typemax(Int16))
end

function write_data(io::IO, fmt::WAVFormat, samples::Array{Int32, })
    if fmt.nbits != 24
        error("Int32 arrays must be encoded with 24 bits!")
    end
    for i = 1:size(samples, 1) # number of samples in each channel
        for j = 1:fmt.nchannels
            s = clamp(samples[i, j], int32(-2^23), int32(2^23 - 1))
            # TODO Swap here when worried about big endian byte order
            write(io, uint8( s & 0x000000ff))
            write(io, uint8((s & 0x0000ff00) >> 8))
            write(io, uint8((s & 0x00ff0000) >> 16))
        end
    end
end

function write_data(io::IO, fmt::WAVFormat, samples::Array{Float32, })
    if fmt.nbits != 32
        error("Float32 arrays must be encoded with 32 bits!")
    end
    clamp_and_write_samples(io, samples, float32(-1.0), float32(1.0))
end

function write_data(io::IO, fmt::WAVFormat, samples::Array{Float64, })
    # clamp the input data to the valid range
    samples = [clamp(samples[i, j], -1.0, 1.0) for i = 1:size(samples, 1), j = 1:size(samples, 2)]

    # Scale to full range of the chosen data type
    if fmt.nbits == 16
        return write_data(io, fmt, convert(Array{Int16}, round(samples * typemax(Int16))))
    elseif fmt.nbits == 24
        return write_data(io, fmt, convert(Array{Int32}, round(samples * (2^23 - 1))))
    elseif fmt.nbits == 32
        return write_data(io, fmt, convert(Array{Float32}, samples))
    elseif fmt.nbits == 8
        return write_data(io, fmt, convert(Array{Uint8}, round((samples + 1.0) * typemax(Uint8) / 2)))
    end
    error("Unsupported bit width")
end

get_data_range(samples::Array, subrange) = samples
get_data_range(samples::Array, subrange::Int) = samples[1:subrange, :]
get_data_range(samples::Array, subrange::Real) = samples[1:convert(Int, subrange), :]
get_data_range(samples::Array, subrange::Range1{Int}) = samples[subrange, :]
get_data_range(samples::Array, subrange::Range1{Real}) = samples[convert(Range1{Int}, subrange), :]

# How do I make the options optional? It seems that I have to pass *something* in.
# @note This only works on little-endian machines! Need to byte swap on big-endian systems.
function wavread(io::IO, opts::Options)
    @defaults opts subrange=Any format="double"
    chunk_size = read_header(io)
    fmt = WAVFormat()
    samples = Array(Float64)

    # Note: This assumes that the format chunk is written in the file before the data chunk. The
    # specification does not require this assumption, but most real files are written that way.

    # Subtract the size of the format field from chunk_size; now it holds the size
    # of all the sub-chunks
    chunk_size -= 4
    while chunk_size > 0
        # Read subchunk ID and size
        subchunk_id = read(io, Uint8, 4)
        subchunk_size = read(io, Uint32)
        chunk_size -= 8 + subchunk_size
        # check the subchunk ID
        if subchunk_id == b"fmt "
            fmt = read_format(io, subchunk_size)
        elseif subchunk_id == b"data"
            if format == "size"
                @check_used opts
                return int(number_of_samples(subchunk_size, fmt) / fmt.nchannels), int(fmt.nchannels)
            end
            samples = read_data(io, subchunk_size, fmt, opts)
        else
            # return unknown sub-chunks?
            # Note: Ignoring unknown sub chunks for now
            skip(io, subchunk_size)
        end
    end
    samples = get_data_range(samples, subrange)
    @check_used opts
    return samples, fmt.sample_rate, fmt.nbits, None
end

function wavread(filename::String, opts::Options)
    @defaults opts subrange=Any format="double"
    io = open(filename, "r")
    finalizer(io, close)
    @check_used opts
    return wavread(io, opts)
end

# These are the MATLAB compatible signatures
wavread(filename::String) = wavread(filename, @options)
wavread(io::IO) = wavread(io, @options)
wavread(filename::String, fmt::String) = wavread(filename, @options format=fmt)
wavread(filename::String, N::Int) = wavread(filename, @options subrange=N)
wavread(filename::String, N::Range1{Int}) = wavread(filename, @options subrange=N)
wavread(filename::String, N::Int, fmt::String) = wavread(filename, @options subrange=N format=fmt)
wavread(filename::String, N::Range1{Int}, fmt::String) = wavread(filename, @options subrange=N format=fmt)

get_compression_code(samples::Array, nbits::Integer) = WAV_LINEAR_PCM
get_compression_code{T<:FloatingPoint}(samples::Array{T}, nbits::Integer) = nbits != 32 ? WAV_LINEAR_PCM : WAV_NORM_FLOAT

function wavwrite(samples::Array, io::IO, opts::Options)
    @defaults opts Fs=8000 nbits=16
    fmt = WAVFormat()
    fmt.compression_code = get_compression_code(samples, nbits)
    fmt.nchannels = size(samples, 2)
    fmt.sample_rate = Fs
    fmt.nbits = nbits
    fmt.block_align = fmt.nbits / 8 * fmt.nchannels
    fmt.bps = fmt.sample_rate * fmt.block_align
    fmt.data_length = length(samples) * fmt.nbits / 8

    write_header(io, fmt)
    write_format(io, fmt)
    # write the data subchunk header
    write(io, b"data")
    write(io, fmt.data_length) # Uint32
    write_data(io, fmt, samples)

    # The file is not flushed unless I explicitly call it here
    flush(io)
    @check_used opts
end

function wavwrite(samples::Array, filename::String, opts::Options)
    @defaults opts Fs=8000 nbits=16
    io = open(filename, "w")
    finalizer(io, close)
    @check_used opts
    return wavwrite(samples, io, opts)
end

wavwrite(y::Array, filename::String) = wavwrite(y, filename, @options)
wavwrite(y::Array, io::IO) = wavwrite(y, io, @options)
wavwrite(y::Array, f::Real, filename::String) = wavwrite(y, filename, @options Fs=f)
wavwrite(y::Array, f::Real, N::Real, filename::String) = wavwrite(y, filename, @options Fs=f nbits=N)

# support for writing native arrays...
wavwrite(y::Array{Uint8, }, io::IO) = wavwrite(y, io, @options nbits=8)
wavwrite(y::Array{Uint8, }, filename::String) = wavwrite(y, filename, @options nbits=8)
wavwrite(y::Array{Int16, }, io::IO) = wavwrite(y, io, @options nbits=16)
wavwrite(y::Array{Int16, }, filename::String) = wavwrite(y, filename, @options nbits=16)
wavwrite(y::Array{Int32, }, io::IO) = wavwrite(y, io, @options nbits=24)
wavwrite(y::Array{Int32, }, filename::String) = wavwrite(y, filename, @options nbits=24)
wavwrite(y::Array{Float32, }, io::IO) = wavwrite(y, io, @options nbits=32)
wavwrite(y::Array{Float32, }, filename::String) = wavwrite(y, filename, @options nbits=32)

end # module
