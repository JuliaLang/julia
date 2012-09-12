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

function read_data(io::IO, chunk_size::Uint32, fmt::WAVFormat, opts::Options)
    @defaults opts format="double"
    n_samples = number_of_samples(chunk_size, fmt)
    bias = 0

    # WAV Files are funny;
    # Data values are signed unless the sample is encoded with 8 bits.
    if fmt.nbits != 24
        if fmt.nbits == 16
            data_type = Int16
        elseif fmt.nbits == 32
            data_type = Int32
        elseif fmt.nbits == 8
            data_type = Uint8
            bias = 1
        elseif fmt.nbits == 64
            data_type = Int64
        else
            error("Unsupported bit width")
        end
        max_value = typemax(data_type)
        if fmt.nbits == 8
            max_value /= 2
        end
        samples = read(io, data_type, n_samples)
    else # fmt.nbits == 24
        max_value = 2^23 - 1
        data_type = Int32
        raw_samples = read(io, Uint8, n_samples * 3)
        samples = zeros(Int32, n_samples)
        for i = 1:n_samples
            samples[i] |= uint32(raw_samples[(i - 1) * 3 + 1])
            samples[i] |= uint32(raw_samples[(i - 1) * 3 + 2]) << 8
            samples[i] |= uint32(raw_samples[(i - 1) * 3 + 3]) << 16
            # sign extend negative values
            if samples[i] & 0x00800000 > 0
                samples[i] |= 0xff000000
            end
        end
    end
                
    # reshape to honor the number of channels...
    # I couldn't get reshape to work the way that I want it to. Channels are interleaved in
    # the file.
    my_samples = zeros(data_type, int(n_samples / fmt.nchannels), int(fmt.nchannels))
    for i = 1:fmt.nchannels
        my_samples[:, i] = samples[i:fmt.nchannels:n_samples]
    end
    
    # TODO I think that MATLAB always converts the samples to floating point, but I'm not sure.
    # and rebase the floating point value to the range -1.0 to 1.0 where 0 is the midpoint
    if format == "native"
        return my_samples
    end
    # format == "double"
    return convert(Array{Float64}, my_samples) / max_value - bias
end

function write_data(io::IO, fmt::WAVFormat, samples::Array)
    # write the data subchunk header
    write(io, b"data")
    write(io, fmt.data_length) # Uint32

    bias = 0
    data_type = Int16
    if fmt.nbits == 16
        data_type = Int16
    elseif fmt.nbits == 32
        data_type = Int32
    elseif fmt.nbits == 8
        data_type = Uint8
        bias = 1.0
    elseif fmt.nbits == 64
        data_type = Int64
    elseif fmt.nbits == 24
        max_value = 2^23 - 1
        samples = convert(Array{Int32}, round(samples * max_value))
        assert(max(samples) < 2^23)
         # for each sample
        for i = 1:size(samples, 1)
            # for each channel
            for j = 1:size(samples, 2)
                write(io, convert(Uint8, samples[i, j] & 0xff))
                write(io, convert(Uint8, (samples[i, j]>>8) & 0xff))
                write(io, convert(Uint8, (samples[i, j]>>16) & 0xff))
            end
        end
        return
    else
        error("Unsupported bit width")
    end
    max_value = typemax(data_type)
    if fmt.nbits == 8
        max_value /= 2
    end

    # Scale to full range of the chosen data type
    samples = round((samples + bias) * max_value)

    # Interleave the channel samples before writing to the stream.
    flat_array = zeros(Float64, length(samples))
    for i = 1:fmt.nchannels
        flat_array[i:fmt.nchannels:length(samples)] = samples[:, i]
    end
    write(io, convert(Array{data_type}, flat_array))
end

get_data_range(samples::Array, subrange::Any) = samples
get_data_range(samples::Array, subrange::Int) = samples[1:subrange, :]
get_data_range(samples::Array, subrange::Range1{Int}) = samples[subrange, :]

# How do I make the options optional? It seems that I have to pass *something* in.
# @note This only works on little-endian machines! Need to byte swap on big-endian systems.
function wavread(filename::String, opts::Options)
    @defaults opts subrange=Any format="double"
    io = open(filename, "r")
    finalizer(io, close)

    chunk_size = read_header(io)
    fmt = WAVFormat()
    samples = Array(Float32)

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
                return number_of_samples(subchunk_size, fmt), int(fmt.nchannels)
            end
            samples = read_data(io, subchunk_size, fmt, opts)
        else
            # return unknown sub-chunks?
            # Note: Ignoring unknown sub chunks for now
            skip(io, subchunk_size)
        end
    end
    samples = get_data_range(samples, subrange)
    return samples, fmt.sample_rate, fmt.nbits, None
end

# These are the MATLAB compatible signatures
wavread(filename::String) = wavread(filename, @options format="double")
wavread(filename::String, fmt::String) = wavread(filename, @options format=fmt)
wavread(filename::String, N::Int) = wavread(filename, @options subrange=N)
wavread(filename::String, N::Range1{Int}) = wavread(filename, @options subrange=N)
wavread(filename::String, N::Int, fmt::String) = wavread(filename, @options subrange=N format=fmt)
wavread(filename::String, N::Range1{Int}, fmt::String) = wavread(filename, @options subrange=N format=fmt)

function wavwrite(samples::Array, sample_rate::Number, nbits::Number, filename::String)
    io = open(filename, "w")
    finalizer(io, close)

    fmt = WAVFormat()
    fmt.compression_code = 1 # Linear PCM
    fmt.nchannels = size(samples, 2)
    fmt.sample_rate = sample_rate
    fmt.nbits = nbits
    fmt.block_align = fmt.nbits / 8 * fmt.nchannels
    fmt.bps = fmt.sample_rate * fmt.block_align
    fmt.data_length = length(samples) * fmt.nbits / 8

    write_header(io, fmt)
    write_format(io, fmt)
    write_data(io, fmt, samples)

    # The file is not flushed unless I explicitly call it here
    flush(io)
end

end # module
