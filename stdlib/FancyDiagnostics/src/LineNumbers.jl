module LineNumbers

export SourceFile, compute_line, LineBreaking
import Base: getindex, setindex!, length

# Offsets are 0 based
struct SourceFile
    data::Vector{UInt8}
    offsets::Vector{UInt64}
end
length(file::SourceFile) = length(file.offsets)

function SourceFile(data)
    offsets = UInt64[0]
    buf = IOBuffer(data)
    local line
    while !eof(buf)
        line = readuntil(buf,'\n')
        !eof(buf) && push!(offsets, position(buf))
    end
    if !isempty(offsets) && line[end] == '\n'
        push!(offsets, position(buf))
    end
    SourceFile(Vector{UInt8}(data),offsets)
end

function compute_line(file::SourceFile, offset)
    ind = searchsortedfirst(file.offsets, offset)
    ind <= length(file.offsets) && file.offsets[ind] == offset ? ind : ind - 1
end

function getindex(file::SourceFile, line::Int)
    if line == length(file.offsets)
        return file.data[(file.offsets[end]+1):end]
    else
        # - 1 to skip the '\n'
        return file.data[(file.offsets[line]+1):(file.offsets[line+1]-1)]
    end
end
getindex(file::SourceFile, arr::AbstractArray) = [file[x] for x in arr]

# LineBreaking

"""
Indexing adaptor to map from a flat byte offset to a `[line][offset]` pair.
Optionally, off may specify a byte offset relative to which the line number and
offset should be computed
"""
struct LineBreaking{T}
    off::UInt64
    file::SourceFile
    obj::T
end

function indtransform(lb::LineBreaking, x::Int)
    offline = compute_line(lb.file, lb.off)
    line = compute_line(lb.file, x)
    lineoffset = lb.file.offsets[line]
    off = x - lineoffset + 1
    if lineoffset < lb.off
        off -= lb.off - lineoffset
    end
    (line - offline + 1), off
end

function getindex(lb::LineBreaking, x::Int)
    l, o = indtransform(lb, x)
    lb.obj[l][o]
end

function setindex!(lb::LineBreaking, y, x::Int)
    l, o = indtransform(lb, x)
    lb.obj[l][o] = y
end
function setindex!(lb::LineBreaking, y, x::AbstractArray)
    for i in x
        lb[i] = y
    end
end

end # module
