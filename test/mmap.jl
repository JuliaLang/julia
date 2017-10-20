# This file is a part of Julia. License is MIT: https://julialang.org/license

using Mmap

######################################################################
# test write(io, ...) for bits types, layout conformance with mmap
######################################################################

function test_write_isbits_mmap(vector, path = tempname())
    open(path, "w") do io
        for elt in vector
            write(io, elt)
        end
    end
    open(path, "r") do io
        vector_mmap = Mmap.mmap(io, Vector{eltype(vector)}, (length(vector,)))
        @test vector_mmap == vector
    end
end

test_write_isbits_mmap([Date(rand(1900:2100), rand(1:12), rand(1:28))
                        for _ in 1:200])

# testing write with a padded structure
struct Padded24
    x::Int16
    y::Int8
end

test_write_isbits_mmap([Padded24(rand(Int16), rand(Int8)) for _ in 1:215])
