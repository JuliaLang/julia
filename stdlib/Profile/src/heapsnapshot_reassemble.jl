# TODO(PR): This code hasn't been reviewed yet.

module HeapSnapshot

# SoA layout to reduce padding
struct Edges
    type::Vector{Int8}       # index into `snapshot.meta.edge_types`
    name_or_index::Vector{UInt} # Either an index into `snapshot.strings`, or the index in an array, depending on edge_type
    to_pos::Vector{UInt32}   # index into `snapshot.nodes`
end
function init_edges(n::Int)
    Edges(
        Vector{Int8}(undef, n),
        Vector{UInt}(undef, n),
        Vector{UInt32}(undef, n),
    )
end
Base.length(n::Edges) = length(n.type)

# trace_node_id and detachedness are always 0 in the snapshots Julia produces so we don't store them
struct Nodes
    type::Vector{Int8}         # index into `snapshot.meta.node_types`
    name_idx::Vector{UInt32} # index into `snapshot.strings`
    id::Vector{UInt}           # unique id, in julia it is the address of the object
    self_size::Vector{Int}     # size of the object itself, not including the size of its fields
    edge_count::Vector{UInt32} # number of outgoing edges
    edges::Edges               # outgoing edges
end
function init_nodes(n::Int, e::Int)
    Nodes(
        Vector{Int8}(undef, n),
        Vector{UInt32}(undef, n),
        Vector{UInt}(undef, n),
        Vector{Int}(undef, n),
        Vector{UInt32}(undef, n),
        init_edges(e),
    )
end
Base.length(n::Nodes) = length(n.type)

# Like Base.dec, but doesn't allocate a string and writes directly to the io object
# We know all of the numbers we're about to write fit into a UInt and are non-negative
let _dec_d100 = UInt16[(0x30 + i % 10) << 0x8 + (0x30 + i ÷ 10) for i = 0:99]
    global _write_decimal_number
    _write_decimal_number(io, x::Integer, buf) = _write_decimal_number(io, unsigned(x), buf)
    function _write_decimal_number(io, x::Unsigned, digits_buf)
        buf = digits_buf
        n = ndigits(x)
        i = n
        @inbounds while i >= 2
            d, r = divrem(x, 0x64)
            d100 = _dec_d100[(r % Int)::Int + 1]
            buf[i-1] = d100 % UInt8
            buf[i] = (d100 >> 0x8) % UInt8
            x = oftype(x, d)
            i -= 2
        end
        if i > 0
            @inbounds buf[i] = 0x30 + (rem(x, 0xa) % UInt8)::UInt8
        end
        write(io, @view buf[max(i, 1):n])
    end
end

function assemble_snapshot(in_prefix, out_file::AbstractString = in_prefix)
    open(out_file, "w") do io
        assemble_snapshot(in_prefix, io)
    end
end
# Manually parse and write the .json files, given that we don't have JSON import/export in
# julia's stdlibs.
function assemble_snapshot(in_prefix, io::IO)
    preamble = read(string(in_prefix, ".json"), String)
    pos = last(findfirst("node_count\":", preamble)) + 1
    endpos = findnext(==(','), preamble, pos) - 1
    node_count = parse(Int, String(@view preamble[pos:endpos]))

    pos = last(findnext("edge_count\":", preamble, endpos)) + 1
    endpos = findnext(==('}'), preamble, pos) - 1
    edge_count = parse(Int, String(@view preamble[pos:endpos]))

    nodes = init_nodes(node_count, edge_count)

    # Parse nodes with empty edge counts that we need to fill later
    # TODO: preallocate line buffer
    for (i, line) in enumerate(eachline(string(in_prefix, ".nodes")))
        iter = eachsplit(line, ',')
        x, s = iterate(iter)
        node_type = parse(Int8, x)
        x, s = iterate(iter, s)
        node_name_idx = parse(UInt32, x)
        x, s = iterate(iter, s)
        id = parse(UInt, x)
        x, s = iterate(iter, s)
        self_size = parse(Int,  x)
        x, s = iterate(iter, s)
        edge_count = parse(UInt, x)
        @assert edge_count == 0
        x, s = iterate(iter, s)
        @assert parse(Int8, x) == 0 # trace_node_id
        x, s = iterate(iter, s)
        @assert parse(Int8, x) == 0 # detachedness

        nodes.type[i] = node_type
        nodes.name_idx[i] = node_name_idx
        nodes.id[i] = id
        nodes.self_size[i] = self_size
        nodes.edge_count[i] = edge_count
    end

    # Parse the edges to fill in the edge counts for nodes and correct the to_node offsets
    # TODO: preallocate line buffer
    for (i, line) in enumerate(eachline(string(in_prefix, ".edges")))
        iter = eachsplit(line, ',')
        x, s = iterate(iter)
        edge_type = parse(Int8, x)
        x, s = iterate(iter, s)
        edge_name_or_index = parse(UInt, x)
        x, s = iterate(iter, s)
        from_node = parse(Int,  x)
        x, s = iterate(iter, s)
        to_node = parse(UInt32, x)

        nodes.edges.type[i] = edge_type
        nodes.edges.name_or_index[i] = edge_name_or_index
        nodes.edges.to_pos[i] = to_node * 7 # 7 fields per node, the streaming format doesn't multiply the offset by 7
        nodes.edge_count[from_node + 1] += UInt32(1)  # C and JSON use 0-based indexing
    end

    _digits_buf = zeros(UInt8, ndigits(typemax(UInt)))
    println(io, @view(preamble[1:end-2]), ",") # remove trailing "}\n", we don't end the snapshot here
    println(io, "\"nodes\":[")
    for i in 1:length(nodes)
        i > 1 && println(io, ",")
        _write_decimal_number(io, nodes.type[i], _digits_buf)
        print(io, ",")
        _write_decimal_number(io, nodes.name_idx[i], _digits_buf)
        print(io, ",")
        _write_decimal_number(io, nodes.id[i], _digits_buf)
        print(io, ",")
        _write_decimal_number(io, nodes.self_size[i], _digits_buf)
        print(io, ",")
        _write_decimal_number(io, nodes.edge_count[i], _digits_buf)
        print(io, ",0,0")
    end
    println(io, "],\"edges\":[")
    for i in 1:length(nodes.edges)
        i > 1 && println(io, ",")
        _write_decimal_number(io, nodes.edges.type[i], _digits_buf)
        print(io, ",")
        _write_decimal_number(io, nodes.edges.name_or_index[i], _digits_buf)
        print(io, ",")
        _write_decimal_number(io, nodes.edges.to_pos[i], _digits_buf)
    end
    open(string(in_prefix, ".strings"), "r") do strings_io
        skip(strings_io, 2) # skip "{\n"
        println(io, "],")
        write(io, strings_io) # strings contain the trailing "}" so we close out what we opened in preamble
    end
    return nothing
end

end
