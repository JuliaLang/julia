# This file is a part of Julia. License is MIT: https://julialang.org/license

module HeapSnapshot

"""
    assemble_snapshot(filepath::AbstractString, out_file::AbstractString)

Assemble a .heapsnapshot file from the .json files produced by `Profile.take_snapshot`.
"""

# SoA layout to reduce padding
struct Edges
    type::Vector{Int8}       # index into `snapshot.meta.edge_types`
    name_or_index::Vector{UInt} # Either an index into `snapshot.strings`, or the index in an array, depending on edge_type
    to_pos::Vector{UInt}   # index into `snapshot.nodes`
end
function Edges(n::Int)
    Edges(
        Vector{Int8}(undef, n),
        Vector{UInt}(undef, n),
        Vector{UInt}(undef, n),
    )
end
Base.length(n::Edges) = length(n.type)

# trace_node_id and detachedness are always 0 in the snapshots Julia produces so we don't store them
struct Nodes
    type::Vector{Int8}         # index into `snapshot.meta.node_types`
    name_idx::Vector{UInt32} # index into `snapshot.strings`
    id::Vector{UInt}           # unique id, in julia it is the address of the object
    self_size::Vector{Int}     # size of the object itself, not including the size of its fields
    edge_count::Vector{UInt} # number of outgoing edges
    edges::Edges               # outgoing edges
    # This is the main complexity of the .heapsnapshot format, and it's the reason we need
    # to read in all the data before writing it out. The edges vector contains all edges,
    # but organized by which node they came from. First, it contains all the edges coming
    # out of node 0, then all edges leaving node 1, etc. So we need to have visited all
    # edges, and assigned them to their corresponding nodes, before we can emit the file.
    edge_idxs::Vector{Vector{UInt}} # indexes into edges, keeping per-node outgoing edge ids
end
function Nodes(n::Int, e::Int)
    Nodes(
        Vector{Int8}(undef, n),
        Vector{UInt32}(undef, n),
        Vector{UInt}(undef, n),
        Vector{Int}(undef, n),
        Vector{UInt32}(undef, n),
        Edges(e),
        [Vector{UInt}() for _ in 1:n],  # Take care to construct n separate empty vectors
    )
end
Base.length(n::Nodes) = length(n.type)

const k_node_number_of_fields = 7

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
    preamble = read(string(in_prefix, ".metadata.json"), String)
    pos = last(findfirst("node_count\":", preamble)) + 1
    endpos = findnext(==(','), preamble, pos) - 1
    node_count = parse(Int, String(@view preamble[pos:endpos]))

    pos = last(findnext("edge_count\":", preamble, endpos)) + 1
    endpos = findnext(==(','), preamble, pos) - 1
    edge_count = parse(Int, String(@view preamble[pos:endpos]))

    nodes = Nodes(node_count, edge_count)

    orphans = Set{UInt}() # nodes that have no incoming edges
    # Parse nodes with empty edge counts that we need to fill later
    open(string(in_prefix, ".nodes"), "r") do nodes_file
        for i in 1:length(nodes)
            node_type = read(nodes_file, Int8)
            node_name_idx = read(nodes_file, UInt)
            id = read(nodes_file, UInt)
            self_size = read(nodes_file, Int)
            @assert read(nodes_file, Int) == 0 # trace_node_id
            @assert read(nodes_file, Int8) == 0 # detachedness

            nodes.type[i] = node_type
            nodes.name_idx[i] = node_name_idx
            nodes.id[i] = id
            nodes.self_size[i] = self_size
            nodes.edge_count[i] = 0 # edge_count
            # populate the orphans set with node index
            push!(orphans, i-1)
        end
    end

    # Parse the edges to fill in the edge counts for nodes and correct the to_node offsets
    open(string(in_prefix, ".edges"), "r") do edges_file
        for i in 1:length(nodes.edges)
            edge_type = read(edges_file, Int8)
            edge_name_or_index = read(edges_file, UInt)
            from_node = read(edges_file, UInt)
            to_node = read(edges_file, UInt)

            nodes.edges.type[i] = edge_type
            nodes.edges.name_or_index[i] = edge_name_or_index
            nodes.edges.to_pos[i] = to_node * k_node_number_of_fields # 7 fields per node, the streaming format doesn't multiply the offset by 7
            nodes.edge_count[from_node + 1] += UInt32(1)  # C and JSON use 0-based indexing
            push!(nodes.edge_idxs[from_node + 1], i) # Index into nodes.edges
            # remove the node from the orphans if it has at least one incoming edge
            if to_node in orphans
                delete!(orphans, to_node)
            end
        end
    end

    _digits_buf = zeros(UInt8, ndigits(typemax(UInt)))
    println(io, @view(preamble[1:end-1]), ",") # remove trailing "}" to reopen the object

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
    print(io, "],\n")
    print(io, "\"edges\":[")
    e = 1
    for n in 1:length(nodes)
        count = nodes.edge_count[n]
        len_edges = length(nodes.edge_idxs[n])
        @assert count == len_edges "For node $n: $count != $len_edges"
        for i in nodes.edge_idxs[n]
            e > 1 && print(io, ",")
            println(io)
            _write_decimal_number(io, nodes.edges.type[i], _digits_buf)
            print(io, ",")
            _write_decimal_number(io, nodes.edges.name_or_index[i], _digits_buf)
            print(io, ",")
            _write_decimal_number(io, nodes.edges.to_pos[i], _digits_buf)
            if !(nodes.edges.to_pos[i] % k_node_number_of_fields == 0)
                @warn "Bug in to_pos for edge $i from node $n: $(nodes.edges.to_pos[i])"
            end
            e += 1
        end
    end
    println(io, "],")

    # not used. Required by microsoft/vscode-v8-heap-tools
    # This order of these fields is required by chrome dev tools otherwise loading fails
    println(io, "\"trace_function_infos\":[],")
    println(io, "\"trace_tree\":[],")
    println(io, "\"samples\":[],")
    println(io, "\"locations\":[],")

    println(io, "\"strings\":[")
    open(string(in_prefix, ".strings"), "r") do strings_io
        first = true
        while !eof(strings_io)
            str_size = read(strings_io, UInt)
            str_bytes = read(strings_io, str_size)
            str = String(str_bytes)
            if first
                first = false
            else
                print(io, ",\n")
            end
            print_str_escape_json(io, str)
        end
    end
    print(io, "]}")

    # remove the uber node from the orphans
    if 0 in orphans
        delete!(orphans, 0)
    end

    @assert isempty(orphans) "Orphaned nodes: $(orphans), node count: $(length(nodes)), orphan node count: $(length(orphans))"

    return nothing
end

"""
    cleanup_streamed_files(prefix::AbstractString)

Remove files streamed during `take_heap_snapshot` in streaming mode.
"""
function cleanup_streamed_files(prefix::AbstractString)
    rm(string(prefix, ".metadata.json"))
    rm(string(prefix, ".nodes"))
    rm(string(prefix, ".edges"))
    rm(string(prefix, ".strings"))
    return nothing
end

function print_str_escape_json(stream::IO, s::AbstractString)
    print(stream, '"')
    for c in s
        if c == '"'
            print(stream, "\\\"")
        elseif c == '\\'
            print(stream, "\\\\")
        elseif c == '\b'
            print(stream, "\\b")
        elseif c == '\f'
            print(stream, "\\f")
        elseif c == '\n'
            print(stream, "\\n")
        elseif c == '\r'
            print(stream, "\\r")
        elseif c == '\t'
            print(stream, "\\t")
        elseif '\x00' <= c <= '\x1f'
            print(stream, "\\u", lpad(string(UInt16(c), base=16), 4, '0'))
        elseif !isvalid(c)
            # we have to do this because vscode's viewer doesn't like the replace character
            print(stream, "[invalid unicode character]")
        else
            print(stream, c)
        end
    end
    print(stream, '"')
end

end
