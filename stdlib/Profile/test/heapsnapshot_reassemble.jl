using Test

@testset "_write_decimal_number" begin
    _digits_buf = zeros(UInt8, ndigits(typemax(UInt)))
    io = IOBuffer()

    test_write(d) = begin
        Profile.HeapSnapshot._write_decimal_number(io, d, _digits_buf)
        s = String(take!(io))
        seekstart(io)
        return s
    end
    @test test_write(0) == "0"
    @test test_write(99) == "99"

    @test test_write(UInt8(0)) == "0"
    @test test_write(UInt32(0)) == "0"
    @test test_write(Int32(0)) == "0"

    @test test_write(UInt8(99)) == "99"
    @test test_write(UInt32(99)) == "99"
    @test test_write(Int32(99)) == "99"

    # Sample among possible UInts we might print
    for x in typemin(UInt8):typemax(UInt8)
        @test test_write(x) == string(x)
    end
    for x in typemin(UInt):typemax(UInt)÷10001:typemax(UInt)
        @test test_write(x) == string(x)
    end
end

function test_print_str_escape_json(input::AbstractString, expected::AbstractString)
    output = IOBuffer()
    Profile.HeapSnapshot.print_str_escape_json(output, input)
    @test String(take!(output)) == expected
end

@testset "print_str_escape_json" begin
    # Test basic string escaping
    test_print_str_escape_json("\"hello\"", "\"\\\"hello\\\"\"")

    # Test escaping of control characters
    test_print_str_escape_json("\x01\x02\x03", "\"\\u0001\\u0002\\u0003\"")

    # Test escaping of other special characters
    test_print_str_escape_json("\b\f\n\r\t", "\"\\b\\f\\n\\r\\t\"")

    # Test handling of mixed characters
    test_print_str_escape_json("abc\ndef\"ghi", "\"abc\\ndef\\\"ghi\"")

    # Test handling of empty string
    test_print_str_escape_json("", "\"\"")

    # Invalid unicode is replaced (vscode's viewer chokes on the replacement char)
    test_print_str_escape_json(String(UInt8[0xff]), "\"[invalid unicode character]\"")
end

# assemble_snapshot on hand-written part files, checking the assembled JSON
# structure and the error paths for malformed inputs
@testset "assemble_snapshot from synthetic parts" begin
    # node layout: type, name_idx, id, self_size, trace_node_id, detachedness
    # edge layout: type, name_or_index, from_node, to_node (node indices)
    function write_parts(prefix; to_node = 1, strings_short_by = 0)
        write(string(prefix, ".metadata.json"),
            """{"snapshot":{"node_count":2,"edge_count":1,"trace_function_count":0}}""")
        open(string(prefix, ".nodes"), "w") do io
            for i in 0:1
                write(io, UInt32(0), UInt(i), UInt(0x1000 + i), Int(16), Int(0), Int8(0))
            end
        end
        open(string(prefix, ".edges"), "w") do io
            write(io, UInt32(0), UInt(0), UInt(0), UInt(to_node))
        end
        open(string(prefix, ".strings"), "w") do io
            write(io, UInt(sizeof("node_a")), codeunits("node_a"))
            # optionally declare the final string longer than the bytes present
            write(io, UInt(sizeof("node_b") + strings_short_by), codeunits("node_b"))
        end
    end

    array_section(s, key) = begin
        start = last(findfirst("\"$key\":[", s)) + 1
        s[start:findnext(==(']'), s, start)-1]
    end
    n_elems(section) = isempty(strip(section)) ? 0 : count(==(','), section) + 1

    mktempdir() do tmpdir
        prefix = joinpath(tmpdir, "syn")
        out_file = joinpath(tmpdir, "syn.heapsnapshot")

        write_parts(prefix)
        Profile.HeapSnapshot.assemble_snapshot(prefix, out_file)
        sshot = read(out_file, String)
        @test n_elems(array_section(sshot, "nodes")) == 7 * 2
        @test n_elems(array_section(sshot, "edges")) == 3 * 1
        @test contains(sshot, "\"node_a\"")
        @test contains(sshot, "\"node_b\"")
        # to_node is emitted as an offset into the flat nodes array
        @test contains(array_section(sshot, "edges"), "7")
        rm(out_file)

        # a node with no incoming edges is an error, and must not leave output behind
        write_parts(prefix; to_node = 0) # node 1 becomes an orphan
        @test_throws "no incoming edges" Profile.HeapSnapshot.assemble_snapshot(prefix, out_file)
        @test !isfile(out_file)

        # a truncated strings file is an error, and must not leave output behind
        write_parts(prefix; strings_short_by = 5)
        @test_throws "truncated strings file" Profile.HeapSnapshot.assemble_snapshot(prefix, out_file)
        @test !isfile(out_file)

        # a failed assembly must preserve a pre-existing output file
        write(out_file, "pre-existing")
        @test_throws SystemError Profile.HeapSnapshot.assemble_snapshot(joinpath(tmpdir, "nonexistent"), out_file)
        @test read(out_file, String) == "pre-existing"
        rm(out_file)

        # cleanup_streamed_files tolerates missing part files
        Profile.HeapSnapshot.cleanup_streamed_files(prefix)
        @test Profile.HeapSnapshot.cleanup_streamed_files(prefix) === nothing
    end
end
