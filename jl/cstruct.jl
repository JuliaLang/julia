function tmpfile(tmpf_list::Vector{String})
    filename = chomp(readall(`tempfile`))
    push(tmpf_list, filename)
    return filename
end

function tmpfile(tmpf_list::Vector{String}, suffix::String)
    filename = chomp(readall(`tempfile`))
    filename_s = "$(filename)$(suffix)"
    run(`mv $filename $filename_s`)
    push(tmpf_list, filename_s)
    return filename_s
end

function tmpfile_delete_all(tmpf_list::Vector{String})

    for filename = tmpf_list
        run(`rm -f $filename`)
    end
    return nothing
end

typealias CStructFieldDescriptor (ASCIIString, Type)

type CStructDescriptor
    struct_name::ASCIIString
    struct_fields::Vector{ASCIIString}
    fields_types::Vector{BitsKind}
    struct_size::Int32
    fields_offsets::Vector{Int32}
    function CStructDescriptor{S<:String, K<:BitsKind}(required_headers::Vector{S}, struct_name::S, struct_fields::Vector{CStructFieldDescriptor})
        include_lines = Array(String, 2 + length(required_headers))

        include_lines[1] = "#include <stdlib.h>\n"
        include_lines[2] = "#include <stdio.h>\n"
        i = 3
        for l = required_headers
            include_lines[i] = "#include <$l>\n"
            i += 1
        end

        num_struct_fields = length(struct_fields)

        struct_size_line = "long unsigned _struct_size = (long unsigned) sizeof($struct_name);\n"
        struct_decl_line = "$struct_name _sample_struct;\n"

        fields_offset_decl = "long unsigned _field_offsets[$num_struct_fields];\n"


        fields_offset_lines = Array(String, num_struct_fields)
        i = 1
        for f = struct_fields
            fields_offset_lines[i] = "_field_offsets[$(i - 1)] = (long unsigned) ((char*)(&(_sample_struct.$(f[1]))) - (char*)(&_sample_struct));\n"
            i += 1
        end

        #struct_print_line = "printf(\"STRUCT: $struct_name %lu\\n\", _struct_size);\n"
        struct_print_line = "printf(\"%lu\\n\", _struct_size);\n"

        fields_offset_print_lines = Array(String, num_struct_fields)
        for i = 1 : num_struct_fields
            #fields_offset_print_lines[i] = "printf(\"FIELD: $f %lu\\n\", _field_offsets[$(i - 1)]);\n"
            fields_offset_print_lines[i] = "printf(\"%lu\\n\", _field_offsets[$(i - 1)]);\n"
        end

        program_includes = strcat(include_lines...)

        program_main_decl = "$(struct_size_line)$(struct_decl_line)$(fields_offset_decl)"
        program_main_comp = strcat(fields_offset_lines...)
        program_main_print = strcat(struct_print_line, fields_offset_print_lines...)
        program = "$(program_includes)int main(void) {\n$(program_main_decl)$(program_main_comp)$(program_main_print)return 0;\n}"

        #println(program)

        tmpf_list = Array(String, 0)

        program_file = tmpfile(tmpf_list, ".c")
        binary_file = tmpfile(tmpf_list)

        try
            pf = open(program_file, "w")
            with_output_stream(pf, println, program)
            close(pf)
        catch err
            tmpfile_delete_all(tmpf_list)
            throw(err)
        end


        try
            #print(`gcc -Wall -o $binary_file $program_file`)
            run(`gcc -Wall -o $binary_file $program_file`)
        catch err
            tmpfile_delete_all(tmpf_list)
            throw(err)
        end

        final_result = zeros(Int32, num_struct_fields + 1)
        try
            final_result = map(int32, split(readall(`$binary_file`)))
        catch err
            tmpfile_delete_all(tmpf_list)
            throw(err)
        end

        #println("fr=$final_result")

        tmpfile_delete_all(tmpf_list)
        struct_fields_names = convert(Vector{ASCIIString}, [ x[1] | x = struct_fields ])
        struct_fields_types = convert(Vector{Type}, [ x[2] | x = struct_fields ])
        new(struct_name, struct_fields_names, struct_fields_types, final_result[1], final_result[2:end])
    end
end

function CStructDescriptor{S<:String, K}(required_headers::Vector{S}, struct_name::S, struct_fields::Vector{K})
    CStructDescriptor(required_headers, struct_name, convert(Vector{(ASCIIString, Type)}, struct_fields))
end

type CStruct
    ptr::Ptr{Void}
    desc::CStructDescriptor
    function CStruct(desc::CStructDescriptor)
        ptr = ccall(:calloc, Ptr{Void}, (Int32, Int32), 1, desc.struct_size)
        if ptr == C_NULL
            error("memory allocation failed for struct of type $(desc.struct_name)")
        end
        new(ptr, desc)
    end
end

CStruct{S<:String, K}(required_headers::Vector{S}, struct_name::S, struct_fields::Vector{K}) =
    CStruct(CStructDescriptor(required_headers, struct_name, struct_fields))

function cstruct_delete(struct::CStruct)
    ccall(:free, Void, (Ptr{Void},), struct.ptr)
    struct.ptr = C_NULL
end


function ref(struct::CStruct, field_name::String)
    if struct.ptr == C_NULL
        error("struct is not allocated")
    end
    for i = 1 : length(struct.desc.struct_fields)
        if field_name == struct.desc.struct_fields[i]
            store = Array(struct.desc.fields_types[i], 1);
            src_mempos = struct.ptr + struct.desc.fields_offsets[i]
            sz = sizeof(struct.desc.fields_types[i])
            ccall(:memmove, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Uint), store, src_mempos, sz)
            return store[1]
        end
    end
    error("field '$field_name' not found in struct '$(struct.desc.struct_name)'")
end 

function assign{T}(struct::CStruct, val::T, field_name::String)
    if struct.ptr == C_NULL
        error("struct is not allocated")
    end
    for i = 1 : length(struct.desc.struct_fields)
        if field_name == struct.desc.struct_fields[i]
            store = Array(struct.desc.fields_types[i], 1);
            store[1] = val
            dest_mempos = struct.ptr + struct.desc.fields_offsets[i]
            sz = sizeof(struct.desc.fields_types[i])
            ccall(:memmove, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Uint), dest_mempos, store, sz)
            return
        end
    end
    error("field '$field_name' not found in struct '$(struct.desc.struct_name)'")
end 

pointer(struct::CStruct) = struct.ptr

abstract CStructWrapper

assign{T}(wrap::CStructWrapper, val::T, field_name::String) = assign(wrap.struct, val, field_name)
ref(wrap::CStructWrapper, field_name::String) = ref(wrap.strucct, val, field_name)

pointer(wrap::CStructWrapper) = pointer(wrap.struct)

cstruct_delete(wrap::CStructWrapper) = cstruct_delete(wrap.struct)


#iptcp_param = CStruct(["glpk.h"], "glp_iptcp", ["msg_lev", "ord_alg"], [Int32, Int32])

#required_headers = ["glpk.h"]
#struct_name = "glp_smcp"
#struct_fields = [ "msg_lev", "meth", "pricing" ]
#fields_types = [ Int32, Int32, Int32 ]

