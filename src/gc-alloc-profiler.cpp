// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "gc-alloc-profiler.h"

#include "julia_internal.h"
#include "gc.h"

#include <string>
#include <unordered_map>
#include <vector>

using std::unordered_map;
using std::string;
using std::vector;

struct StackFrame {
    string func_name;
    string file_name;
    intptr_t line_no;

    string total; // cache of the above fields concatenated
};

struct RawBacktrace {
    jl_bt_element_t *data;
    size_t size;
};

struct Alloc {
    size_t type_address;
    RawBacktrace backtrace;
    size_t size;
};

struct AllocProfile {
    int skip_every;

    vector<Alloc> allocs;
    unordered_map<size_t, string> type_name_by_address;

    size_t alloc_counter;
    size_t last_recorded_alloc;
};

struct Serializer {
    AllocProfile *profile;
    unordered_map<string, vector<StackFrame>> frames_cache;
    size_t cache_hits;
    size_t cache_misses;
};

// == utility functions ==

// https://stackoverflow.com/a/33799784/751061
// TODO: dedup with heap snapshot, or rebase off of that branch
void print_str_escape_json(ios_t *stream, const string &s) {
    ios_printf(stream, "\"");
    for (auto c = s.cbegin(); c != s.cend(); c++) {
        switch (*c) {
        case '"': ios_printf(stream, "\\\""); break;
        case '\\': ios_printf(stream, "\\\\"); break;
        case '\b': ios_printf(stream, "\\b"); break;
        case '\f': ios_printf(stream, "\\f"); break;
        case '\n': ios_printf(stream, "\\n"); break;
        case '\r': ios_printf(stream, "\\r"); break;
        case '\t': ios_printf(stream, "\\t"); break;
        default:
            if ('\x00' <= *c && *c <= '\x1f') {
                ios_printf(stream, "\\u%04x", (int)*c);
            } else {
                ios_printf(stream, "%c", *c);
            }
        }
    }
    ios_printf(stream, "\"");
}

struct StringTable {
    typedef unordered_map<string, size_t> MapType;

    MapType map;
    vector<string> strings;

    StringTable() {}
    StringTable(std::initializer_list<string> strs) : strings(strs) {
        for (const auto& str : strs) {
            map.insert({str, map.size()});
        }
    }

    size_t find_or_create_string_id(string key) {
        auto val = map.find(key);
        if (val == map.end()) {
            val = map.insert(val, {key, map.size()});
            strings.push_back(key);
        }
        return val->second;
    }

    void print_json_array(ios_t *stream, string key, bool newlines) {
        ios_printf(stream, "[");
        bool first = true;
        size_t id = 0;
        for (const auto &str : strings) {
            if (first) {
                first = false;
            } else {
                ios_printf(stream, newlines ? ",\n" : ",");
            }
            ios_printf(stream, "{\"id\":%zu", id);
            id++;
            ios_printf(stream, ",\"%s\":", key.c_str());
            print_str_escape_json(stream, str);
            ios_printf(stream, "}");
        }
        ios_printf(stream, "]");
    }
};

string frame_as_string(jl_bt_element_t *entry, size_t entry_size) {
    auto size_in_bytes = entry_size * sizeof(jl_bt_element_t);
    char *buf = (char*)malloc(size_in_bytes);
    for (int i=0; i < size_in_bytes; i++) {
        buf[i] = ((char*)entry)[i];
    }
    return string(buf, size_in_bytes);
}

// https://stackoverflow.com/questions/874134/find-out-if-string-ends-with-another-string-in-c
bool ends_with(string const &full_string, string const &ending) {
    if (full_string.length() >= ending.length()) {
        return (0 == full_string.compare(full_string.length() - ending.length(), ending.length(), ending));
    } else {
        return false;
    }
}

bool starts_with(string const &full_string, string const &beginning) {
    if (full_string.length() >= beginning.length()) {
        return (0 == full_string.compare(0, beginning.length(), beginning));
    } else {
        return false;
    }
}

string _type_as_string(jl_datatype_t *type) {
    if ((uintptr_t)type < 4096U) {
        return "<corrupt>";
    } else if (type == (jl_datatype_t*)jl_buff_tag) {
        return "<buffer>";
    } else if (type == (jl_datatype_t*)jl_malloc_tag) {
        return "<malloc>";
    } else if (type == jl_string_type) {
        return "<string>";
    } else if (type == jl_symbol_type) {
        return "<symbol>";
    } else if (jl_is_datatype(type)) {
        ios_t str_;
        ios_mem(&str_, 10024);
        JL_STREAM* str = (JL_STREAM*)&str_;

        jl_static_show(str, (jl_value_t*)type);

        string type_str = string((const char*)str_.buf, str_.size);
        ios_close(&str_);

        return type_str;
    } else {
        return "<missing>";
    }
}

// === stack stuff ===

vector<StackFrame> get_julia_frames(jl_bt_element_t *bt_entry) {
    vector<StackFrame> ret;

    size_t ip = jl_bt_entry_header(bt_entry);
    jl_value_t *code = jl_bt_entry_jlvalue(bt_entry, 0);
    if (jl_is_method_instance(code)) {
        // When interpreting a method instance, need to unwrap to find the code info
        code = ((jl_method_instance_t*)code)->uninferred;
    }
    if (jl_is_code_info(code)) {
        jl_code_info_t *src = (jl_code_info_t*)code;
        // See also the debug info handling in codegen.cpp.
        // NB: debuginfoloc is 1-based!
        intptr_t debuginfoloc = ((int32_t*)jl_array_data(src->codelocs))[ip];
        while (debuginfoloc != 0) {
            jl_line_info_node_t *locinfo = (jl_line_info_node_t*)
                jl_array_ptr_ref(src->linetable, debuginfoloc - 1);
            assert(jl_typeis(locinfo, jl_lineinfonode_type));
            const char *func_name = "Unknown";
            jl_value_t *method = locinfo->method;
            if (jl_is_method_instance(method))
                method = ((jl_method_instance_t*)method)->def.value;
            if (jl_is_method(method))
                method = (jl_value_t*)((jl_method_t*)method)->name;
            if (jl_is_symbol(method))
                func_name = jl_symbol_name((jl_sym_t*)method);

            ret.push_back(StackFrame{
                func_name,
                jl_symbol_name(locinfo->file),
                locinfo->line,
            });
            
            debuginfoloc = locinfo->inlined_at;
        }
    }
    else {
        // If we're using this function something bad has already happened;
        // be a bit defensive to avoid crashing while reporting the crash.
        jl_safe_printf("No code info - unknown interpreter state!\n");
    }
    return ret;
}

vector<StackFrame> get_native_frames(uintptr_t ip) JL_NOTSAFEPOINT {
    vector<StackFrame> out_frames;

    // This function is not allowed to reference any TLS variables since
    // it can be called from an unmanaged thread on OSX.
    // it means calling getFunctionInfo with noInline = 1
    jl_frame_t *frames = NULL;
    int n = jl_getFunctionInfo(&frames, ip, 0, 0);
    int i;

    for (i = 0; i < n; i++) {
        jl_frame_t frame = frames[i];
        if (!frame.func_name) {
            // TODO: record these somewhere
            // jl_safe_printf("unknown function (ip: %p)\n", (void*)ip);
        }
        else {
            out_frames.push_back(StackFrame{
                frame.func_name,
                frame.file_name,
                frame.line,
            });

            free(frame.func_name);
            free(frame.file_name);
        }
    }
    free(frames);

    return out_frames;
}

vector<StackFrame> get_frames(
    Serializer *serializer,
    jl_bt_element_t *entry,
    size_t entry_size,
    bool is_native
) {
    string entry_str = frame_as_string(entry, entry_size);

    auto maybe_frames = serializer->frames_cache.find(entry_str);
    if (maybe_frames == serializer->frames_cache.end()) {
        serializer->cache_misses++;
        auto frames = is_native
            ? get_native_frames(entry[0].uintptr)
            : get_julia_frames(entry);
        serializer->frames_cache[entry_str] = frames;
        return frames;
    } else {
        serializer->cache_hits++;
        return maybe_frames->second;
    }
}

RawBacktrace get_raw_backtrace() {
    // TODO: don't allocate this every time
    jl_bt_element_t *bt_data = (jl_bt_element_t*) malloc(JL_MAX_BT_SIZE);

    // TODO: tune the number of frames that are skipped
    size_t bt_size = rec_backtrace(bt_data, JL_MAX_BT_SIZE, 1);

    return RawBacktrace{
        bt_data,
        bt_size
    };
}

vector<StackFrame> expand_stack(Serializer *serializer, RawBacktrace backtrace) {
    vector<StackFrame> out;

    int i = 0;
    while (i < backtrace.size) {
        jl_bt_element_t *entry = backtrace.data + i;
        auto entry_size = jl_bt_entry_size(entry);
        i += entry_size;
        auto is_native = jl_bt_is_native(entry);;

        // TODO: cache frames by bt_element as string?
        auto frames = get_frames(serializer, entry, entry_size, is_native);
        
        for (auto frame : frames) {
            auto frame_label = frame.func_name;
            auto is_julia = ends_with(frame.file_name, ".jl") || frame.file_name == "top-level scope";
            auto actual_is_native = !is_julia;

            if (actual_is_native) {
                continue;
            }

            out.push_back(frame);
        }
    }

    return out;
}

string stack_frame_to_string(StackFrame frame) {
    if (frame.total != "") {
        return frame.total;
    }

    ios_t str;
    ios_mem(&str, 1024);

    ios_printf(
        &str, "%s at %s:%d",
        frame.func_name.c_str(), frame.file_name.c_str(), frame.line_no
    );

    string type_str = string((const char*)str.buf, str.size);
    frame.total = type_str;
    ios_close(&str);

    return frame.total;
}

// === trie stuff ===

void profile_serialize(ios_t *out, Serializer *serializer) {
    StringTable locations;

    ios_printf(out, "{\n");
    ios_printf(out, "  \"allocs\":[\n");
    bool first_alloc = true;
    for (auto alloc : serializer->profile->allocs) {
        if (first_alloc) {
            first_alloc = false;
        } else {
            ios_printf(out, ",\n");
        }

        auto frames = expand_stack(serializer, alloc.backtrace);
        // print out stack and type

        ios_printf(out, "    {\"stack\":[");
        bool first_frame = true;
        for (auto frame : frames) {
            if (first_frame) {
                first_frame = false;
            } else {
                ios_printf(out, ",");
            }

            size_t frame_id = locations.find_or_create_string_id(stack_frame_to_string(frame));
            ios_printf(out, "%zu", frame_id);
        }        
        ios_printf(out, "]"); // end stack

        ios_printf(out, ",\"type\":\"%zu\"", alloc.type_address);
        ios_printf(out, ",\"size\":%zu", alloc.size);
        ios_printf(out, "}"); // end alloc
    }
    ios_printf(out, "\n  ],\n"); // end allocs

    // print locations
    ios_printf(out, "  \"locations\":");
    locations.print_json_array(out, "loc", true);
    ios_printf(out, ",\n"); // end locations

    // print types
    ios_printf(out, "  \"types\":[\n");
    auto first_type = true;
    for (auto type : serializer->profile->type_name_by_address) {
        if (first_type) {
            first_type = false;
        } else {
            ios_printf(out, ",\n");
        }
        
        ios_printf(out, "    {");
        ios_printf(out, "\"id\":\"%zu\",", type.first);
        ios_printf(out, "\"name\":");
        print_str_escape_json(out, type.second);
        ios_printf(out, "}");
    }
    ios_printf(out, "\n  ]\n");

    ios_printf(out, "}\n");
}

void alloc_profile_serialize(ios_t *out, AllocProfile *profile) {
    jl_printf(JL_STDERR, "serializing trie from %d allocs\n", profile->allocs.size());

    auto serializer = Serializer{profile};
    profile_serialize(out, &serializer);

    jl_printf(JL_STDERR, "  frame cache hits: %d\n", serializer.cache_hits);
    jl_printf(JL_STDERR, "  frame cache misses: %d\n", serializer.cache_misses);
}

// == global variables manipulated by callbacks ==

AllocProfile *g_alloc_profile = nullptr;
int g_alloc_profile_enabled = false;

// == exported interface ==

JL_DLLEXPORT void jl_start_alloc_profile(int skip_every) {
    g_alloc_profile_enabled = true;
    g_alloc_profile = new AllocProfile{skip_every};
}

JL_DLLEXPORT void jl_stop_and_write_alloc_profile(ios_t *stream) {
    alloc_profile_serialize(stream, g_alloc_profile);
    ios_flush(stream);

    // TODO: free everything in the profile
    // especially all those malloc'd backtraces
    
    g_alloc_profile_enabled = false;
    
    // TODO: something to free the alloc profile?
    // I don't know how to C++
    g_alloc_profile = nullptr;
}

// == callbacks called into by the outside ==

void register_type_string(jl_datatype_t *type) {
    auto id = g_alloc_profile->type_name_by_address.find((size_t)type);
    if (id != g_alloc_profile->type_name_by_address.end()) {
        return;
    }

    string type_str = _type_as_string(type);
    g_alloc_profile->type_name_by_address[(size_t)type] = type_str;
}

void _record_allocated_value(jl_value_t *val, size_t size) {
    auto profile = g_alloc_profile;
    profile->alloc_counter++;
    auto diff = profile->alloc_counter - profile->last_recorded_alloc;
    if (diff < profile->skip_every) {
        return;
    }
    profile->last_recorded_alloc = profile->alloc_counter;

    auto type = (jl_datatype_t*)jl_typeof(val);
    register_type_string(type);

    // TODO: get stack, push into vector
    auto backtrace = get_raw_backtrace();

    profile->allocs.push_back(Alloc{
        (size_t) type,
        backtrace,
        size
    });
}

void _report_gc_started() {
    // ...
}

// TODO: figure out how to pass all of these in as a struct
void _report_gc_finished(uint64_t pause, uint64_t freed, uint64_t allocd) {
    // TODO: figure out how to put in commas
    jl_printf(
        JL_STDERR,
        "GC: pause %fms. collected %fMB. %lld allocs total\n",
        pause/1e6, freed/1e6, allocd
    );
}
