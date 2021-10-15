// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "gc-garbage-profiler.h"

#include "julia_internal.h"
#include "gc.h"

#include <string>
#include <algorithm>
#include <vector>
#include <unordered_map>

using std::string;
using std::unordered_map;
using std::vector;

// == utility functions ==

void print_str_escape_csv(ios_t *stream, const std::string &s) {
    ios_printf(stream, "\"");
    for (auto c = s.cbegin(); c != s.cend(); c++) {
        switch (*c) {
        case '"': ios_printf(stream, "\"\""); break;
        default:
            ios_printf(stream, "%c", *c);
        }
    }
    ios_printf(stream, "\"");
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

// == global variables manipulated by callbacks ==
// TODO: wrap these up into a struct

ios_t *g_gc_log_stream = nullptr;

ios_t *g_garbage_profile_stream = nullptr;
int gc_epoch = 0;
// for each type, the index in mem_event where the type
// event appears.
unordered_map<size_t, string> g_type_name_by_address;
unordered_map<size_t, size_t> g_type_address_by_value_address;
unordered_map<size_t, size_t> g_frees_by_type_address;

// == exported interface ==

JL_DLLEXPORT void jl_start_logging(ios_t *stream) {
    g_gc_log_stream = stream;
    ios_printf(g_gc_log_stream, "gc_epoch,duration_ms,bytes_freed\n");
}

JL_DLLEXPORT void jl_stop_logging() {
    g_gc_log_stream = nullptr;
}

JL_DLLEXPORT void jl_start_garbage_profile(ios_t *stream) {
    g_garbage_profile_stream = stream;
    ios_printf(g_garbage_profile_stream, "gc_epoch,type,num_freed\n");
}

JL_DLLEXPORT void jl_stop_garbage_profile() {
    // TODO: flush file?
    g_garbage_profile_stream = nullptr;
    g_type_name_by_address.clear();
    g_type_address_by_value_address.clear();
    g_frees_by_type_address.clear();
}

// == callbacks called into by the outside ==

void _report_gc_started() {
    g_frees_by_type_address.clear();
}

bool pair_cmp(std::pair<size_t, size_t> a, std::pair<size_t, size_t> b) {
    return a.second > b.second;
}

// TODO: figure out how to pass all of these in as a struct
void _report_gc_finished(uint64_t pause, uint64_t freed, uint64_t allocd) {
    if (g_gc_log_stream != nullptr) {
        ios_printf(
            g_gc_log_stream,
            "%d,%d,%d\n",
            gc_epoch, (int)(pause/1e6), freed
        );
        ios_flush(g_gc_log_stream);
    }

    // sort frees
    vector<std::pair<size_t, size_t>> pairs;
    for (auto const &pair : g_frees_by_type_address) {
        pairs.push_back(pair);
    }
    std::sort(pairs.begin(), pairs.end(), pair_cmp);

    // print frees
    if (g_garbage_profile_stream != nullptr) {
        for (auto const &pair : pairs) {
            auto type_str = g_type_name_by_address.find(pair.first);
            if (type_str != g_type_name_by_address.end()) {
                ios_printf(g_garbage_profile_stream, "%d,", gc_epoch);
                print_str_escape_csv(g_garbage_profile_stream, type_str->second);
                ios_printf(g_garbage_profile_stream, ",%d\n", pair.second);
            } else {
                jl_printf(JL_STDERR, "couldn't find type %p\n", pair.first);
                // TODO: warn about missing type
            }
        }
        ios_flush(g_garbage_profile_stream);
    }
    gc_epoch++;
}

void register_type_string(jl_datatype_t *type) {
    auto id = g_type_name_by_address.find((size_t)type);
    if (id != g_type_name_by_address.end()) {
        return;
    }

    string type_str = _type_as_string(type);
    g_type_name_by_address[(size_t)type] = type_str;
}

void _record_allocated_value(jl_value_t *val) {
    auto type = (jl_datatype_t*)jl_typeof(val);
    register_type_string(type);

    g_type_address_by_value_address[(size_t)val] = (size_t)type;
}

void _record_freed_value(jl_taggedvalue_t *tagged_val) {
    jl_value_t *val = jl_valueof(tagged_val);

    auto value_address = (size_t)val;
    auto type_address = g_type_address_by_value_address.find(value_address);
    if (type_address == g_type_address_by_value_address.end()) {
        return; // TODO: warn
    }
    auto frees = g_frees_by_type_address.find(type_address->second);

    if (frees == g_frees_by_type_address.end()) {
        g_frees_by_type_address[type_address->second] = 1;
    } else {
        g_frees_by_type_address[type_address->second] = frees->second + 1;
    }
}
