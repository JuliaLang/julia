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

struct RawBacktrace {
    jl_bt_element_t *data;
    size_t size;
};

struct RawAlloc {
    jl_datatype_t *type_address;
    RawBacktrace backtrace;
    size_t size;
};

struct AllocProfile {
    int skip_every;

    vector<RawAlloc> allocs;
    unordered_map<size_t, string> type_name_by_address;
    unordered_map<size_t, size_t> type_address_by_value_address;
    unordered_map<size_t, size_t> frees_by_type_address;

    size_t alloc_counter;
    size_t last_recorded_alloc;
};

// == global variables manipulated by callbacks ==

AllocProfile g_alloc_profile;
int g_alloc_profile_enabled = false;

// == utility functions ==

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

RawBacktrace get_raw_backtrace() {
    jl_bt_element_t *bt_data = (jl_bt_element_t*) malloc(JL_MAX_BT_SIZE);

    // TODO: tune the number of frames that are skipped
    size_t bt_size = rec_backtrace(bt_data, JL_MAX_BT_SIZE, 1);

    return RawBacktrace{
        bt_data,
        bt_size
    };
}

// == exported interface ==

JL_DLLEXPORT void jl_start_alloc_profile(int skip_every) {
    g_alloc_profile_enabled = true;
    g_alloc_profile = AllocProfile{skip_every};
}

extern "C" {  // Needed since the function doesn't take any arguments.

JL_DLLEXPORT struct RawAllocResults jl_stop_alloc_profile() {
    g_alloc_profile_enabled = false;

    auto results = RawAllocResults{
        g_alloc_profile.allocs.data(),
        g_alloc_profile.allocs.size()
    };
    
    // package up type names
    results.num_type_names = g_alloc_profile.type_name_by_address.size();
    // TODO: free this malloc
    results.type_names = (TypeNamePair*) malloc(sizeof(TypeNamePair) * results.num_type_names);
    int i = 0;
    for (auto type_addr_name : g_alloc_profile.type_name_by_address) {
        jl_printf(JL_STDERR, "type name: %s\n", type_addr_name.second.c_str());
        // TODO: free this malloc
        char *name = (char *) malloc(type_addr_name.second.length() + 1);
        memcpy(name, type_addr_name.second.c_str(), type_addr_name.second.length() + 1);
        results.type_names[i++] = TypeNamePair{
            type_addr_name.first,
            name,
        };
    }

    // package up frees
    results.num_frees = g_alloc_profile.frees_by_type_address.size();
    results.frees = (FreeInfo*) malloc(sizeof(FreeInfo) * results.num_frees);
    int j = 0;
    for (auto type_addr_free_count : g_alloc_profile.frees_by_type_address) {
        results.frees[j++] = FreeInfo{
            type_addr_free_count.first,
            type_addr_free_count.second
        };
    }

    return results;
}

JL_DLLEXPORT void jl_free_alloc_profile() {
    g_alloc_profile.frees_by_type_address.clear();
    g_alloc_profile.type_address_by_value_address.clear();
    g_alloc_profile.type_name_by_address.clear();
    g_alloc_profile.alloc_counter = 0;
    for (auto alloc : g_alloc_profile.allocs) {
        free(alloc.backtrace.data);
    }
    g_alloc_profile.allocs.clear();
}

}

// == callbacks called into by the outside ==

void register_type_string(jl_datatype_t *type) {
    auto id = g_alloc_profile.type_name_by_address.find((size_t)type);
    if (id != g_alloc_profile.type_name_by_address.end()) {
        return;
    }

    string type_str = _type_as_string(type);
    g_alloc_profile.type_name_by_address[(size_t)type] = type_str;
}

void _record_allocated_value(jl_value_t *val, size_t size) {
    auto& profile = g_alloc_profile;
    profile.alloc_counter++;
    auto diff = profile.alloc_counter - profile.last_recorded_alloc;
    if (diff < profile.skip_every) {
        return;
    }
    profile.last_recorded_alloc = profile.alloc_counter;

    auto type = (jl_datatype_t*)jl_typeof(val);
    register_type_string(type);

    profile.type_address_by_value_address[(size_t)val] = (size_t)type;

    profile.allocs.emplace_back(RawAlloc{
        type,
        get_raw_backtrace(),
        size
    });
}

void _record_freed_value(jl_taggedvalue_t *tagged_val) {
    jl_value_t *val = jl_valueof(tagged_val);

    auto value_address = (size_t)val;
    auto type_address = g_alloc_profile.type_address_by_value_address.find(value_address);
    if (type_address == g_alloc_profile.type_address_by_value_address.end()) {
        return; // TODO: warn
    }
    auto frees = g_alloc_profile.frees_by_type_address.find(type_address->second);

    if (frees == g_alloc_profile.frees_by_type_address.end()) {
        g_alloc_profile.frees_by_type_address[type_address->second] = 1;
    } else {
        g_alloc_profile.frees_by_type_address[type_address->second] = frees->second + 1;
    }
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
