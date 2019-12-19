var julia_boxed_heap = {
    table: new WebAssembly.Table({initial: 4096, element: "anyref"}),
    allocated: new WeakMap(),
    allocated_primitives: new Map(),
    freelist: -1,
    max_allocated: 0,
    proxies: new Map()
};

function allocate_index() {
    if (julia_boxed_heap.freelist != -1) {
        var idx = julia_boxed_heap.freelist;
        julia_boxed_heap.freelist = julia_boxed_heap.table[idx];
        return idx;
    } else if (julia_boxed_heap.max_allocated < julia_boxed_heap.table.length) {
        julia_boxed_heap.max_allocated += 1;
        return julia_boxed_heap.max_allocated;
    }
    julia_boxed_heap.table.grow(julia_boxed_heap.table.length + 4096);
    julia_boxed_heap.max_allocated += 1;
    return julia_boxed_heap.max_allocated;
}

function box_jsval(val) {
    var idx = julia_boxed_heap.allocated.get(val);
    if (idx !== undefined)
        return idx;
    var idx = allocate_index();
    julia_boxed_heap.table.set(idx, val);
    julia_boxed_heap.allocated.set(val, idx);
    return idx;
}

function box_primitive(val) {
    var idx = julia_boxed_heap.allocated_primitives.get(val);
    if (idx !== undefined)
        return idx;
    var idx = allocate_index();
    julia_boxed_heap.table.set(idx, val);
    julia_boxed_heap.allocated_primitives.set(val, idx);
    return idx;
}

function get_boxed_jsval(idx) {
    return julia_boxed_heap.table.get(idx);
}

var jl_float64_type;
var jl_jsundefined_type; var jl_jsundefined;
var jl_bool_type; var jl_true; var jl_false;
var jl_jsnull_type; var jl_jsnull;
var jl_jsobject_type;
var jl_jsstring_type;
var jl_jssymbol_type;
var jl_jsfunction_type;

Module.initialize_jscall_runtime = function() {
    // TODO: Should this be base?
    let main_module = Module['_jl_get_main_module']()
    let name_buf = Module['_malloc'](1024)
    function jl_get_global(the_module, name) {
        stringToUTF8(name, name_buf, 1024);
        let sym = Module['_jl_symbol'](name_buf);
        return Module['_jl_get_global'](the_module, sym);
    }
    jl_float64_type = jl_get_global(main_module, "Float64")
    jl_bool_type = jl_get_global(main_module, "Bool")
    jl_true = jl_get_global(main_module, "true")
    jl_false = jl_get_global(main_module, "false")

    let base_module = jl_get_global(main_module, "Base");
    assert(base_module != 0);
    let js_module = jl_get_global(base_module, "JS");
    assert(js_module != 0);
    jl_jsundefined_type = jl_get_global(js_module, "JSUndefined")
    jl_jsundefined = jl_get_global(js_module, "undefined")
    jl_jsnull_type = jl_get_global(js_module, "JSNull")
    jl_jsnull = jl_get_global(js_module, "null")
    jl_jsobject_type = jl_get_global(js_module, "JSObject")
    jl_jsstring_type = jl_get_global(js_module, "JSString")
    jl_jssymbol_type = jl_get_global(js_module, "JSSymbol")
    jl_jsfunction_type = jl_get_global(js_module, "JSFunction")
    Module["_free"](name_buf);
}

function jl_typeof(arg_ptr) {
    return HEAP32[(arg_ptr >> 2) - 1] & 0xfffffff8;
}

function jlboxed_to_js(arg_ptr) {
    arg_type = jl_typeof(arg_ptr)
    if (arg_type == jl_float64_type) {
        return HEAPF64[arg_ptr >> 3];
    } else if (arg_type == jl_jsundefined_type) {
        return undefined;
    } else if (arg_type == jl_jsnull_type) {
        return null;
    } else if (arg_type == jl_bool_type) {
        return arg_ptr == jl_true ? true : false;
    } else {
        arg_type_type = jl_typeof(arg_type)
        assert(arg_type == jl_jsobject_type ||
                arg_type == jl_jsstring_type ||
                arg_type == jl_jssymbol_type ||
                arg_type == jl_jsfunction_type ||
                arg_type_type == jl_jsfunction_type);
        obj_idx = HEAP32[arg_ptr >> 2];
        return get_boxed_jsval(obj_idx);
    }
}

var JLProxy = {
    isExtensible: function() { return false; },
    isJlProxy: function (jsobj) {
        return jsobj['$$'] !== undefined && jsobj['$$']['type'] === 'JLProxy';
    },
    has: function(jsobj, key) {
        return false;
    },
    getPtr: function(jsobj) {
        return jsobj['$$']['ptr'];
    }
}

const JL_GC_HELD = 0x4
function mark(ptr) {
    HEAP32[(ptr >> 2) - 1] |= 0x4
}

function unmark(ptr) {
    HEAP32[(ptr >> 2) - 1] &= 0xfffffffb
}

var JLFinalize;
if (typeof FinalizationGroup !== 'undefined') {
    function free_jl_proxy(ptrs) {
        for (const ptr of ptrs) {
            unmark(ptr)
            julia_boxed_heap.delete(ptr)
        }
    }
    JLFinalize = new FinalizationGroup(free_jl_proxy);
}

function makeProxy(ptr) {
    var proxy = julia_boxed_heap.proxies[ptr];
    if (proxy !== undefined) {
        if (JLFinalize !== undefined) {
            proxy = proxy.get();
            if (proxy !== null) {
                return proxy;
            }
        } else {
            return proxy;
        }
    }
    var target = function(){};
    target['$$'] = { ptr : ptr, type : 'JLProxy' };
    proxy = new Proxy(target, JLProxy);
    if (JLFinalize !== undefined) {
        JLFinalize.register(proxy, ptr);
        julia_boxed_heap.proxies[ptr] = new WeakRef(proxy);
    } else {
        julia_boxed_heap.proxies[ptr] = proxy;
    }
    mark(ptr);
    return proxy;
}

function js_to_jlboxed(val) {
    if (typeof val == 'number') {
        return Module['_jl_box_float64'](val);
    } else if (typeof val == 'undefined') {
        return jl_jsundefined;
    } else if (typeof val == 'object' && val === null) {
        return jl_jsnull;
    } else if (typeof val == 'boolean') {
        return val ? jl_true : jl_false;
    } else if (JLProxy.isJlProxy(val)) {
        return JLProxy.getPtr(val);
    } else if (typeof val == 'string' || typeof val == 'symbol') {
        let idx = box_primitive(val);
        let ptls = Module['_jl_get_ptls_states']()
        let ptr = Module['_jl_gc_alloc'](ptls, 4,
            typeof val == 'string'   ? jl_jsstring_type   :
                                       jl_jssymbol_type)
        HEAP32[ptr >> 2] = idx;
        return ptr;
    } else if (typeof val == 'function') {
        let idx = box_jsval(val);
        let ptls = Module['_jl_get_ptls_states']()
        // We overallocate to force type-like alignment
        let ptr = Module['_jl_gc_alloc'](ptls, 8, jl_jsfunction_type);
        HEAP32[ptr >> 2] = idx;
        return ptr;
    } else {
        let jljstyp = Object.getPrototypeOf(val).constructor;
        let idx = box_jsval(val);
        let ptls = Module['_jl_get_ptls_states']()
        allocated_typ = js_to_jlboxed(jljstyp);
        let ptr = Module['_jl_gc_alloc'](ptls, 4, allocated_typ);
        HEAP32[ptr >> 2] = idx;
        return ptr;
    }
}
