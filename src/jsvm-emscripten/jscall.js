mergeInto(LibraryManager.library, {
  jl_do_jscall: function(libname, fname, args, nargs) {
    var parent;
    if (libname == 0) {
        parent = self;
    } else {
        parent = window[UTF8ToString(libname)];
    }
    f = parent[UTF8ToString(fname)];
    js_args = new Array(nargs);
    for (var arg_idx = 0; arg_idx < nargs; arg_idx++) {
      arg_ptr = HEAP32[(args >> 2) + arg_idx];
      js_args[arg_idx] = jlboxed_to_js(arg_ptr);
    }
    return js_to_jlboxed(Reflect.apply(f, undefined, js_args));
  },
  jl_do_f_jscall: function(f, this_arg, args, nargs) {
    f = jlboxed_to_js(f)
    this_arg = jlboxed_to_js(this_arg)
    js_args = new Array(nargs);
    for (var arg_idx = 0; arg_idx < nargs; arg_idx++) {
      arg_ptr = HEAP32[(args >> 2) + arg_idx];
      js_args[arg_idx] = jlboxed_to_js(arg_ptr);
    }
    return js_to_jlboxed(Reflect.apply(f, this_arg, js_args));
  },
  jl_init_jscall: function() {
    initialize_runtime()
  },
  jl_js_instanceof: function (x, y) {
    return jlboxed_to_js(x) instanceof jlboxed_to_js(y)
  }
});
