// This file is a part of Julia. License is MIT: https://julialang.org/license

// RUN: clang --analyze -Xanalyzer -analyzer-output=text -Xclang -load -Xclang libGCCheckerPlugin%shlibext -I%julia_home/src -I%julia_home/src/support -I%julia_home/usr/include ${CLANGSA_FLAGS} ${CPPFLAGS} ${CFLAGS} -Xclang -analyzer-checker=core,julia.GCChecker --analyzer-no-default-checks -Xclang -verify -x c %s

#include "julia.h"
#include "julia_internal.h"

extern void look_at_value(jl_value_t *v);
extern void process_unrooted(jl_value_t *maybe_unrooted JL_MAYBE_UNROOTED);
extern void jl_gc_safepoint();

void unrooted_argument() {
    look_at_value((jl_value_t*)jl_svec1(NULL)); // expected-warning{{Passing non-rooted value as argument to function}}
                                                // expected-note@-1{{Passing non-rooted value as argument to function}}
                                                // expected-note@-2{{Started tracking value here}}
};

void simple_svec() {
    // This is ok, because jl_svecref is non-allocating
    jl_svec_t *val = jl_svec1(NULL);
    assert(jl_svecref(val, 0) == NULL);
}

jl_value_t *simple_missing_root() {
    jl_svec_t *val = jl_svec1(NULL);
    jl_gc_safepoint();
    return jl_svecref(val, 0); // XXX-expected-warning{{Passing non-rooted value as argument to function}}
};

jl_value_t *root_value() {
    jl_svec_t *val = jl_svec2(NULL, NULL);
    JL_GC_PUSH1(&val);
    jl_gc_safepoint();
    jl_value_t *ret = jl_svecref(val, 0);
    JL_GC_POP();
    return ret;
};

void root_value_data() {
    jl_svec_t *val = jl_svec1(NULL); // expected-note{{Started tracking value here}}
    jl_value_t **data = jl_svec_data(val);
    JL_GC_PUSH1(&val); // expected-note{{GC frame changed here}}
                       // expected-note@-1{{Value was rooted here}}
    jl_gc_safepoint();
    look_at_value(*data);
    JL_GC_POP(); // expected-note{{GC frame changed here}}
                 // expected-note@-1{{Root was released here}}
    jl_gc_safepoint(); // expected-note{{Value may have been GCed here}}
    *data; // expected-warning{{Creating derivative of value that may have been GCed}}
           // expected-note@-1{{Creating derivative of value that may have been GCed}}
};

void root_value_data2() {
    jl_svec_t *val = jl_svec1(NULL); // expected-note{{Started tracking value here}}
    jl_value_t **data = jl_svec_data(val);
    JL_GC_PUSH1(&val); // expected-note{{GC frame changed here}}
                       // expected-note@-1{{Value was rooted here}}
    jl_gc_safepoint();
    look_at_value(data[0]);
    JL_GC_POP(); // expected-note{{GC frame changed here}}
                 // expected-note@-1{{Root was released here}}
    jl_gc_safepoint(); // expected-note{{Value may have been GCed here}}
    data[0]; // expected-warning{{Creating derivative of value that may have been GCed}}
             // expected-note@-1{{Creating derivative of value that may have been GCed}}
};


void root_value_data3() {
    jl_svec_t *val = jl_svec1(NULL); // expected-note{{Started tracking value here}}
    jl_value_t **data = jl_svec_data(val);
    JL_GC_PUSH1(&val); // expected-note{{GC frame changed here}}
                       // expected-note@-1{{Value was rooted here}}
    jl_gc_safepoint();
    look_at_value(**&data);
    JL_GC_POP(); // expected-note{{GC frame changed here}}
                 // expected-note@-1{{Root was released here}}
    jl_gc_safepoint(); // expected-note{{Value may have been GCed here}}
    **&data; // expected-warning{{Creating derivative of value that may have been GCed}}
             // expected-note@-1{{Creating derivative of value that may have been GCed}}
};

void root_value_data4() {
    jl_svec_t *val = jl_svec1(NULL); // expected-note{{Started tracking value here}}
    jl_value_t **data = jl_svec_data(val);
    JL_GC_PUSH1(&val); // expected-note{{GC frame changed here}}
                       // expected-note@-1{{Value was rooted here}}
    jl_gc_safepoint();
    look_at_value(*&data[0]);
    JL_GC_POP(); // expected-note{{GC frame changed here}}
                 // expected-note@-1{{Root was released here}}
    jl_gc_safepoint(); // expected-note{{Value may have been GCed here}}
    *&data[0]; // expected-warning{{Creating derivative of value that may have been GCed}}
               // expected-note@-1{{Creating derivative of value that may have been GCed}}
};


jl_value_t *existing_root() {
    jl_svec_t *val = NULL;
    JL_GC_PUSH1(&val);
    val = jl_svec1(NULL);
    jl_gc_safepoint();
    jl_value_t *ret = jl_svecref(val, 0);
    JL_GC_POP();
    return ret;
};

jl_value_t *late_root() {
    jl_svec_t *val = NULL;
    val = jl_svec1(NULL); // expected-note {{Started tracking value here}}
    jl_gc_safepoint(); // expected-note {{Value may have been GCed here}}
    JL_GC_PUSH1(&val); // expected-warning{{Trying to root value which may have been GCed}}
                       // expected-note@-1{{Trying to root value which may have been GCed}}
    jl_value_t *ret = jl_svecref(val, 0);
    JL_GC_POP();
    return ret;
};

jl_value_t *late_root2() {
    jl_svec_t *val = NULL;
    jl_svec_t *val2 = NULL;
    JL_GC_PUSH1(&val); // expected-note {{GC frame changed here}}
    val2 = jl_svec1(NULL); // expected-note {{Started tracking value here}}
    jl_gc_safepoint(); // expected-note {{Value may have been GCed here}}
    val = val2; // expected-warning{{Trying to root value which may have been GCed}}
                // expected-note@-1{{Trying to root value which may have been GCed}}
    jl_value_t *ret = jl_svecref(val, 0);
    JL_GC_POP();
    return ret;
};

jl_value_t *already_freed() {
    jl_svec_t *val = NULL;
    JL_GC_PUSH1(&val);
    val = jl_svec1(NULL);
    JL_GC_POP();
    jl_gc_safepoint();
    jl_value_t *ret = jl_svecref(val, 0);
    return ret;
};

int field_access() {
    jl_svec_t *val = jl_svec1(NULL); // expected-note {{Started tracking value here}}
    jl_gc_safepoint(); // expected-note{{Value may have been GCed here}}
    return val->length == 1; // expected-warning{{Trying to access value which may have been GCed}}
                             // expected-note@-1{{Trying to access value which may have been GCed}}
}

int pushargs_roots() {
  jl_value_t **margs;
  jl_svec_t *val = jl_svec1(NULL);;
  JL_GC_PUSHARGS(margs, 2);
  margs[1] = (jl_value_t*)val;
  jl_gc_safepoint();
  JL_GC_POP();
  return val->length == 1;
}

int pushargs_roots_freed() {
  jl_value_t **margs;
  jl_svec_t *val = jl_svec1(NULL); // expected-note{{Started tracking value here}}
  JL_GC_PUSHARGS(margs, 1); // expected-note{{GC frame changed here}}
  margs[0] = (jl_value_t*)val; // expected-note{{Value was rooted here}}
  JL_GC_POP(); // expected-note{{GC frame changed here}}
               // expected-note@-1{{Root was released here}}
  jl_gc_safepoint(); // expected-note{{Value may have been GCed here}}
  return val->length == 1; // expected-warning{{Trying to access value which may have been GCed}}
                           // expected-note@-1{{Trying to access value which may have been GCed}}
}

int unrooted() {
  jl_svec_t *val = jl_svec1(NULL); // expected-note{{Started tracking value here}}
  // This is ok
  process_unrooted((jl_value_t*)val); // expected-note{{Value may have been GCed here}}
  // This is not
  return val->length == 1; // expected-warning{{Trying to access value which may have been GCed}}
                           // expected-note@-1{{Trying to access value which may have been GCed}}
}

extern jl_value_t *global_value JL_GLOBALLY_ROOTED;
void globally_rooted() {
  jl_value_t *val = global_value;
  jl_gc_safepoint();
  look_at_value(val);
  JL_GC_PUSH1(&val);
  jl_gc_safepoint();
  look_at_value(val);
  JL_GC_POP();
  jl_gc_safepoint();
  look_at_value(val);
}

extern jl_value_t *first_array_elem(jl_array_t *a JL_PROPAGATES_ROOT);
void root_propagation(jl_expr_t *expr) {
  jl_value_t *val = first_array_elem(expr->args);
  jl_gc_safepoint();
  look_at_value(val);
}

void argument_propagation(jl_value_t *a) {
  jl_svec_t *types = jl_svec2(NULL, NULL);
  JL_GC_PUSH1(&types);
  jl_value_t *val = jl_svecset(types, 0, jl_typeof(a));
  jl_gc_safepoint();
  look_at_value(val);
  jl_svecset(types, 1, jl_typeof(a));
  JL_GC_POP();
}

// New value creation via []
void arg_array(jl_value_t **args) {
  jl_gc_safepoint();
  jl_value_t *val = args[1];
  look_at_value(val);
  jl_value_t *val2 = NULL;
  JL_GC_PUSH1(&val2);
  val2 = val;
  JL_GC_POP();
}

// New value creation via ->
void member_expr(jl_expr_t *e) {
  jl_value_t *val = NULL;
  JL_GC_PUSH1(&val);
  val = (jl_value_t*)e->args;
  JL_GC_POP();
}

void member_expr2(jl_typemap_entry_t *tm) {
  jl_value_t *val = NULL;
  JL_GC_PUSH1(&val);
  val = (jl_value_t*)tm->func.linfo;
  JL_GC_POP();
}

static inline void look_at_args(jl_value_t **args) {
  look_at_value(args[1]);
  jl_value_t *val = NULL;
  JL_GC_PUSH1(&val);
  val = args[2];
  JL_GC_POP();
}

void pushargs_as_args()
{
  jl_value_t **args;
  JL_GC_PUSHARGS(args, 5);
  look_at_args(args);
  JL_GC_POP();
}

static jl_typemap_entry_t *call_cache[10] JL_GLOBALLY_ROOTED;
void global_array2() {
  jl_value_t *val = NULL;
  JL_GC_PUSH1(&val);
  val = (jl_value_t*)call_cache[1]->func.linfo;
  JL_GC_POP();
}

void global_array3() {
  jl_value_t *val = NULL;
  jl_typemap_entry_t *tm = NULL;
  tm = call_cache[1];
  val = (jl_value_t*)tm->func.linfo;
  look_at_value(val);
}

static inline void look_at_value2(jl_value_t *v) {
  look_at_value(v);
}
void mtable(jl_value_t *f) {
  look_at_value2((jl_value_t*)jl_gf_mtable(f));
  jl_value_t *val = NULL;
  JL_GC_PUSH1(&val);
  val = (jl_value_t*)jl_gf_mtable(f);
  JL_GC_POP();
}

void mtable2(jl_value_t **v) {
  jl_value_t *val = NULL;
  JL_GC_PUSH1(&val);
  val = (jl_value_t*)jl_gf_mtable(v[2]);
  JL_GC_POP();
}

void tparam0(jl_value_t *atype) {
   look_at_value(jl_tparam0(atype));
}

extern jl_value_t *global_atype JL_GLOBALLY_ROOTED;
void tparam0_global() {
   look_at_value(jl_tparam0(global_atype));
}

static jl_value_t *some_global JL_GLOBALLY_ROOTED;
void global_copy() {
    jl_value_t *local = NULL;
    jl_gc_safepoint();
    JL_GC_PUSH1(&local);
    local = some_global;
    some_global = NULL;
    jl_gc_safepoint();
    look_at_value(some_global);
    JL_GC_POP();
}

// Check that rooting the same value twice uses to oldest scope
void scopes() {
    jl_value_t *val = (jl_value_t*)jl_svec1(NULL);
    JL_GC_PUSH1(&val);
    jl_value_t *val2 = val;
    JL_GC_PUSH1(&val2);
    JL_GC_POP();
    jl_gc_safepoint();
    look_at_value(val);
    JL_GC_POP();
}

jl_module_t *propagation(jl_module_t *m JL_PROPAGATES_ROOT);
void module_member(jl_module_t *m)
{
    for(int i=(int)m->usings.len-1; i >= 0; --i) {
      jl_module_t *imp = (jl_module_t*)m->usings.items[i];
      jl_gc_safepoint();
      look_at_value((jl_value_t*)imp);
      jl_module_t *prop = propagation(imp);
      look_at_value((jl_value_t*)prop);
      JL_GC_PUSH1(&imp);
      jl_gc_safepoint();
      look_at_value((jl_value_t*)imp);
      JL_GC_POP();
    }
}

int type_type(jl_value_t *v) {
    return jl_is_type_type(jl_typeof(v));
}

/* TODO: BROKEN
void assoc_exact_broken(jl_value_t **args, size_t n, int8_t offs, size_t world) {
    jl_typemap_level_t *cache = jl_new_typemap_level();
    jl_typemap_assoc_exact(cache->any, args, n, offs, world); /expected-warning{{Passing non-rooted value as argument to function that may GC}}
}
*/

void assoc_exact_ok(jl_value_t *args1, jl_value_t **args, size_t n, int8_t offs, size_t world) {
    jl_typemap_level_t *cache = jl_new_typemap_level();
    JL_GC_PUSH1(&cache);
    jl_typemap_assoc_exact(cache->any, args1, args, n, offs, world);
    JL_GC_POP();
}

// jl_box_* special cases
void box_special_cases1(int i) {
    look_at_value(jl_box_long(i)); // expected-warning{{Passing non-rooted value as argument to function}}
                                   // expected-note@-1{{Passing non-rooted value as argument to function}}
                                   // expected-note@-2{{Started tracking value here}}
}

void box_special_cases2() {
    look_at_value(jl_box_long(0));
}

jl_value_t *alloc_something();
jl_value_t *boxed_something() {
  jl_value_t *val = alloc_something();
  return jl_box_long(jl_datatype_size(val));
}

jl_value_t *alloc_something();
void out_arg(jl_value_t **out JL_REQUIRE_ROOTED_SLOT)
{
    jl_value_t *val = alloc_something();
    JL_GC_PUSH1(&val);
    *out = val;
    JL_GC_POP();
}

void foo_out_arg()
{
    jl_value_t *val_slot = NULL;
    JL_GC_PUSH1(&val_slot);
    out_arg(&val_slot);
    look_at_value(val_slot);
    JL_GC_POP();
}

typedef struct _varbinding {
    jl_tvar_t *var;
    jl_value_t *lb;
    jl_value_t *ub;
} jl_varbinding_t;

extern void escape_vb(jl_varbinding_t **vb);
void stack_rooted(jl_value_t *lb JL_MAYBE_UNROOTED, jl_value_t *ub JL_MAYBE_UNROOTED) {
    jl_varbinding_t vb = { NULL, lb, ub };
    JL_GC_PUSH2(&vb.lb, &vb.ub);
    escape_vb(&vb);
    look_at_value(vb.lb);
    JL_GC_POP();
}

void JL_NORETURN throw_internal(jl_value_t *e JL_MAYBE_UNROOTED)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    ptls->sig_exception = e;
    jl_gc_unsafe_enter(ptls);
    look_at_value(e);
}
