// This file is a part of Julia. License is MIT: https://julialang.org/license

// RUN: clang -D__clang_gcanalyzer__ --analyze -Xanalyzer -analyzer-output=text -Xclang -load -Xclang libGCCheckerPlugin%shlibext -I%julia_home/src -I%julia_home/src/support -I%julia_home/usr/include ${CLANGSA_FLAGS} ${CLANGSA_CXXFLAGS} ${CPPFLAGS} ${CFLAGS} -Xclang -analyzer-checker=core,julia.GCChecker --analyzer-no-default-checks -Xclang -verify -x c %s

#include "julia.h"
#include "julia_internal.h"

extern void look_at_value(jl_value_t *v);
extern void process_unrooted(jl_value_t *maybe_unrooted JL_MAYBE_UNROOTED);
extern void jl_gc_safepoint();
extern void jl_gc_push_arraylist(jl_ptls_t ptls, arraylist_t *list) JL_NOTSAFEPOINT;

void unrooted_argument() {
    look_at_value((jl_value_t*)jl_svec1(NULL)); // expected-warning{{Passing non-rooted value as argument to function that may GC}}
                                                // expected-note@-1{{Passing non-rooted value as argument to function}}
                                                // expected-note@-2{{Started tracking value here}}
};

void simple_svec() {
    // This is ok, because jl_svecref is non-allocating
    jl_svec_t *val = jl_svec1(NULL);
    assert(jl_svecref(val, 0) == NULL);
}

jl_value_t *simple_missing_root() {
    jl_svec_t *val = jl_svec1(NULL); // expected-note{{Started tracking value here}}
    jl_gc_safepoint(); // expected-note{{Value may have been GCed here}}
    return jl_svecref(val, 0); // expected-warning{{Argument value may have been GCed}}
                               // expected-note@-1{{Argument value may have been GCed}}
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
    look_at_value(*data); // expected-warning{{Creating derivative of value that may have been GCed}}
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
    look_at_value(data[0]); // expected-warning{{Creating derivative of value that may have been GCed}}
                            // expected-note@-1{{Creating derivative of value that may have been GCed}}
};


void root_value_data3() {
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
    JL_GC_PUSH1(&val); // expected-note{{GC frame changed here}}
    val = jl_svec1(NULL); // expected-note{{Started tracking value here}}
                          // expected-note@-1{{Value was rooted here}}
    JL_GC_POP(); // expected-note{{GC frame changed here}}
                 // expected-note@-1{{Root was released here}}
    jl_gc_safepoint(); // expected-note{{Value may have been GCed here}}
    jl_value_t *ret = jl_svecref(val, 0); // expected-warning{{Argument value may have been GCed}}
                                          // expected-note@-1{{Argument value may have been GCed}}
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

// Root array slots remain roots when accessed through saved slot pointers.
int pushargs_slot_pointer_roots() {
  jl_value_t **margs;
  jl_svec_t *val = jl_svec1(NULL);
  JL_GC_PUSHARGS(margs, 2);
  margs[1] = (jl_value_t*)val;
  jl_value_t **slot = &margs[1];
  jl_gc_safepoint();
  look_at_value(*slot);
  JL_GC_POP();
  return 0;
}

int pushargs_slot_pointer_assignment_roots() {
  jl_value_t **margs;
  jl_svec_t *val = jl_svec1(NULL);
  JL_GC_PUSHARGS(margs, 2);
  jl_value_t **slot = &margs[1];
  *slot = (jl_value_t*)val;
  jl_gc_safepoint();
  look_at_value(val);
  JL_GC_POP();
  return 0;
}

int pushargs_slot_pointer_after_pop_freed() {
  jl_value_t **margs;
  jl_svec_t *val = jl_svec1(NULL); // expected-note{{Started tracking value here}}
  JL_GC_PUSHARGS(margs, 1); // expected-note{{GC frame changed here}}
  jl_value_t **slot = &margs[0];
  *slot = (jl_value_t*)val; // expected-note{{Value was rooted here}}
  JL_GC_POP(); // expected-note{{GC frame changed here}}
               // expected-note@-1{{Root was released here}}
  jl_gc_safepoint(); // expected-note{{Value may have been GCed here}}
  return val->length == 1; // expected-warning{{Trying to access value which may have been GCed}}
                           // expected-note@-1{{Trying to access value which may have been GCed}}
}

int scalar_root_slot_pointer_assignment_roots() {
  jl_svec_t *val = jl_svec1(NULL);
  jl_value_t *root = NULL;
  JL_GC_PUSH1(&root);
  jl_value_t **slot = &root;
  *slot = (jl_value_t*)val;
  jl_gc_safepoint();
  look_at_value((jl_value_t*)val);
  JL_GC_POP();
  return 0;
}

int scalar_root_overwrite_releases_old_value() {
  jl_svec_t *root = jl_svec1(NULL); // expected-note{{Started tracking value here}}
  jl_svec_t *alias = root;
  JL_GC_PUSH1(&root); // expected-note{{GC frame changed here}}
                      // expected-note@-1{{Value was rooted here}}
  root = NULL; // expected-note{{Root was released here}}
  jl_gc_safepoint(); // expected-note{{Value may have been GCed here}}
  return alias->length == 1; // expected-warning{{Trying to access value which may have been GCed}}
                             // expected-note@-1{{Trying to access value which may have been GCed}}
}

int permanent_root_slot_overwrite_is_conservative(jl_value_t **slot JL_REQUIRE_ROOTED_SLOT) {
  jl_svec_t *val = jl_svec1(NULL);
  jl_svec_t *alias = val;
  *slot = (jl_value_t*)val;
  *slot = NULL;
  jl_gc_safepoint();
  return alias->length == 1;
}

void rooted_slot_svecref_keeps_child_live(jl_svec_t **slot JL_REQUIRE_ROOTED_SLOT) {
  jl_value_t *param = jl_svecref(*slot, 0);
  jl_gc_safepoint();
  look_at_value(param);
}

int multiple_root_overwrite_keeps_value_rooted() {
  jl_svec_t *val = jl_svec1(NULL);
  jl_value_t *root1 = NULL;
  jl_value_t *root2 = NULL;
  JL_GC_PUSH2(&root1, &root2);
  root1 = (jl_value_t*)val;
  root2 = (jl_value_t*)val;
  root1 = NULL;
  jl_gc_safepoint();
  look_at_value((jl_value_t*)val);
  JL_GC_POP();
  return 0;
}

// Storing an already rooted value into a rooted object should not replace the
// value's existing longer-lived root.
void rooted_field_store_preserves_existing_root(jl_typename_t *tn) {
  jl_datatype_t *dt = NULL;
  JL_GC_PUSH1(&dt);
  dt = (jl_datatype_t*)jl_svec1(NULL);
  dt->name = tn;
  JL_GC_POP();
  jl_gc_safepoint();
  look_at_value((jl_value_t*)tn);
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

int arraylist_push_pop_releases_items(jl_ptls_t ptls) {
  jl_svec_t *val = jl_svec1(NULL); // expected-note{{Started tracking value here}}
  arraylist_t list = {3, 3, (void**)&val, {NULL}};
  jl_gc_push_arraylist(ptls, &list); // expected-note{{GC frame changed here}}
                                     // expected-note@-1{{Value was rooted here}}
  jl_gc_safepoint();
  look_at_value((jl_value_t*)val);
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
extern jl_expr_t *new_expr_for_analyzer(void);
void root_propagation(jl_expr_t *expr) {
  jl_value_t *val = first_array_elem(expr->args);
  jl_gc_safepoint();
  look_at_value(val);
}

void rooted_argument_derivatives_keep_roots(jl_value_t *v) {
  jl_value_t **data = (jl_value_t**)v;
  jl_value_t *first = data[0];
  jl_value_t *second = data[1];
  jl_gc_safepoint();
  look_at_value(first);
  look_at_value(second);
}

void rooted_argument_cast_derivative_keeps_root(jl_value_t *v) {
  jl_sym_t **data = (jl_sym_t**)v;
  jl_sym_t *sym = data[0];
  jl_gc_safepoint();
  look_at_value((jl_value_t*)sym);
}

void rooted_svec_data_pointer_arithmetic_keeps_root(jl_svec_t *cache) {
  jl_value_t **data = jl_svec_data(cache);
  jl_value_t *val = data[0];
  jl_gc_safepoint();
  look_at_value(val);
}

static jl_value_t *rooted_svec_data_inlined_helper(jl_svec_t *cache) {
  jl_value_t **data = jl_svec_data(cache);
  return data[0];
}

void rooted_svec_data_inlined_call_keeps_root(jl_svec_t *cache) {
  jl_svec_t *local = cache;
  jl_value_t *val = rooted_svec_data_inlined_helper(local);
  jl_gc_safepoint();
  look_at_value(val);
}

void rooted_exprarg_siblings_keep_roots(jl_expr_t *warning) {
  JL_GC_PUSH1(&warning);
  jl_value_t *level = jl_exprarg(warning, 0);
  jl_value_t *group = jl_exprarg(warning, 1);
  (void)group;
  jl_value_t *kwargs = jl_alloc_vec_any(0);
  (void)kwargs;
  look_at_value(level);
  JL_GC_POP();
}

void rooted_returned_exprarg_siblings_keep_roots(void) {
  jl_expr_t *warning = new_expr_for_analyzer();
  JL_GC_PUSH1(&warning);
  jl_value_t *level = jl_exprarg(warning, 0);
  jl_value_t *group = jl_exprarg(warning, 1);
  (void)group;
  jl_value_t *kwargs = jl_alloc_vec_any(0);
  (void)kwargs;
  look_at_value(level);
  JL_GC_POP();
}

static inline jl_value_t *root_array_outparam_value(jl_value_t **out) {
  jl_value_t **margs;
  JL_GC_PUSHARGS(margs, 1);
  margs[0] = (jl_value_t*)new_expr_for_analyzer();
  *out = margs[0];
  JL_GC_POP();
  return margs[0];
}

void root_outparam_after_safepointing_call(void) {
  jl_value_t *out = NULL;
  jl_value_t *result = root_array_outparam_value(&out);
  (void)result;
  JL_GC_PUSH1(&out);
  JL_GC_POP();
}

static inline void rooted_outparam_value(jl_value_t **out) {
  jl_svec_t *v = jl_svec1(NULL);
  JL_GC_PUSH1(&v);
  *out = (jl_value_t*)v;
  JL_GC_POP();
}

void root_outparam_after_inlined_call(void) {
  jl_value_t *out = NULL;
  rooted_outparam_value(&out);
  JL_GC_PUSH1(&out);
  JL_GC_POP();
}

void apply_single_rooted_value(void) {
  jl_value_t *f = jl_svec1(NULL);
  JL_GC_PUSH1(&f);
  (void)jl_apply(&f, 1);
  JL_GC_POP();
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

void rooted_svec_parent_keeps_child(void) {
  jl_value_t *child = (jl_value_t*)new_expr_for_analyzer();
  jl_value_t *alias = child;
  jl_svec_t *parent = NULL;
  JL_GC_PUSH2(&child, &parent);
  parent = jl_svec1(child);
  child = NULL;
  jl_gc_safepoint();
  look_at_value(alias);
  JL_GC_POP();
}

void rooted_svec_data_element_store_keeps_child(void) {
  jl_value_t *child = (jl_value_t*)new_expr_for_analyzer();
  jl_value_t *alias = child;
  jl_svec_t *parent = NULL;
  JL_GC_PUSH2(&child, &parent);
  parent = jl_svec1(NULL);
  jl_value_t **data = jl_svec_data(parent);
  data[0] = child;
  child = NULL;
  jl_gc_safepoint();
  look_at_value(alias);
  JL_GC_POP();
}

void rooted_svec_clear_releases_child(void) {
  jl_value_t *child = (jl_value_t*)new_expr_for_analyzer(); // expected-note{{Started tracking value here}}
  jl_value_t *alias = child;
  jl_svec_t *parent = NULL;
  JL_GC_PUSH2(&child, &parent); // expected-note{{GC frame changed here}}
                                // expected-note@-1{{Value was rooted here}}
  parent = jl_svec1(child);
  child = NULL;
  jl_svecset(parent, 0, NULL); // expected-note{{Root was released here}}
  jl_gc_safepoint(); // expected-note{{Value may have been GCed here}}
  look_at_value(alias); // expected-warning{{Argument value may have been GCed}}
                        // expected-note@-1{{Argument value may have been GCed}}
  JL_GC_POP();
}

void rooted_svecref_alias_clear_releases_child(void) {
  jl_value_t *child = (jl_value_t*)new_expr_for_analyzer();
  jl_value_t *alias = NULL;
  jl_svec_t *parent = NULL;
  JL_GC_PUSH2(&child, &parent); // expected-note{{GC frame changed here}}
  parent = jl_svec1(child);
  alias = jl_svecref(parent, 0); // expected-note{{Started tracking value here (root was inherited)}}
  child = NULL;
  jl_svecset(parent, 0, NULL); // expected-note{{Root was released here}}
  jl_gc_safepoint(); // expected-note{{Value may have been GCed here}}
  look_at_value(alias); // expected-warning{{Argument value may have been GCed}}
                        // expected-note@-1{{Argument value may have been GCed}}
  JL_GC_POP();
}

void rooted_svec_replace_releases_old_child(void) {
  jl_value_t *old_child = (jl_value_t*)new_expr_for_analyzer(); // expected-note{{Started tracking value here}}
  jl_value_t *old_alias = old_child;
  jl_value_t *new_child = NULL;
  jl_value_t *new_alias = NULL;
  jl_svec_t *parent = NULL;
  JL_GC_PUSH3(&old_child, &new_child, &parent); // expected-note{{GC frame changed here}}
                                                // expected-note@-1{{Value was rooted here}}
  parent = jl_svec1(old_child);
  old_child = NULL;
  new_child = (jl_value_t*)new_expr_for_analyzer();
  new_alias = new_child;
  jl_svecset(parent, 0, new_child); // expected-note{{Root was released here}}
  new_child = NULL;
  jl_gc_safepoint(); // expected-note{{Value may have been GCed here}}
  look_at_value(new_alias);
  look_at_value(old_alias); // expected-warning{{Argument value may have been GCed}}
                            // expected-note@-1{{Argument value may have been GCed}}
  JL_GC_POP();
}

void symbolic_svec_replace_keeps_old_child_conservatively(size_t i) {
  jl_value_t *old_child = (jl_value_t*)new_expr_for_analyzer();
  jl_value_t *old_alias = old_child;
  jl_value_t *new_child = NULL;
  jl_svec_t *parent = NULL;
  JL_GC_PUSH3(&old_child, &new_child, &parent);
  parent = jl_svec2(NULL, NULL);
  jl_svecset(parent, i, old_child);
  old_child = NULL;
  new_child = (jl_value_t*)new_expr_for_analyzer();
  jl_svecset(parent, i, new_child);
  new_child = NULL;
  jl_gc_safepoint();
  look_at_value(old_alias);
  JL_GC_POP();
}

// A root slot assigned to a child object keeps that child live even if another
// incoming edge from an unrooted container is later invalidated.
void rooted_loaded_child_independent_of_container(void) {
  jl_svec_t *child = jl_svec1(NULL);
  jl_value_t *container = NULL;
  jl_value_t *root = NULL;
  JL_GC_PUSH3(&child, &container, &root);
  container = (jl_value_t*)jl_svec1(child);
  root = jl_svecref(container, 0);
  child = NULL;
  container = NULL;
  jl_gc_safepoint();
  (void)((jl_svec_t*)root)->length;
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

static jl_typemap_entry_t *this_call_cache[10] JL_GLOBALLY_ROOTED;
void global_array2() {
  jl_value_t *val = NULL;
  JL_GC_PUSH1(&val);
  val = (jl_value_t*)this_call_cache[1]->func.linfo;
  JL_GC_POP();
}

void global_array3() {
  jl_value_t *val = NULL;
  jl_typemap_entry_t *tm = NULL;
  tm = this_call_cache[1];
  val = (jl_value_t*)tm->func.linfo;
  look_at_value(val);
}

void nonconst_loads(jl_svec_t *v)
{
    size_t i = jl_svec_len(v);
    jl_method_instance_t **data = (jl_method_instance_t**)jl_svec_data(v);
    jl_method_instance_t *mi = data[i];
    look_at_value(mi->specTypes);
}

void nonconst_loads2()
{
    jl_svec_t *v = jl_svec1(NULL); // expected-note{{Started tracking value here}}
    size_t i = jl_svec_len(v);
    jl_method_instance_t **data = (jl_method_instance_t**)jl_svec_data(v);
    jl_method_instance_t *mi = data[i]; // expected-note{{No Root to propagate. Tracking}}
    look_at_value(mi->specTypes); //expected-warning{{Passing non-rooted value as argument to function that may GC}}
                                  //expected-note@-1{{Passing non-rooted value as argument to function that may GC}}
                                  //expected-note@-2{{No Root to propagate. Tracking}}
}

static inline void look_at_value2(jl_value_t *v) {
  look_at_value(v);
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
    for(int i=(int)m->usings.len-1; i >= 0; i -= 3) {
      jl_module_t *imp = propagation(m);
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

// declare
jl_typemap_level_t *jl_new_typemap_level(void);

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

// Rooted-by annotations create graph edges from the configured rooting argument.
extern void test_rooted_by_arg_store(jl_svec_t *holder JL_PROPAGATES_ROOT,
                                     size_t i, jl_value_t *value JL_ROOTED_BY_ARG_INDEXED(0, 1)) JL_NOTSAFEPOINT;
extern void test_rooted_by_arg_store_pair(jl_svec_t *holder JL_PROPAGATES_ROOT,
                                          size_t i, jl_value_t *first JL_ROOTED_BY_ARG_INDEXED(0, 1),
                                          jl_value_t *second JL_ROOTED_BY_ARG_INDEXED(0, 1)) JL_NOTSAFEPOINT;
extern void test_rooted_by_arg_store_with_flag(jl_svec_t *holder JL_PROPAGATES_ROOT,
                                               int flag,
                                               jl_value_t *value JL_ROOTED_BY_ARG(0),
                                               jl_sym_t *key) JL_NOTSAFEPOINT;
extern void test_out_rooted_by_arg(jl_method_instance_t *mi JL_PROPAGATES_ROOT,
                                   jl_code_instance_t **out JL_OUT_ROOTED_BY_ARG(0)) JL_NOTSAFEPOINT;
extern jl_genericmemoryref_t test_memoryref_from_memory(jl_genericmemory_t *mem JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT;

void rooted_by_arg_annotation_keeps_value_live(void)
{
    jl_svec_t *holder = NULL;
    JL_GC_PUSH1(&holder);
    holder = jl_svec1(NULL);
    jl_value_t *value = (jl_value_t*)jl_svec1(NULL);
    test_rooted_by_arg_store(holder, 0, value);
    jl_gc_safepoint();
    look_at_value(value);
    JL_GC_POP();
}

void rooted_by_arg_indexed_pair_keeps_both_values_live(void)
{
    jl_svec_t *holder = NULL;
    jl_value_t *first = NULL;
    jl_value_t *second = NULL;
    JL_GC_PUSH3(&holder, &first, &second);
    holder = jl_svec2(NULL, NULL);
    first = (jl_value_t*)jl_svec1(NULL);
    second = (jl_value_t*)jl_svec1(NULL);
    jl_value_t *first_alias = first;
    jl_value_t *second_alias = second;
    test_rooted_by_arg_store_pair(holder, 0, first, second);
    first = NULL;
    second = NULL;
    jl_gc_safepoint();
    look_at_value(first_alias);
    look_at_value(second_alias);
    JL_GC_POP();
}

void rooted_by_arg_unrelated_integer_does_not_replace_field(void)
{
    jl_svec_t *holder = NULL;
    jl_value_t *old_child = NULL;
    jl_value_t *new_child = NULL;
    JL_GC_PUSH3(&holder, &old_child, &new_child);
    old_child = (jl_value_t*)jl_svec1(NULL);
    new_child = (jl_value_t*)jl_svec1(NULL);
    holder = jl_svec1(old_child);
    jl_value_t *old_alias = old_child;
    old_child = NULL;
    test_rooted_by_arg_store_with_flag(holder, 0, new_child, NULL);
    jl_gc_safepoint();
    look_at_value(old_alias);
    JL_GC_POP();
}

void out_rooted_by_arg_annotation_keeps_value_live(jl_method_instance_t *mi)
{
    jl_code_instance_t *ci = NULL;
    test_out_rooted_by_arg(mi, &ci);
    jl_gc_safepoint();
    look_at_value((jl_value_t*)ci);
}

void memoryrefset_roots_value_through_memoryref_field(void)
{
    jl_genericmemory_t *mem = NULL;
    jl_value_t *value = NULL;
    JL_GC_PUSH2(&mem, &value);
    mem = jl_alloc_memory_any(1);
    jl_genericmemoryref_t ref = test_memoryref_from_memory(mem);
    value = (jl_value_t*)jl_svec1(NULL);
    jl_value_t *alias = value;
    jl_memoryrefset(ref, value, 0);
    value = NULL;
    jl_gc_safepoint();
    look_at_value(alias);
    JL_GC_POP();
}

void memoryrefset_roots_value_through_nested_memoryref(void)
{
    jl_genericmemory_t *mem = NULL;
    jl_value_t *value = NULL;
    JL_GC_PUSH2(&mem, &value);
    mem = jl_alloc_memory_any(1);
    value = (jl_value_t*)jl_svec1(NULL);
    jl_value_t *alias = value;
    jl_memoryrefset(test_memoryref_from_memory(mem), value, 1);
    value = NULL;
    jl_gc_safepoint();
    look_at_value(alias);
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

// These cover graph-root edges that are easy to lose during GCChecker refactors.
void argument_root_slot_store_keeps_value_live(jl_value_t **slot JL_REQUIRE_ROOTED_SLOT)
{
    jl_value_t *v = (jl_value_t*)jl_svec1(NULL);
    *slot = v;
    jl_gc_safepoint();
    look_at_value(v);
}

void unannotated_argument_buffer_store_does_not_root(jl_value_t **slot)
{
    jl_value_t *v = (jl_value_t*)jl_svec1(NULL); // expected-note{{Started tracking value here}}
    *slot = v;
    jl_gc_safepoint(); // expected-note{{Value may have been GCed here}}
    look_at_value(v); // expected-warning{{Argument value may have been GCed}}
                      // expected-note@-1{{Argument value may have been GCed}}
}

void annotated_argument_root_slot_accepts_null(jl_value_t **slot JL_REQUIRE_ROOTED_SLOT)
{
    *slot = NULL;
    jl_gc_safepoint();
}

void dynamic_root_array_element_keeps_value_live(size_t n)
{
    jl_value_t **margs;
    if (n == 0)
        n = 1;
    JL_GC_PUSHARGS(margs, n);
    margs[0] = (jl_value_t*)jl_svec1(NULL);
    jl_gc_safepoint();
    look_at_value(margs[0]);
    JL_GC_POP();
}

void dynamic_root_array_symbolic_element_keeps_value_live(size_t n)
{
    jl_value_t **margs;
    JL_GC_PUSHARGS(margs, n + 1);
    margs[n] = (jl_value_t*)jl_svec1(NULL);
    jl_gc_safepoint();
    look_at_value(margs[n]);
    JL_GC_POP();
}

void dynamic_root_array_copy_keeps_value_live(size_t n)
{
    jl_value_t **margs;
    JL_GC_PUSHARGS(margs, n + 1);
    margs[0] = (jl_value_t*)jl_svec1(NULL);
    margs[n] = margs[0];
    jl_gc_safepoint();
    look_at_value(margs[n]);
    JL_GC_POP();
}

extern jl_value_t *test_return_roots_argument(jl_value_t *v JL_ROOTED_BY_RETURN);
extern jl_value_t *test_return_does_not_root_argument(jl_value_t *v);
extern jl_value_t *test_return_roots_varargs(jl_datatype_t *type, ...) JL_ROOTED_VARARGS;
extern jl_value_t *test_return_does_not_root_varargs(jl_datatype_t *type, ...);
void rooted_by_return_keeps_argument_live(void)
{
    jl_value_t *v = (jl_value_t*)jl_svec1(NULL);
    jl_value_t *holder = v;
    JL_GC_PUSH1(&holder);
    holder = test_return_roots_argument(v);
    jl_gc_safepoint();
    look_at_value(v);
    JL_GC_POP();
}

void unannotated_return_does_not_root_fixed_argument(void)
{
    jl_value_t *v = (jl_value_t*)jl_svec1(NULL); // expected-note{{Started tracking value here}}
    jl_value_t *holder = v;
    JL_GC_PUSH1(&holder); // expected-note{{GC frame changed here}}
                           // expected-note@-1{{Value was rooted here}}
    holder = test_return_does_not_root_argument(v); // expected-note{{Root was released here}}
    jl_gc_safepoint(); // expected-note{{Value may have been GCed here}}
    look_at_value(v); // expected-warning{{Argument value may have been GCed}}
                      // expected-note@-1{{Argument value may have been GCed}}
    JL_GC_POP();
}

void rooted_by_return_keeps_varargs_live(void)
{
    jl_value_t *v = (jl_value_t*)jl_svec1(NULL);
    jl_value_t *holder = v;
    JL_GC_PUSH1(&holder);
    holder = test_return_roots_varargs(jl_quotenode_type, v);
    jl_gc_safepoint();
    look_at_value(v);
    JL_GC_POP();
}

void jl_svec_varargs_keep_fields_live(void)
{
    jl_value_t *v = (jl_value_t*)jl_svec1(NULL);
    jl_value_t *holder = v;
    JL_GC_PUSH1(&holder);
    holder = (jl_value_t*)jl_svec(1, v);
    jl_gc_safepoint();
    look_at_value(v);
    JL_GC_POP();
}

void rooted_by_return_does_not_implicitly_root_varargs(void)
{
    jl_value_t *v = (jl_value_t*)jl_svec1(NULL); // expected-note{{Started tracking value here}}
    jl_value_t *holder = v;
    JL_GC_PUSH1(&holder); // expected-note{{GC frame changed here}}
                           // expected-note@-1{{Value was rooted here}}
    holder = test_return_does_not_root_varargs(jl_quotenode_type, v); // expected-note{{Root was released here}}
    jl_gc_safepoint(); // expected-note{{Value may have been GCed here}}
    look_at_value(v); // expected-warning{{Argument value may have been GCed}}
                      // expected-note@-1{{Argument value may have been GCed}}
    JL_GC_POP();
}

void jl_new_struct_return_keeps_fields_live(void)
{
    jl_value_t *v = (jl_value_t*)jl_svec1(NULL);
    jl_value_t *holder = v;
    JL_GC_PUSH1(&holder);
    holder = jl_new_struct(jl_quotenode_type, v);
    jl_gc_safepoint();
    look_at_value(v);
    JL_GC_POP();
}

void promise_rooted_derived_load_keeps_value_live(jl_value_t **slot JL_MAYBE_UNROOTED)
{
    jl_value_t *v = *slot;
    JL_GC_PROMISE_ROOTED(v);
    jl_gc_safepoint();
    look_at_value(v);
}

void promise_rooted_accepts_null(void)
{
    jl_value_t *v = NULL;
    JL_GC_PROMISE_ROOTED(v);
    JL_GC_PROMISE_ROOTED(NULL);
}

extern jl_datatype_t *test_global_datatype JL_GLOBALLY_ROOTED;
void globally_rooted_field_derivative_keeps_value_live(void)
{
    jl_value_t *v = (jl_value_t*)test_global_datatype->super;
    jl_gc_safepoint();
    look_at_value(v);
}

extern _Atomic(jl_typemap_entry_t*) test_atomic_call_cache[10] JL_GLOBALLY_ROOTED;
extern int test_maybe_safepoint_match(jl_typemap_entry_t *entry JL_MAYBE_UNROOTED);
void globally_rooted_atomic_array_entry_keeps_value_live(size_t i)
{
    jl_typemap_entry_t *entry = jl_atomic_load_relaxed(&test_atomic_call_cache[i & 7]);
    if (entry && test_maybe_safepoint_match((jl_typemap_entry_t*)jl_svec_data(entry->sig->parameters)))
        (void)jl_atomic_load_relaxed(&entry->min_world);
}

void module_arraylist_carrier_reports_freed_parent(jl_module_t *m JL_MAYBE_UNROOTED) // expected-note{{Argument was annotated as MAYBE_UNROOTED}}
{
    arraylist_t *usings = &m->usings;
    jl_gc_safepoint(); // expected-note{{Value may have been GCed here}}
    (void)usings->len; // expected-warning{{Trying to access value which may have been GCed}}
                       // expected-note@-1{{Trying to access value which may have been GCed}}
}

typedef struct {
    jl_value_t **data;
} test_unknown_carrier_t;
void unknown_carrier_field_load_stays_untracked(void *opaque)
{
    test_unknown_carrier_t *env = (test_unknown_carrier_t*)opaque;
    jl_svec_t *v = (jl_svec_t*)env->data[0];
    jl_gc_safepoint();
    (void)v->length;
}

typedef struct test_alloca_carrier_t {
    jl_value_t *val;
    struct test_alloca_carrier_t *prev;
} test_alloca_carrier_t;
extern int test_alloca_carrier_may_safepoint(test_alloca_carrier_t *env JL_MAYBE_UNROOTED);
void alloca_carrier_pointer_is_not_gc_object(void)
{
    test_alloca_carrier_t *env = (test_alloca_carrier_t*)alloca(sizeof(test_alloca_carrier_t));
    env->val = NULL;
    env->prev = NULL;
    if (test_alloca_carrier_may_safepoint(env))
        (void)env->val;
}

JL_DLLEXPORT jl_value_t *jl_totally_used_function(int i)
{
    jl_value_t *v = jl_box_int32(i); // expected-note{{Started tracking value here}}
    jl_gc_safepoint(); // expected-note{{Value may have been GCed here}}
    return v; // expected-warning{{Return value may have been GCed}}
              // expected-note@-1{{Return value may have been GCed}}
}
