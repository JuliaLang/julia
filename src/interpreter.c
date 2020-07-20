// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <stdlib.h>
#include <setjmp.h>
#ifdef _OS_WINDOWS_
#include <malloc.h>
#endif
#include "julia.h"
#include "julia_internal.h"
#include "builtin_proto.h"
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    jl_code_info_t *src; // contains the names and number of slots
    jl_method_instance_t *mi; // MethodInstance we're executing, or NULL if toplevel
    jl_module_t *module; // context for globals
    jl_value_t **locals; // slots for holding local slots and ssavalues
    jl_svec_t *sparam_vals; // method static parameters, if eval-ing a method body
    size_t ip; // Leak the currently-evaluating statement index to backtrace capture
    int preevaluation; // use special rules for pre-evaluating expressions (deprecated--only for ccall handling)
    int continue_at; // statement index to jump to after leaving exception handler (0 if none)
} interpreter_state;


// general alloca rules are incompatible on C and C++, so define a macro that deals with the difference
#ifdef __cplusplus
#define JL_CPPALLOCA(var,n)                                                         \
  var = (decltype(var))alloca((n))
#else
#define JL_CPPALLOCA(var,n)                                                         \
  JL_GCC_IGNORE_START("-Wc++-compat")                                               \
  var = alloca((n));                                                                \
  JL_GCC_IGNORE_STOP
#endif

#ifdef __clang_analyzer__

extern void JL_GC_ENABLEFRAME(interpreter_state*) JL_NOTSAFEPOINT;

// This is necessary, because otherwise the analyzer considers this undefined
// behavior and terminates the exploration
#define JL_GC_PUSHFRAME(frame,n)     \
  JL_CPPALLOCA(frame, sizeof(*frame)+((n) * sizeof(jl_value_t*)));                  \
  memset(&frame[1], 0, sizeof(void*) * n); \
  _JL_GC_PUSHARGS((jl_value_t**)&frame[1], n);

#else

#define JL_GC_ENCODE_PUSHFRAME(n)  ((((size_t)(n))<<2)|2)

#define JL_GC_PUSHFRAME(frame,n)                                                    \
  JL_CPPALLOCA(frame, sizeof(*frame)+(((n)+3)*sizeof(jl_value_t*)));                \
  ((void**)&frame[1])[0] = NULL;                                                    \
  ((void**)&frame[1])[1] = (void*)JL_GC_ENCODE_PUSHFRAME(n);                        \
  ((void**)&frame[1])[2] = jl_pgcstack;                                             \
  memset(&((void**)&frame[1])[3], 0, (n)*sizeof(jl_value_t*));                      \
  jl_pgcstack = (jl_gcframe_t*)&(((void**)&frame[1])[1])

// we define this separately so that we can populate the frame before we add it to the backtrace
// it's recommended to mark the containing function with NOINLINE, though not essential
#define JL_GC_ENABLEFRAME(frame) \
  ((void**)&frame[1])[0] = __builtin_frame_address(0);

#endif


static jl_value_t *eval_value(jl_value_t *e, interpreter_state *s);
static jl_value_t *eval_body(jl_array_t *stmts, interpreter_state *s, size_t ip, int toplevel);

int jl_is_toplevel_only_expr(jl_value_t *e);

// method definition form

static jl_value_t *eval_methoddef(jl_expr_t *ex, interpreter_state *s)
{
    jl_value_t **args = jl_array_ptr_data(ex->args);
    jl_sym_t *fname = (jl_sym_t*)args[0];
    jl_module_t *modu = s->module;
    if (jl_is_globalref(fname)) {
        modu = jl_globalref_mod(fname);
        fname = jl_globalref_name(fname);
    }
    assert(jl_expr_nargs(ex) != 1 || jl_is_symbol(fname));

    if (jl_is_symbol(fname)) {
        jl_value_t *bp_owner = (jl_value_t*)modu;
        jl_binding_t *b = jl_get_binding_for_method_def(modu, fname);
        jl_value_t **bp = &b->value;
        jl_value_t *gf = jl_generic_function_def(b->name, b->owner, bp, bp_owner, b);
        if (jl_expr_nargs(ex) == 1)
            return gf;
    }

    jl_value_t *atypes = NULL, *meth = NULL;
    JL_GC_PUSH2(&atypes, &meth);
    atypes = eval_value(args[1], s);
    meth = eval_value(args[2], s);
    jl_method_def((jl_svec_t*)atypes, (jl_code_info_t*)meth, s->module);
    JL_GC_POP();
    return jl_nothing;
}

// expression evaluator

static jl_value_t *do_call(jl_value_t **args, size_t nargs, interpreter_state *s)
{
    jl_value_t **argv;
    assert(nargs >= 1);
    JL_GC_PUSHARGS(argv, nargs);
    size_t i;
    for (i = 0; i < nargs; i++)
        argv[i] = eval_value(args[i], s);
    jl_value_t *result = jl_apply(argv, nargs);
    JL_GC_POP();
    return result;
}

static jl_value_t *do_invoke(jl_value_t **args, size_t nargs, interpreter_state *s)
{
    jl_value_t **argv;
    assert(nargs >= 2);
    JL_GC_PUSHARGS(argv, nargs - 1);
    size_t i;
    for (i = 1; i < nargs; i++)
        argv[i - 1] = eval_value(args[i], s);
    jl_method_instance_t *meth = (jl_method_instance_t*)args[0];
    assert(jl_is_method_instance(meth));
    jl_value_t *result = jl_invoke(argv[1], &argv[2], nargs - 2, meth);
    JL_GC_POP();
    return result;
}

jl_value_t *jl_eval_global_var(jl_module_t *m, jl_sym_t *e)
{
    jl_value_t *v = jl_get_global(m, e);
    if (v == NULL)
        jl_undefined_var_error(e);
    return v;
}

static int jl_source_nslots(jl_code_info_t *src) JL_NOTSAFEPOINT
{
    return jl_array_len(src->slotflags);
}

static int jl_source_nssavalues(jl_code_info_t *src) JL_NOTSAFEPOINT
{
    return jl_is_long(src->ssavaluetypes) ? jl_unbox_long(src->ssavaluetypes) : jl_array_len(src->ssavaluetypes);
}

static void eval_stmt_value(jl_value_t *stmt, interpreter_state *s)
{
    jl_value_t *res = eval_value(stmt, s);
    s->locals[jl_source_nslots(s->src) + s->ip] = res;
}

static jl_value_t *eval_value(jl_value_t *e, interpreter_state *s)
{
    jl_code_info_t *src = s->src;
    if (jl_is_ssavalue(e)) {
        ssize_t id = ((jl_ssavalue_t*)e)->id - 1;
        if (src == NULL || id >= jl_source_nssavalues(src) || id < 0 || s->locals == NULL)
            jl_error("access to invalid SSAValue");
        else
            return s->locals[jl_source_nslots(src) + id];
    }
    if (jl_is_slot(e) || jl_is_argument(e)) {
        ssize_t n = jl_slot_number(e);
        if (src == NULL || n > jl_source_nslots(src) || n < 1 || s->locals == NULL)
            jl_error("access to invalid slot number");
        jl_value_t *v = s->locals[n - 1];
        if (v == NULL)
            jl_undefined_var_error((jl_sym_t*)jl_array_ptr_ref(src->slotnames, n - 1));
        return v;
    }
    if (jl_is_quotenode(e)) {
        return jl_quotenode_value(e);
    }
    if (jl_is_globalref(e)) {
        return jl_eval_global_var(jl_globalref_mod(e), jl_globalref_name(e));
    }
    if (jl_is_symbol(e)) {  // bare symbols appear in toplevel exprs not wrapped in `thunk`
        return jl_eval_global_var(s->module, (jl_sym_t*)e);
    }
    if (jl_is_pinode(e)) {
        jl_value_t *val = eval_value(jl_fieldref_noalloc(e, 0), s);
#ifndef JL_NDEBUG
        JL_GC_PUSH1(&val);
        jl_typeassert(val, jl_fieldref_noalloc(e, 1));
        JL_GC_POP();
#endif
        return val;
    }
    assert(!jl_is_phinode(e) && !jl_is_phicnode(e) && !jl_is_upsilonnode(e) && "malformed IR");
    if (!jl_is_expr(e))
        return e;
    jl_expr_t *ex = (jl_expr_t*)e;
    jl_value_t **args = jl_array_ptr_data(ex->args);
    size_t nargs = jl_array_len(ex->args);
    jl_sym_t *head = ex->head;
    if (head == call_sym) {
        return do_call(args, nargs, s);
    }
    else if (head == invoke_sym) {
        return do_invoke(args, nargs, s);
    }
    else if (head == isdefined_sym) {
        jl_value_t *sym = args[0];
        int defined = 0;
        if (jl_is_slot(sym) || jl_is_argument(sym)) {
            ssize_t n = jl_slot_number(sym);
            if (src == NULL || n > jl_source_nslots(src) || n < 1 || s->locals == NULL)
                jl_error("access to invalid slot number");
            defined = s->locals[n - 1] != NULL;
        }
        else if (jl_is_globalref(sym)) {
            defined = jl_boundp(jl_globalref_mod(sym), jl_globalref_name(sym));
        }
        else if (jl_is_symbol(sym)) {
            defined = jl_boundp(s->module, (jl_sym_t*)sym);
        }
        else if (jl_is_expr(sym) && ((jl_expr_t*)sym)->head == static_parameter_sym) {
            ssize_t n = jl_unbox_long(jl_exprarg(sym, 0));
            assert(n > 0);
            if (s->sparam_vals && n <= jl_svec_len(s->sparam_vals)) {
                jl_value_t *sp = jl_svecref(s->sparam_vals, n - 1);
                defined = !jl_is_typevar(sp);
            }
            else {
                // static parameter val unknown needs to be an error for ccall
                jl_error("could not determine static parameter value");
            }
        }
        else {
            assert(0 && "malformed isdefined expression");
        }
        return defined ? jl_true : jl_false;
    }
    else if (head == throw_undef_if_not_sym) {
        jl_value_t *cond = eval_value(args[1], s);
        assert(jl_is_bool(cond));
        if (cond == jl_false) {
            jl_sym_t *var = (jl_sym_t*)args[0];
            if (var == getfield_undefref_sym)
                jl_throw(jl_undefref_exception);
            else
                jl_undefined_var_error(var);
        }
        return jl_nothing;
    }
    else if (head == new_sym) {
        jl_value_t **argv;
        JL_GC_PUSHARGS(argv, nargs);
        for (size_t i = 0; i < nargs; i++)
            argv[i] = eval_value(args[i], s);
        jl_value_t *v = jl_new_structv((jl_datatype_t*)argv[0], &argv[1], nargs - 1);
        JL_GC_POP();
        return v;
    }
    else if (head == splatnew_sym) {
        jl_value_t **argv;
        JL_GC_PUSHARGS(argv, 2);
        argv[0] = eval_value(args[0], s);
        argv[1] = eval_value(args[1], s);
        jl_value_t *v = jl_new_structt((jl_datatype_t*)argv[0], argv[1]);
        JL_GC_POP();
        return v;
    }
    else if (head == static_parameter_sym) {
        ssize_t n = jl_unbox_long(args[0]);
        assert(n > 0);
        if (s->sparam_vals && n <= jl_svec_len(s->sparam_vals)) {
            jl_value_t *sp = jl_svecref(s->sparam_vals, n - 1);
            if (jl_is_typevar(sp) && !s->preevaluation)
                jl_undefined_var_error(((jl_tvar_t*)sp)->name);
            return sp;
        }
        // static parameter val unknown needs to be an error for ccall
        jl_error("could not determine static parameter value");
    }
    else if (head == copyast_sym) {
        return jl_copy_ast(eval_value(args[0], s));
    }
    else if (head == exc_sym) {
        return jl_current_exception();
    }
    else if (head == boundscheck_sym) {
        return jl_true;
    }
    else if (head == meta_sym || head == coverageeffect_sym || head == inbounds_sym || head == loopinfo_sym) {
        return jl_nothing;
    }
    else if (head == gc_preserve_begin_sym || head == gc_preserve_end_sym) {
        // The interpreter generally keeps values that were assigned in this scope
        // rooted. If the interpreter learns to be more aggressive here, we may
        // want to explicitly root these values.
        return jl_nothing;
    }
    else if (head == method_sym && nargs == 1) {
        return eval_methoddef(ex, s);
    }
    jl_errorf("unsupported or misplaced expression %s", jl_symbol_name(head));
    abort();
}

// phi nodes don't behave like proper instructions, so we require a special interpreter to handle them
static size_t eval_phi(jl_array_t *stmts, interpreter_state *s, size_t ns, size_t to)
{
    size_t from = s->ip;
    size_t ip = to;
    unsigned nphi = 0;
    for (ip = to; ip < ns; ip++) {
        jl_value_t *e = jl_array_ptr_ref(stmts, ip);
        if (!jl_is_phinode(e))
            break;
        nphi += 1;
    }
    if (nphi) {
        jl_value_t **dest = &s->locals[jl_source_nslots(s->src) + to];
        jl_value_t **phis; // = (jl_value_t**)alloca(sizeof(jl_value_t*) * nphi);
        JL_GC_PUSHARGS(phis, nphi);
        for (unsigned i = 0; i < nphi; i++) {
            jl_value_t *e = jl_array_ptr_ref(stmts, to + i);
            assert(jl_is_phinode(e));
            jl_array_t *edges = (jl_array_t*)jl_fieldref_noalloc(e, 0);
            ssize_t edge = -1;
            size_t closest = to; // implicit edge has `to <= edge - 1 < to + i`
            // this is because we could see the following IR (all 1-indexed):
            //   goto %3 unless %cond
            //   %2 = phi ...
            //   %3 = phi (1)[1 => %a], (2)[2 => %b]
            // from = 1, to = closest = 2, i = 1 --> edge = 2, edge_from = 2, from = 2
            for (unsigned j = 0; j < jl_array_len(edges); ++j) {
                size_t edge_from = jl_unbox_long(jl_arrayref(edges, j)); // 1-indexed
                if (edge_from == from + 1) {
                    if (edge == -1)
                        edge = j;
                }
                else if (closest < edge_from && edge_from < (to + i + 1)) {
                    // if we found a nearer implicit branch from fall-through,
                    // that occurred since the last explicit branch,
                    // we should use the value from that edge instead
                    edge = j;
                    closest = edge_from;
                }
            }
            jl_value_t *val = NULL;
            unsigned n_oldphi = closest - to;
            if (n_oldphi) {
                // promote this implicit branch to a basic block start
                // and move all phi values to their position in edges
                // note that we might have already processed some phi nodes
                // in this basic block, so we need to be extra careful here
                // to ignore those
                for (unsigned j = 0; j < n_oldphi; j++) {
                    dest[j] = phis[j];
                }
                for (unsigned j = n_oldphi; j < i; j++) {
                    // move the rest to the start of phis
                    phis[j - n_oldphi] = phis[j];
                    phis[j] = NULL;
                }
                from = closest - 1;
                i -= n_oldphi;
                dest += n_oldphi;
                to += n_oldphi;
                nphi -= n_oldphi;
            }
            if (edge != -1) {
                // if edges list doesn't contain last branch, or the value is explicitly undefined
                // then this value should be unused.
                jl_array_t *values = (jl_array_t*)jl_fieldref_noalloc(e, 1);
                val = jl_array_ptr_ref(values, edge);
                if (val)
                    val = eval_value(val, s);
            }
            phis[i] = val;
        }
        // now move all phi values to their position in edges
        for (unsigned j = 0; j < nphi; j++) {
            dest[j] = phis[j];
        }
        JL_GC_POP();
    }
    return ip;
}

static jl_value_t *eval_body(jl_array_t *stmts, interpreter_state *s, size_t ip, int toplevel)
{
    jl_handler_t __eh;
    size_t ns = jl_array_len(stmts);

    while (1) {
        s->ip = ip;
        if (ip >= ns)
            jl_error("`body` expression must terminate in `return`. Use `block` instead.");
        if (toplevel)
            jl_get_ptls_states()->world_age = jl_world_counter;
        jl_value_t *stmt = jl_array_ptr_ref(stmts, ip);
        assert(!jl_is_phinode(stmt));
        size_t next_ip = ip + 1;
        assert(!jl_is_phinode(stmt) && !jl_is_phicnode(stmt) && "malformed IR");
        if (jl_is_gotonode(stmt)) {
            next_ip = jl_gotonode_label(stmt) - 1;
        }
        else if (jl_is_gotoifnot(stmt)) {
            jl_value_t *cond = eval_value(jl_gotoifnot_cond(stmt), s);
            if (cond == jl_false) {
                next_ip = jl_gotoifnot_label(stmt) - 1;
            }
            else if (cond != jl_true) {
                jl_type_error("if", (jl_value_t*)jl_bool_type, cond);
            }
        }
        else if (jl_is_returnnode(stmt)) {
            return eval_value(jl_returnnode_value(stmt), s);
        }
        else if (jl_is_upsilonnode(stmt)) {
            jl_value_t *val = jl_fieldref_noalloc(stmt, 0);
            if (val)
                val = eval_value(val, s);
            jl_value_t *phic = s->locals[jl_source_nslots(s->src) + ip];
            assert(jl_is_ssavalue(phic));
            ssize_t id = ((jl_ssavalue_t*)phic)->id - 1;
            s->locals[jl_source_nslots(s->src) + id] = val;
        }
        else if (jl_is_expr(stmt)) {
            // Most exprs are allowed to end a BB by fall through
            jl_sym_t *head = ((jl_expr_t*)stmt)->head;
            if (head == assign_sym) {
                jl_value_t *lhs = jl_exprarg(stmt, 0);
                jl_value_t *rhs = eval_value(jl_exprarg(stmt, 1), s);
                if (jl_is_slot(lhs)) {
                    ssize_t n = jl_slot_number(lhs);
                    assert(n <= jl_source_nslots(s->src) && n > 0);
                    s->locals[n - 1] = rhs;
                }
                else {
                    jl_module_t *modu;
                    jl_sym_t *sym;
                    if (jl_is_globalref(lhs)) {
                        modu = jl_globalref_mod(lhs);
                        sym = jl_globalref_name(lhs);
                    }
                    else {
                        assert(jl_is_symbol(lhs));
                        modu = s->module;
                        sym = (jl_sym_t*)lhs;
                    }
                    JL_GC_PUSH1(&rhs);
                    jl_binding_t *b = jl_get_binding_wr(modu, sym, 1);
                    jl_checked_assignment(b, rhs);
                    JL_GC_POP();
                }
            }
            else if (head == enter_sym) {
                jl_enter_handler(&__eh);
                // This is a bit tricky, but supports the implementation of PhiC nodes.
                // They are conceptually slots, but the slot to store to doesn't get explicitly
                // mentioned in the store (aka the "UpsilonNode") (this makes them integrate more
                // nicely with the rest of the SSA representation). In a compiler, we would figure
                // out which slot to store to at compile time when we encounter the statement. We
                // can't quite do that here, but we do something similar: We scan the catch entry
                // block (the only place where PhiC nodes may occur) to find all the Upsilons we
                // can possibly encounter. Then, we remember which slot they store to (we abuse the
                // SSA value result array for this purpose). TODO: We could do this only the first
                // time we encounter a given enter.
                size_t catch_ip = jl_unbox_long(jl_exprarg(stmt, 0)) - 1;
                while (catch_ip < ns) {
                    jl_value_t *phicnode = jl_array_ptr_ref(stmts, catch_ip);
                    if (!jl_is_phicnode(phicnode))
                        break;
                    jl_array_t *values = (jl_array_t*)jl_fieldref_noalloc(phicnode, 0);
                    for (size_t i = 0; i < jl_array_len(values); ++i) {
                        jl_value_t *val = jl_array_ptr_ref(values, i);
                        assert(jl_is_ssavalue(val));
                        size_t upsilon = ((jl_ssavalue_t*)val)->id - 1;
                        assert(jl_is_upsilonnode(jl_array_ptr_ref(stmts, upsilon)));
                        s->locals[jl_source_nslots(s->src) + upsilon] = jl_box_ssavalue(catch_ip + 1);
                    }
                    s->locals[jl_source_nslots(s->src) + catch_ip] = NULL;
                    catch_ip += 1;
                }
                // store current top of exception stack for restore in pop_exception.
                s->locals[jl_source_nslots(s->src) + ip] = jl_box_ulong(jl_excstack_state());
                if (!jl_setjmp(__eh.eh_ctx, 1)) {
                    return eval_body(stmts, s, next_ip, toplevel);
                }
                else if (s->continue_at) { // means we reached a :leave expression
                    ip = s->continue_at;
                    s->continue_at = 0;
                    continue;
                }
                else { // a real exeception
                    ip = catch_ip;
                    continue;
                }
            }
            else if (head == leave_sym) {
                int hand_n_leave = jl_unbox_long(jl_exprarg(stmt, 0));
                assert(hand_n_leave > 0);
                // equivalent to jl_pop_handler(hand_n_leave), but retaining eh for longjmp:
                jl_ptls_t ptls = jl_get_ptls_states();
                jl_handler_t *eh = ptls->current_task->eh;
                while (--hand_n_leave > 0)
                    eh = eh->prev;
                jl_eh_restore_state(eh);
                // leave happens during normal control flow, but we must
                // longjmp to pop the eval_body call for each enter.
                s->continue_at = next_ip;
                jl_longjmp(eh->eh_ctx, 1);
            }
            else if (head == pop_exception_sym) {
                size_t prev_state = jl_unbox_ulong(eval_value(jl_exprarg(stmt, 0), s));
                jl_restore_excstack(prev_state);
            }
            else if (toplevel) {
                if (head == method_sym && jl_expr_nargs(stmt) > 1) {
                    eval_methoddef((jl_expr_t*)stmt, s);
                }
                else if (head == toplevel_sym) {
                    jl_value_t *res = jl_toplevel_eval(s->module, stmt);
                    s->locals[jl_source_nslots(s->src) + s->ip] = res;
                }
                else if (jl_is_toplevel_only_expr(stmt)) {
                    jl_toplevel_eval(s->module, stmt);
                }
                else if (head == meta_sym) {
                    if (jl_expr_nargs(stmt) == 1 && jl_exprarg(stmt, 0) == (jl_value_t*)nospecialize_sym) {
                        jl_set_module_nospecialize(s->module, 1);
                    }
                    if (jl_expr_nargs(stmt) == 1 && jl_exprarg(stmt, 0) == (jl_value_t*)specialize_sym) {
                        jl_set_module_nospecialize(s->module, 0);
                    }
                    if (jl_expr_nargs(stmt) == 2 && jl_exprarg(stmt, 0) == (jl_value_t*)optlevel_sym) {
                        if (jl_is_long(jl_exprarg(stmt, 1))) {
                            int n = jl_unbox_long(jl_exprarg(stmt, 1));
                            jl_set_module_optlevel(s->module, n);
                        }
                    }
                }
                else {
                    eval_stmt_value(stmt, s);
                }
            }
            else {
                eval_stmt_value(stmt, s);
            }
        }
        else if (jl_is_newvarnode(stmt)) {
            jl_value_t *var = jl_fieldref(stmt, 0);
            assert(jl_is_slot(var));
            ssize_t n = jl_slot_number(var);
            assert(n <= jl_source_nslots(s->src) && n > 0);
            s->locals[n - 1] = NULL;
        }
        else if (toplevel && jl_is_linenode(stmt)) {
            jl_lineno = jl_linenode_line(stmt);
        }
        else {
            eval_stmt_value(stmt, s);
        }
        ip = eval_phi(stmts, s, ns, next_ip);
    }
    abort();
}

// preparing method IR for interpreter

jl_code_info_t *jl_code_for_interpreter(jl_method_instance_t *mi)
{
    jl_code_info_t *src = (jl_code_info_t*)mi->uninferred;
    if (jl_is_method(mi->def.value)) {
        if (!src || (jl_value_t*)src == jl_nothing) {
            if (mi->def.method->source) {
                src = (jl_code_info_t*)mi->def.method->source;
            }
            else {
                assert(mi->def.method->generator);
                src = jl_code_for_staged(mi);
            }
        }
        if (src && (jl_value_t*)src != jl_nothing) {
            JL_GC_PUSH1(&src);
            src = jl_uncompress_ir(mi->def.method, NULL, (jl_array_t*)src);
            mi->uninferred = (jl_value_t*)src;
            jl_gc_wb(mi, src);
            JL_GC_POP();
        }
    }
    if (!src || !jl_is_code_info(src)) {
        jl_error("source missing for method called in interpreter");
    }
    return src;
}

// interpreter entry points

jl_value_t *NOINLINE jl_fptr_interpret_call(jl_value_t *f, jl_value_t **args, uint32_t nargs, jl_code_instance_t *codeinst)
{
    interpreter_state *s;
    jl_method_instance_t *mi = codeinst->def;
    jl_code_info_t *src = jl_code_for_interpreter(mi);
    jl_array_t *stmts = src->code;
    assert(jl_typeis(stmts, jl_array_any_type));
    unsigned nroots = jl_source_nslots(src) + jl_source_nssavalues(src) + 2;
    JL_GC_PUSHFRAME(s, nroots);
    jl_value_t **locals = (jl_value_t**)&s[1] + 3;
    locals[0] = (jl_value_t*)src;
    locals[1] = (jl_value_t*)stmts;
    s->locals = locals + 2;
    s->src = src;
    if (jl_is_module(mi->def.value)) {
        s->module = mi->def.module;
    }
    else {
        s->module = mi->def.method->module;
        size_t defargs = mi->def.method->nargs;
        int isva = mi->def.method->isva ? 1 : 0;
        size_t i;
        s->locals[0] = f;
        assert(isva ? nargs + 2 >= defargs : nargs + 1 == defargs);
        for (i = 1; i < defargs - isva; i++)
            s->locals[i] = args[i - 1];
        if (isva) {
            assert(defargs >= 2);
            s->locals[defargs - 1] = jl_f_tuple(NULL, &args[defargs - 2], nargs + 2 - defargs);
        }
    }
    s->sparam_vals = mi->sparam_vals;
    s->preevaluation = 0;
    s->continue_at = 0;
    s->mi = mi;
    JL_GC_ENABLEFRAME(s);
    jl_value_t *r = eval_body(stmts, s, 0, 0);
    JL_GC_POP();
    return r;
}

jl_value_t *NOINLINE jl_interpret_toplevel_thunk(jl_module_t *m, jl_code_info_t *src)
{
    interpreter_state *s;
    unsigned nroots = jl_source_nslots(src) + jl_source_nssavalues(src);
    JL_GC_PUSHFRAME(s, nroots);
    jl_array_t *stmts = src->code;
    assert(jl_typeis(stmts, jl_array_any_type));
    s->src = src;
    s->locals = (jl_value_t**)&s[1] + 3;
    s->module = m;
    s->sparam_vals = jl_emptysvec;
    s->continue_at = 0;
    s->mi = NULL;
    JL_GC_ENABLEFRAME(s);
    size_t last_age = jl_get_ptls_states()->world_age;
    jl_value_t *r = eval_body(stmts, s, 0, 1);
    jl_get_ptls_states()->world_age = last_age;
    JL_GC_POP();
    return r;
}

// deprecated: do not use this method in new code
// it uses special scoping / evaluation / error rules
// which should instead be handled in lowering
jl_value_t *NOINLINE jl_interpret_toplevel_expr_in(jl_module_t *m, jl_value_t *e, jl_code_info_t *src, jl_svec_t *sparam_vals)
{
    interpreter_state *s;
    JL_GC_PUSHFRAME(s, 0);
    s->src = src;
    s->module = m;
    s->sparam_vals = sparam_vals;
    s->preevaluation = (sparam_vals != NULL);
    s->continue_at = 0;
    s->mi = NULL;
    JL_GC_ENABLEFRAME(s);
    jl_value_t *v = eval_value(e, s);
    assert(v);
    JL_GC_POP();
    return v;
}

JL_DLLEXPORT size_t jl_capture_interp_frame(jl_bt_element_t *bt_entry,
        void *stateend, size_t space_remaining)
{
    interpreter_state *s = &((interpreter_state*)stateend)[-1];
    int need_module = !s->mi;
    int required_space = need_module ? 4 : 3;
    if (space_remaining < required_space)
        return 0; // Should not happen
    size_t njlvalues = need_module ? 2 : 1;
    uintptr_t entry_tags = jl_bt_entry_descriptor(njlvalues, 0, JL_BT_INTERP_FRAME_TAG, s->ip);
    bt_entry[0].uintptr = JL_BT_NON_PTR_ENTRY;
    bt_entry[1].uintptr = entry_tags;
    bt_entry[2].jlvalue = s->mi  ? (jl_value_t*)s->mi  :
                          s->src ? (jl_value_t*)s->src : (jl_value_t*)jl_nothing;
    if (need_module) {
        // If we only have a CodeInfo (s->src), we are in a top level thunk and
        // need to record the module separately.
        bt_entry[3].jlvalue = (jl_value_t*)s->module;
    }
    return required_space;
}


#ifdef __cplusplus
}
#endif
