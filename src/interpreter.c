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
    jl_code_instance_t *ci; // CodeInstance we're executing (for generated functions)
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

#ifdef __clang_gcanalyzer__

extern void JL_GC_ENABLEFRAME(interpreter_state*) JL_NOTSAFEPOINT;

// This is necessary, because otherwise the analyzer considers this undefined
// behavior and terminates the exploration
#define JL_GC_PUSHFRAME(frame,locals,n)     \
  JL_CPPALLOCA(frame, sizeof(*frame)+((n) * sizeof(jl_value_t*)));  \
  memset(&frame[1], 0, sizeof(void*) * n); \
  _JL_GC_PUSHARGS((jl_value_t**)&frame[1], n); \
  locals = (jl_value_t**)&frame[1];

#else

#define JL_GC_ENCODE_PUSHFRAME(n)  ((((size_t)(n))<<2)|2)

#define JL_GC_PUSHFRAME(frame,locals,n)                                             \
  JL_CPPALLOCA(frame, sizeof(*frame)+(((n)+3)*sizeof(jl_value_t*)));                \
  ((void**)&frame[1])[0] = NULL;                                                    \
  ((void**)&frame[1])[1] = (void*)JL_GC_ENCODE_PUSHFRAME(n);                        \
  ((void**)&frame[1])[2] = jl_pgcstack;                                             \
  memset(&((void**)&frame[1])[3], 0, (n)*sizeof(jl_value_t*));                      \
  jl_pgcstack = (jl_gcframe_t*)&(((void**)&frame[1])[1]);                           \
  locals = &((jl_value_t**)&frame[1])[3];

// we define this separately so that we can populate the frame before we add it to the backtrace
// it's recommended to mark the containing function with NOINLINE, though not essential
#define JL_GC_ENABLEFRAME(frame) \
    jl_signal_fence(); \
    ((void**)&frame[1])[0] = __builtin_frame_address(0);

#endif


static jl_value_t *eval_value(jl_value_t *e, interpreter_state *s);
static jl_value_t *eval_body(jl_array_t *stmts, interpreter_state *s, size_t ip, int toplevel);

// method definition form

static jl_value_t *eval_methoddef(jl_expr_t *ex, interpreter_state *s)
{
    jl_value_t **args = jl_array_ptr_data(ex->args);

    // generic function definition
    if (jl_expr_nargs(ex) == 1) {
        jl_value_t **args = jl_array_ptr_data(ex->args);
        jl_sym_t *fname = (jl_sym_t*)args[0];
        jl_module_t *modu = s->module;
        if (jl_is_globalref(fname)) {
            modu = jl_globalref_mod(fname);
            fname = jl_globalref_name(fname);
        }
        if (!jl_is_symbol(fname)) {
            jl_error("method: invalid declaration");
        }
        return jl_declare_const_gf(modu, fname);
    }

    jl_value_t *atypes = NULL, *meth = NULL, *fname = NULL;
    JL_GC_PUSH3(&atypes, &meth, &fname);

    fname = eval_value(args[0], s);
    jl_methtable_t *mt = NULL;
    if (jl_is_mtable(fname))
        mt = (jl_methtable_t*)fname;
    atypes = eval_value(args[1], s);
    meth = eval_value(args[2], s);
    jl_method_t *ret = jl_method_def((jl_svec_t*)atypes, mt, (jl_code_info_t*)meth, s->module);
    JL_GC_POP();
    return (jl_value_t *)ret;
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
        argv[i-1] = eval_value(args[i], s);
    jl_value_t *c = args[0];
    assert(jl_is_code_instance(c) || jl_is_method_instance(c));
    jl_value_t *result = NULL;
    if (jl_is_code_instance(c)) {
        jl_code_instance_t *codeinst = (jl_code_instance_t*)c;
        assert(jl_atomic_load_relaxed(&codeinst->min_world) <= jl_current_task->world_age &&
               jl_current_task->world_age <= jl_atomic_load_relaxed(&codeinst->max_world));
        jl_callptr_t invoke = jl_atomic_load_acquire(&codeinst->invoke);
        if (!invoke) {
            jl_compile_codeinst(codeinst);
            invoke = jl_atomic_load_acquire(&codeinst->invoke);
        }
        if (invoke) {
            result = invoke(argv[0], nargs == 2 ? NULL : &argv[1], nargs - 2, codeinst);

        } else {
            if (codeinst->owner != jl_nothing) {
                jl_error("Failed to invoke or compile external codeinst");
            }
            result = jl_invoke(argv[0], nargs == 2 ? NULL : &argv[1], nargs - 2, jl_get_ci_mi(codeinst));
        }
    } else {
        result = jl_invoke(argv[0], nargs == 2 ? NULL : &argv[1], nargs - 2, (jl_method_instance_t*)c);
    }
    JL_GC_POP();
    return result;
}

jl_value_t *jl_eval_global_var(jl_module_t *m, jl_sym_t *e)
{
    jl_value_t *v = jl_get_global(m, e);
    if (v == NULL)
        jl_undefined_var_error(e, (jl_value_t*)m);
    return v;
}

jl_value_t *jl_eval_globalref(jl_globalref_t *g)
{
    jl_value_t *v = jl_get_globalref_value(g);
    if (v == NULL)
        jl_undefined_var_error(g->name, (jl_value_t*)g->mod);
    return v;
}

static int jl_source_nslots(jl_code_info_t *src) JL_NOTSAFEPOINT
{
    return jl_array_nrows(src->slotflags);
}

static int jl_source_nssavalues(jl_code_info_t *src) JL_NOTSAFEPOINT
{
    return jl_is_long(src->ssavaluetypes) ? jl_unbox_long(src->ssavaluetypes) : jl_array_nrows(src->ssavaluetypes);
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
    if (jl_is_slotnumber(e) || jl_is_argument(e)) {
        ssize_t n = jl_slot_number(e);
        if (src == NULL || n > jl_source_nslots(src) || n < 1 || s->locals == NULL)
            jl_error("access to invalid slot number");
        jl_value_t *v = s->locals[n - 1];
        if (v == NULL)
            jl_undefined_var_error((jl_sym_t*)jl_array_ptr_ref(src->slotnames, n - 1), (jl_value_t*)jl_local_sym);
        return v;
    }
    if (jl_is_quotenode(e)) {
        return jl_quotenode_value(e);
    }
    if (jl_is_globalref(e)) {
        return jl_eval_globalref((jl_globalref_t*)e);
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
    size_t nargs = jl_array_nrows(ex->args);
    jl_sym_t *head = ex->head;
    if (head == jl_call_sym) {
        return do_call(args, nargs, s);
    }
    else if (head == jl_invoke_sym) {
        return do_invoke(args, nargs, s);
    }
    else if (head == jl_invoke_modify_sym) {
        return do_call(args + 1, nargs - 1, s);
    }
    else if (head == jl_isdefined_sym) {
        jl_value_t *sym = args[0];
        int defined = 0;
        assert(nargs == 1 && "malformed IR");
        if (jl_is_slotnumber(sym) || jl_is_argument(sym)) {
            ssize_t n = jl_slot_number(sym);
            if (src == NULL || n > jl_source_nslots(src) || n < 1 || s->locals == NULL)
                jl_error("access to invalid slot number");
            defined = s->locals[n - 1] != NULL;
        }
        else if (jl_is_globalref(sym) || jl_is_symbol(sym)) {
            jl_error("[Internal Error]: :isdefined on globalref should use `isdefinedglobal`");
        }
        else if (jl_is_expr(sym) && ((jl_expr_t*)sym)->head == jl_static_parameter_sym) {
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
    else if (head == jl_throw_undef_if_not_sym) {
        jl_value_t *cond = eval_value(args[1], s);
        assert(jl_is_bool(cond));
        if (cond == jl_false) {
            jl_sym_t *var = (jl_sym_t*)args[0];
            if (var == jl_getfield_undefref_sym)
                jl_throw(jl_undefref_exception);
            else
                jl_undefined_var_error(var, (jl_value_t*)jl_local_sym);
        }
        return jl_nothing;
    }
    else if (head == jl_new_sym) {
        jl_value_t **argv;
        JL_GC_PUSHARGS(argv, nargs);
        for (size_t i = 0; i < nargs; i++)
            argv[i] = eval_value(args[i], s);
        jl_value_t *v = jl_new_structv((jl_datatype_t*)argv[0], &argv[1], nargs - 1);
        JL_GC_POP();
        return v;
    }
    else if (head == jl_splatnew_sym) {
        jl_value_t **argv;
        JL_GC_PUSHARGS(argv, 2);
        argv[0] = eval_value(args[0], s);
        argv[1] = eval_value(args[1], s);
        jl_value_t *v = jl_new_structt((jl_datatype_t*)argv[0], argv[1]);
        JL_GC_POP();
        return v;
    }
    else if (head == jl_new_opaque_closure_sym) {
        jl_value_t **argv;
        JL_GC_PUSHARGS(argv, nargs);
        for (size_t i = 0; i < nargs; i++)
            argv[i] = eval_value(args[i], s);
        JL_NARGSV(new_opaque_closure, 4);
        jl_value_t *ret = (jl_value_t*)jl_new_opaque_closure((jl_tupletype_t*)argv[0], argv[1], argv[2],
            argv[4], argv+5, nargs-5, 1);
        JL_GC_POP();
        return ret;
    }
    else if (head == jl_static_parameter_sym) {
        ssize_t n = jl_unbox_long(args[0]);
        assert(n > 0);
        if (s->sparam_vals && n <= jl_svec_len(s->sparam_vals)) {
            jl_value_t *sp = jl_svecref(s->sparam_vals, n - 1);
            if (jl_is_typevar(sp) && !s->preevaluation)
                jl_undefined_var_error(((jl_tvar_t*)sp)->name, (jl_value_t*)jl_static_parameter_sym);
            return sp;
        }
        // static parameter val unknown needs to be an error for ccall
        jl_error("could not determine static parameter value");
    }
    else if (head == jl_copyast_sym) {
        return jl_copy_ast(eval_value(args[0], s));
    }
    else if (head == jl_exc_sym) {
        return jl_current_exception(jl_current_task);
    }
    else if (head == jl_boundscheck_sym) {
        return jl_true;
    }
    else if (head == jl_meta_sym || head == jl_coverageeffect_sym || head == jl_inbounds_sym || head == jl_loopinfo_sym ||
             head == jl_aliasscope_sym || head == jl_popaliasscope_sym || head == jl_inline_sym || head == jl_noinline_sym) {
        return jl_nothing;
    }
    else if (head == jl_gc_preserve_begin_sym || head == jl_gc_preserve_end_sym) {
        // The interpreter generally keeps values that were assigned in this scope
        // rooted. If the interpreter learns to be more aggressive here, we may
        // want to explicitly root these values.
        return jl_nothing;
    }
    else if (head == jl_method_sym && nargs == 1) {
        return eval_methoddef(ex, s);
    }
    else if (head == jl_foreigncall_sym) {
        jl_error("`ccall` requires the compiler");
    }
    else if (head == jl_cfunction_sym) {
        jl_error("`cfunction` requires the compiler");
    }
    jl_errorf("unsupported or misplaced expression %s", jl_symbol_name(head));
    abort();
}

// phi nodes don't behave like proper instructions, so we require a special interpreter to handle them
static size_t eval_phi(jl_array_t *stmts, interpreter_state *s, size_t ns, size_t to)
{
    size_t from = s->ip;
    size_t ip = to;
    unsigned nphiblockstmts = 0;
    unsigned last_phi = 0;
    for (ip = to; ip < ns; ip++) {
        jl_value_t *e = jl_array_ptr_ref(stmts, ip);
        if (!jl_is_phinode(e)) {
            if (jl_is_expr(e) || jl_is_returnnode(e) || jl_is_gotoifnot(e) ||
                jl_is_gotonode(e) || jl_is_phicnode(e) || jl_is_upsilonnode(e) ||
                jl_is_ssavalue(e)) {
                break;
            }
            // Everything else is allowed in the phi-block for implementation
            // convenience - fall through.
        } else {
            last_phi = nphiblockstmts + 1;
        }
        nphiblockstmts += 1;
    }
    // Cut off the phi block at the last phi node. For global refs that are not
    // actually in the phi block, we want to evaluate them in the regular interpreter
    // loop instead to make sure exception state is set up properly in case they throw.
    nphiblockstmts = last_phi;
    ip = to + last_phi;
    if (nphiblockstmts) {
        jl_value_t **dest = &s->locals[jl_source_nslots(s->src) + to];
        jl_value_t **phis; // = (jl_value_t**)alloca(sizeof(jl_value_t*) * nphiblockstmts);
        JL_GC_PUSHARGS(phis, nphiblockstmts);
        for (unsigned i = 0; i < nphiblockstmts; i++) {
            jl_value_t *e = jl_array_ptr_ref(stmts, to + i);
            if (!jl_is_phinode(e)) {
                // IR verification guarantees that the only thing that gets
                // evaluated here are constants, so it doesn't matter if we
                // update the locals or the phis, but let's be consistent
                // for simplicity.
                phis[i] = eval_value(e, s);
                continue;
            }
            jl_array_t *edges = (jl_array_t*)jl_fieldref_noalloc(e, 0);
            ssize_t edge = -1;
            size_t closest = to; // implicit edge has `to <= edge - 1 < to + i`
            // this is because we could see the following IR (all 1-indexed):
            //   goto %3 unless %cond
            //   %2 = phi ...
            //   %3 = phi (1)[1 => %a], (2)[2 => %b]
            // from = 1, to = closest = 2, i = 1 --> edge = 2, edge_from = 2, from = 2
            for (unsigned j = 0; j < jl_array_nrows(edges); ++j) {
                size_t edge_from = jl_array_data(edges, int32_t)[j]; // 1-indexed
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
                nphiblockstmts -= n_oldphi;
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
        for (unsigned j = 0; j < nphiblockstmts; j++) {
            dest[j] = phis[j];
        }
        JL_GC_POP();
    }
    return ip;
}

static jl_value_t *eval_body(jl_array_t *stmts, interpreter_state *s, size_t ip, int toplevel)
{
    jl_handler_t __eh;
    size_t ns = jl_array_nrows(stmts);
    jl_task_t *ct = jl_current_task;

    while (1) {
        s->ip = ip;
        if (ip >= ns)
            jl_error("`body` expression must terminate in `return`. Use `block` instead.");
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
        else if (jl_is_enternode(stmt)) {
            jl_enter_handler(ct, &__eh);
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
            size_t catch_ip = jl_enternode_catch_dest(stmt);
            if (catch_ip) {
                catch_ip -= 1;
                while (catch_ip < ns) {
                    jl_value_t *phicnode = jl_array_ptr_ref(stmts, catch_ip);
                    if (!jl_is_phicnode(phicnode))
                        break;
                    jl_array_t *values = (jl_array_t*)jl_fieldref_noalloc(phicnode, 0);
                    for (size_t i = 0; i < jl_array_nrows(values); ++i) {
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
            }
            s->locals[jl_source_nslots(s->src) + ip] = jl_box_ulong(jl_excstack_state(ct));
            if (jl_enternode_scope(stmt)) {
                jl_value_t *scope = eval_value(jl_enternode_scope(stmt), s);
                // GC preserve the scope, since it is not rooted in the `jl_handler_t *`
                // and may be removed from jl_current_task by any nested block and then
                // replaced later
                JL_GC_PUSH1(&scope);
                ct->scope = scope;
                if (!jl_setjmp(__eh.eh_ctx, 1)) {
                    ct->eh = &__eh;
                    eval_body(stmts, s, next_ip, toplevel);
                    jl_unreachable();
                }
                JL_GC_POP();
            }
            else {
                if (!jl_setjmp(__eh.eh_ctx, 1)) {
                    ct->eh = &__eh;
                    eval_body(stmts, s, next_ip, toplevel);
                    jl_unreachable();
                }
            }

            if (s->continue_at) { // means we reached a :leave expression
                jl_eh_restore_state_noexcept(ct, &__eh);
                ip = s->continue_at;
                s->continue_at = 0;
                continue;
            }
            else { // a real exception
                jl_eh_restore_state(ct, &__eh);
                ip = catch_ip;
                assert(jl_enternode_catch_dest(stmt) != 0);
                continue;
            }
        }
        else if (jl_is_expr(stmt)) {
            // Most exprs are allowed to end a BB by fall through
            jl_sym_t *head = ((jl_expr_t*)stmt)->head;
            if (head == jl_assign_sym) {
                jl_value_t *lhs = jl_exprarg(stmt, 0);
                jl_value_t *rhs = eval_value(jl_exprarg(stmt, 1), s);
                if (jl_is_slotnumber(lhs)) {
                    ssize_t n = jl_slot_number(lhs);
                    assert(n <= jl_source_nslots(s->src) && n > 0);
                    s->locals[n - 1] = rhs;
                }
                else {
                    // This is an unmodeled error. Our frontend only generates
                    // legal `=` expressions, but since GlobalRef used to be legal
                    // here, give a loud error in case any package is modifying
                    // internals.
                    jl_error("Invalid IR: Assignment LHS not a Slot");
                }
            }
            else if (head == jl_leave_sym) {
                int hand_n_leave = 0;
                for (int i = 0; i < jl_expr_nargs(stmt); ++i) {
                    jl_value_t *arg = jl_exprarg(stmt, i);
                    if (arg == jl_nothing)
                        continue;
                    assert(jl_is_ssavalue(arg));
                    jl_value_t *enter_stmt = jl_array_ptr_ref(stmts, ((jl_ssavalue_t*)arg)->id - 1);
                    if (enter_stmt == jl_nothing)
                        continue;
                    hand_n_leave += 1;
                }
                if (hand_n_leave > 0) {
                    assert(hand_n_leave > 0);
                    // equivalent to jl_pop_handler(hand_n_leave), longjmping
                    // to the :enter code above instead, which handles cleanup
                    jl_handler_t *eh = ct->eh;
                    while (--hand_n_leave > 0) {
                        // pop GC frames for any skipped handlers
                        ct->gcstack = eh->gcstack;
                        eh = eh->prev;
                    }
                    // leave happens during normal control flow, but we must
                    // longjmp to pop the eval_body call for each enter.
                    s->continue_at = next_ip;
                    asan_unpoison_task_stack(ct, &eh->eh_ctx);
                    jl_longjmp(eh->eh_ctx, 1);
                }
            }
            else if (head == jl_pop_exception_sym) {
                size_t prev_state = jl_unbox_ulong(eval_value(jl_exprarg(stmt, 0), s));
                jl_restore_excstack(ct, prev_state);
            }
            else if (toplevel) {
                if (head == jl_method_sym && jl_expr_nargs(stmt) > 1) {
                    jl_value_t *res = eval_methoddef((jl_expr_t*)stmt, s);
                    s->locals[jl_source_nslots(s->src) + s->ip] = res;
                }
                else if (head == jl_toplevel_sym) {
                    jl_value_t *res = jl_toplevel_eval(s->module, stmt);
                    s->locals[jl_source_nslots(s->src) + s->ip] = res;
                }
                else if (head == jl_globaldecl_sym) {
                    jl_value_t *val = NULL;
                    if (jl_expr_nargs(stmt) >= 2) {
                        val = eval_value(jl_exprarg(stmt, 1), s);
                        s->locals[jl_source_nslots(s->src) + s->ip] = val; // temporarily root
                    }
                    jl_declare_global(s->module, jl_exprarg(stmt, 0), val, 1);
                    s->locals[jl_source_nslots(s->src) + s->ip] = jl_nothing;
                }
                else if (head == jl_const_sym) {
                    jl_value_t *val = jl_expr_nargs(stmt) == 1 ? NULL : eval_value(jl_exprarg(stmt, 1), s);
                    s->locals[jl_source_nslots(s->src) + s->ip] = val; // temporarily root
                    jl_eval_const_decl(s->module, jl_exprarg(stmt, 0), val);
                    s->locals[jl_source_nslots(s->src) + s->ip] = jl_nothing;
                }
                else if (head == jl_latestworld_sym) {
                    ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
                }
                else if (jl_is_toplevel_only_expr(stmt)) {
                    jl_toplevel_eval(s->module, stmt);
                }
                else if (head == jl_meta_sym) {
                    if (jl_expr_nargs(stmt) == 1 && jl_exprarg(stmt, 0) == (jl_value_t*)jl_nospecialize_sym) {
                        jl_set_module_nospecialize(s->module, 1);
                    }
                    if (jl_expr_nargs(stmt) == 1 && jl_exprarg(stmt, 0) == (jl_value_t*)jl_specialize_sym) {
                        jl_set_module_nospecialize(s->module, 0);
                    }
                    if (jl_expr_nargs(stmt) == 2) {
                        if (jl_exprarg(stmt, 0) == (jl_value_t*)jl_optlevel_sym) {
                            if (jl_is_long(jl_exprarg(stmt, 1))) {
                                int n = jl_unbox_long(jl_exprarg(stmt, 1));
                                jl_set_module_optlevel(s->module, n);
                            }
                        }
                        else if (jl_exprarg(stmt, 0) == (jl_value_t*)jl_compile_sym) {
                            if (jl_is_long(jl_exprarg(stmt, 1))) {
                                jl_set_module_compile(s->module, jl_unbox_long(jl_exprarg(stmt, 1)));
                            }
                        }
                        else if (jl_exprarg(stmt, 0) == (jl_value_t*)jl_infer_sym) {
                            if (jl_is_long(jl_exprarg(stmt, 1))) {
                                jl_set_module_infer(s->module, jl_unbox_long(jl_exprarg(stmt, 1)));
                            }
                        }
                        else if (jl_exprarg(stmt, 0) == (jl_value_t*)jl_max_methods_sym) {
                            if (jl_is_long(jl_exprarg(stmt, 1))) {
                                jl_set_module_max_methods(s->module, jl_unbox_long(jl_exprarg(stmt, 1)));
                            }
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
            assert(jl_is_slotnumber(var));
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

jl_value_t *jl_code_or_ci_for_interpreter(jl_method_instance_t *mi, size_t world)
{
    jl_value_t *ret = NULL;
    jl_code_info_t *src = NULL;
    if (jl_is_method(mi->def.value)) {
        if (mi->def.method->source) {
            jl_method_t *m = mi->def.method;
            src = (jl_code_info_t*)m->source;
            if (!jl_is_code_info(src)) {
                src = jl_uncompress_ir(mi->def.method, NULL, (jl_value_t*)src);
                // Replace the method source by the uncompressed version,
                // under the assumption that the interpreter may need to
                // access it frequently. TODO: Have some sort of usage-based
                // cache here.
                m->source = (jl_value_t*)src;
                jl_gc_wb(m, src);
            }
            ret = (jl_value_t*)src;
        }
        else {
            jl_code_instance_t *cache = jl_atomic_load_relaxed(&mi->cache);
            jl_code_instance_t *uninferred = jl_cached_uninferred(cache, world);
            if (!uninferred) {
                assert(mi->def.method->generator);
                src = jl_code_for_staged(mi, world, &uninferred);
            }
            ret = (jl_value_t*)uninferred;
            src = (jl_code_info_t*)jl_atomic_load_relaxed(&uninferred->inferred);
        }
    }
    else {
        jl_code_instance_t *uninferred = jl_cached_uninferred(jl_atomic_load_relaxed(&mi->cache), world);
        ret = (jl_value_t*)uninferred;
        if (ret) {
            src = (jl_code_info_t*)jl_atomic_load_relaxed(&uninferred->inferred);
        }
    }
    if (!src || !jl_is_code_info(src)) {
        jl_throw(jl_new_struct(jl_missingcodeerror_type, (jl_value_t*)mi));
    }
    return ret;
}

jl_code_info_t *jl_code_for_interpreter(jl_method_instance_t *mi, size_t world)
{
    jl_value_t *code_or_ci = jl_code_or_ci_for_interpreter(mi, world);
    if (jl_is_code_instance(code_or_ci))
        return (jl_code_info_t*)jl_atomic_load_relaxed(&((jl_code_instance_t*)code_or_ci)->inferred);
    return (jl_code_info_t*)code_or_ci;
}

// interpreter entry points

jl_value_t *NOINLINE jl_fptr_interpret_call(jl_value_t *f, jl_value_t **args, uint32_t nargs, jl_code_instance_t *codeinst)
{
    interpreter_state *s;
    jl_method_instance_t *mi = jl_get_ci_mi(codeinst);
    jl_task_t *ct = jl_current_task;
    size_t world = ct->world_age;
    jl_code_info_t *src = NULL;
    jl_value_t *code = jl_code_or_ci_for_interpreter(mi, world);
    jl_code_instance_t *ci = NULL;
    if (jl_is_code_instance(code)) {
        ci = (jl_code_instance_t*)code;
        src = (jl_code_info_t*)jl_atomic_load_relaxed(&ci->inferred);
    } else {
        src = (jl_code_info_t*)code;
    }
    jl_array_t *stmts = src->code;
    assert(jl_typetagis(stmts, jl_array_any_type));
    unsigned nroots = jl_source_nslots(src) + jl_source_nssavalues(src) + 2;
    jl_value_t **locals = NULL;
    JL_GC_PUSHFRAME(s, locals, nroots);
    locals[0] = (jl_value_t*)src;
    locals[1] = (jl_value_t*)stmts;
    s->locals = locals + 2;
    s->src = src;
    if (jl_is_module(mi->def.value)) {
        s->module = mi->def.module;
    }
    else {
        s->module = mi->def.method->module;
        size_t defargs = src->nargs;
        int isva = src->isva;
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
    s->ci = ci;
    JL_GC_ENABLEFRAME(s);
    jl_value_t *r = eval_body(stmts, s, 0, 0);
    JL_GC_POP();
    return r;
}

JL_DLLEXPORT const jl_callptr_t jl_fptr_interpret_call_addr = &jl_fptr_interpret_call;

jl_value_t *jl_interpret_opaque_closure(jl_opaque_closure_t *oc, jl_value_t **args, size_t nargs)
{
    jl_method_t *source = oc->source;
    jl_code_info_t *code = NULL;
    if (source->source) {
        code = jl_uncompress_ir(source, NULL, (jl_value_t*)source->source);
    }
    else {
        // OC constructed from optimized IR. It'll have a single specialization with optimized code
        // in it that we'll try to interpret.
        jl_svec_t *specializations = (jl_svec_t*)jl_atomic_load_relaxed(&source->specializations);
        assert(jl_is_method_instance(specializations));
        jl_method_instance_t *mi = (jl_method_instance_t *)specializations;
        jl_code_instance_t *ci = jl_atomic_load_relaxed(&mi->cache);
        jl_value_t *src = jl_atomic_load_relaxed(&ci->inferred);
        if (!src) {
            // This can happen if somebody did :new_opaque_closure with broken IR. This is definitely bad
            // and UB, but let's try to be slightly nicer than segfaulting here for people debugging.
            jl_error("Internal Error: Opaque closure with no source at all");
        }
        code = jl_uncompress_ir(source, ci, src);
    }
    interpreter_state *s;
    unsigned nroots = jl_source_nslots(code) + jl_source_nssavalues(code) + 2;
    jl_task_t *ct = jl_current_task;
    size_t last_age = ct->world_age;
    ct->world_age = oc->world;
    jl_value_t **locals = NULL;
    JL_GC_PUSHFRAME(s, locals, nroots);
    locals[0] = (jl_value_t*)oc;
    // The analyzer has some trouble with this
    locals[1] = (jl_value_t*)code;
    JL_GC_PROMISE_ROOTED(code);
    locals[2] = (jl_value_t*)oc->captures;
    s->locals = locals + 2;
    s->src = code;
    s->module = source->module;
    s->sparam_vals = NULL;
    s->preevaluation = 0;
    s->continue_at = 0;
    s->mi = NULL;
    s->ci = NULL;
    size_t defargs = source->nargs;
    int isva = source->isva;
    assert(isva ? nargs + 2 >= defargs : nargs + 1 == defargs);
    for (size_t i = 1; i < defargs - isva; i++)
        s->locals[i] = args[i - 1];
    if (isva) {
        assert(defargs >= 2);
        s->locals[defargs - 1] = jl_f_tuple(NULL, &args[defargs - 2], nargs + 2 - defargs);
    }
    JL_GC_ENABLEFRAME(s);
    jl_value_t *r = eval_body(code->code, s, 0, 0);
    locals[0] = r; // GC root
    JL_GC_PROMISE_ROOTED(r);
    ct->world_age = last_age;
    jl_typeassert(r, jl_tparam1(jl_typeof(oc)));
    JL_GC_POP();
    return r;
}

jl_value_t *NOINLINE jl_interpret_toplevel_thunk(jl_module_t *m, jl_code_info_t *src)
{
    interpreter_state *s;
    unsigned nroots = jl_source_nslots(src) + jl_source_nssavalues(src);
    JL_GC_PUSHFRAME(s, s->locals, nroots);
    jl_array_t *stmts = src->code;
    assert(jl_typetagis(stmts, jl_array_any_type));
    s->src = src;
    s->module = m;
    s->sparam_vals = jl_emptysvec;
    s->continue_at = 0;
    s->mi = NULL;
    s->ci = NULL;
    JL_GC_ENABLEFRAME(s);
    jl_value_t *r = eval_body(stmts, s, 0, 1);
    JL_GC_POP();
    return r;
}

// deprecated: do not use this method in new code
// it uses special scoping / evaluation / error rules
// which should instead be handled in lowering
jl_value_t *NOINLINE jl_interpret_toplevel_expr_in(jl_module_t *m, jl_value_t *e, jl_code_info_t *src, jl_svec_t *sparam_vals)
{
    interpreter_state *s;
    jl_value_t **locals;
    JL_GC_PUSHFRAME(s, locals, 0);
    (void)locals;
    s->src = src;
    s->module = m;
    s->sparam_vals = sparam_vals;
    s->preevaluation = (sparam_vals != NULL);
    s->continue_at = 0;
    s->mi = NULL;
    s->ci = NULL;
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
    bt_entry[2].jlvalue = s->ci  ? (jl_value_t*)s->ci  :
                          s->mi  ? (jl_value_t*)s->mi  :
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
