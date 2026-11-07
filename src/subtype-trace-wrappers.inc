// This file is a part of Julia. License is MIT: https://julialang.org/license

// Instrumented wrappers and the `jl_subtype_trace` entry point; included at the
// end of subtype.c (see subtype-trace.inc for the transcript format). Each
// wrapper logs a `call` event, runs the original function (whose definition is
// renamed to `<name>__untraced`), and logs its `ret`.

#ifdef JL_SUBTYPE_TRACE

#define TRACE_CALL(fn, e, nargs, a, b, c) \
    strace_t *t = strace_call(#fn, e, nargs, (jl_value_t*)(a), (jl_value_t*)(b), (jl_value_t*)(c))
#define TRACE_INT(key, v) do { if (t) strace_kint(t, key, v); } while (0)
#define TRACE_VB(key, vb) do { if (t) strace_kint(t, key, strace_vb_id(t, vb)); } while (0)
#define TRACE_ATOM(key, v) do { if (t) strace_katom(t, key, (jl_value_t*)(v)); } while (0)
#define TRACE_END() do { if (t) strace_end(t); } while (0)

// subtyping

static int subtype(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_param_pos_t param)
{
    TRACE_CALL(subtype, e, 2, x, y, NULL);
    TRACE_INT("param", param);
    TRACE_END();
    return strace_ret_int(t, subtype__untraced(x, y, e, param));
}

static int forall_exists_subtype(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_param_pos_t param)
{
    TRACE_CALL(forall_exists_subtype, e, 2, x, y, NULL);
    TRACE_INT("param", param);
    TRACE_END();
    return strace_ret_int(t, forall_exists_subtype__untraced(x, y, e, param));
}

static int local_forall_exists_subtype(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_param_pos_t param, int limit_slow)
{
    TRACE_CALL(local_forall_exists_subtype, e, 2, x, y, NULL);
    TRACE_INT("param", param);
    TRACE_INT("limit_slow", limit_slow);
    TRACE_END();
    return strace_ret_int(t, local_forall_exists_subtype__untraced(x, y, e, param, limit_slow));
}

static int exists_subtype(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_savedenv_t *se, jl_param_pos_t param)
{
    TRACE_CALL(exists_subtype, e, 2, x, y, NULL);
    TRACE_INT("param", param);
    TRACE_INT("se", se->trace_id);
    TRACE_END();
    return strace_ret_int(t, exists_subtype__untraced(x, y, e, se, param));
}

static int forall_exists_equal(jl_value_t *x, jl_value_t *y, jl_stenv_t *e)
{
    TRACE_CALL(forall_exists_equal, e, 2, x, y, NULL);
    TRACE_END();
    return strace_ret_int(t, forall_exists_equal__untraced(x, y, e));
}

static int equal_var(jl_tvar_t *v, jl_value_t *x, jl_stenv_t *e)
{
    TRACE_CALL(equal_var, e, 2, v, x, NULL);
    TRACE_END();
    return strace_ret_int(t, equal_var__untraced(v, x, e));
}

static int subtype_unionall(jl_value_t *ty, jl_unionall_t *u, jl_stenv_t *e, int8_t R, jl_param_pos_t param)
{
    TRACE_CALL(subtype_unionall, e, 2, ty, u, NULL);
    TRACE_INT("R", R);
    TRACE_INT("param", param);
    TRACE_END();
    return strace_ret_int(t, subtype_unionall__untraced(ty, u, e, R, param));
}

static int subtype_var(jl_tvar_t *b, jl_value_t *a, jl_stenv_t *e, int R, jl_param_pos_t param, jl_varbinding_t *bb, int innervar)
{
    TRACE_CALL(subtype_var, e, 2, b, a, NULL);
    TRACE_INT("R", R);
    TRACE_INT("param", param);
    TRACE_VB("vb", bb);
    TRACE_INT("innervar", innervar);
    TRACE_END();
    return strace_ret_int(t, subtype_var__untraced(b, a, e, R, param, bb, innervar));
}

static int var_lt(jl_tvar_t *b, jl_value_t *a, jl_stenv_t *e, jl_param_pos_t param, jl_varbinding_t *bb, int innervar)
{
    TRACE_CALL(var_lt, e, 2, b, a, NULL);
    TRACE_INT("param", param);
    TRACE_VB("vb", bb);
    TRACE_INT("innervar", innervar);
    TRACE_END();
    return strace_ret_int(t, var_lt__untraced(b, a, e, param, bb, innervar));
}

static int var_gt(jl_tvar_t *b, jl_value_t *a, jl_stenv_t *e, jl_param_pos_t param, jl_varbinding_t *bb, int innervar)
{
    TRACE_CALL(var_gt, e, 2, b, a, NULL);
    TRACE_INT("param", param);
    TRACE_VB("vb", bb);
    TRACE_INT("innervar", innervar);
    TRACE_END();
    return strace_ret_int(t, var_gt__untraced(b, a, e, param, bb, innervar));
}

static int subtype_tuple(jl_datatype_t *xd, jl_datatype_t *yd, jl_stenv_t *e, jl_param_pos_t param)
{
    TRACE_CALL(subtype_tuple, e, 2, xd, yd, NULL);
    TRACE_INT("param", param);
    TRACE_END();
    return strace_ret_int(t, subtype_tuple__untraced(xd, yd, e, param));
}

static int subtype_tuple_tail(jl_datatype_t *xd, jl_datatype_t *yd, int8_t R, jl_stenv_t *e, jl_param_pos_t param)
{
    TRACE_CALL(subtype_tuple_tail, e, 2, xd, yd, NULL);
    TRACE_INT("R", R);
    TRACE_INT("param", param);
    TRACE_END();
    return strace_ret_int(t, subtype_tuple_tail__untraced(xd, yd, R, e, param));
}

static int subtype_tuple_varargs(jl_vararg_t *vtx, jl_vararg_t *vty, jl_value_t *lastx, jl_value_t *lasty,
                                 size_t vx, size_t vy, size_t x_reps, jl_stenv_t *e, jl_param_pos_t param)
{
    TRACE_CALL(subtype_tuple_varargs, e, 2, vtx, vty, NULL);
    TRACE_ATOM("lastx", lastx);
    TRACE_ATOM("lasty", lasty);
    TRACE_INT("vx", vx);
    TRACE_INT("vy", vy);
    TRACE_INT("x_reps", x_reps);
    TRACE_INT("param", param);
    TRACE_END();
    return strace_ret_int(t, subtype_tuple_varargs__untraced(vtx, vty, lastx, lasty, vx, vy, x_reps, e, param));
}

static int subtype_ccheck(jl_value_t *x, jl_value_t *y, jl_stenv_t *e)
{
    TRACE_CALL(subtype_ccheck, e, 2, x, y, NULL);
    TRACE_END();
    return strace_ret_int(t, subtype_ccheck__untraced(x, y, e));
}

static int subtype_left_var(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_param_pos_t param)
{
    TRACE_CALL(subtype_left_var, e, 2, x, y, NULL);
    TRACE_INT("param", param);
    TRACE_END();
    return strace_ret_int(t, subtype_left_var__untraced(x, y, e, param));
}

static int obvious_subtype(jl_value_t *x, jl_value_t *y, jl_value_t *y0, int *subtype)
{
    TRACE_CALL(obvious_subtype, NULL, 3, x, y, y0);
    TRACE_END();
    int known = obvious_subtype__untraced(x, y, y0, subtype);
    if (t) {
        strace_ret_begin(t);
        strace_kint(t, "res", known);
        if (known)
            strace_kint(t, "sub", *subtype);
        strace_end(t);
    }
    return known;
}

static int subtype_in_env(jl_value_t *x, jl_value_t *y, jl_stenv_t *e)
{
    TRACE_CALL(subtype_in_env, e, 2, x, y, NULL);
    TRACE_END();
    return strace_ret_int(t, subtype_in_env__untraced(x, y, e));
}

static int subtype_in_env_existential(jl_value_t *x, jl_value_t *y, jl_stenv_t *e)
{
    TRACE_CALL(subtype_in_env_existential, e, 2, x, y, NULL);
    TRACE_END();
    return strace_ret_int(t, subtype_in_env_existential__untraced(x, y, e));
}

static int subtype_in_env_nonexistential(jl_value_t *x, jl_value_t *y, jl_stenv_t *e)
{
    TRACE_CALL(subtype_in_env_nonexistential, e, 2, x, y, NULL);
    TRACE_END();
    return strace_ret_int(t, subtype_in_env_nonexistential__untraced(x, y, e));
}

static int subtype_by_bounds(jl_value_t *x, jl_value_t *y, jl_stenv_t *e)
{
    TRACE_CALL(subtype_by_bounds, e, 2, x, y, NULL);
    TRACE_END();
    return strace_ret_int(t, subtype_by_bounds__untraced(x, y, e));
}

// intersection

static jl_value_t *intersect(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_param_pos_t param)
{
    TRACE_CALL(intersect, e, 2, x, y, NULL);
    TRACE_INT("param", param);
    TRACE_END();
    return strace_ret_val(t, intersect__untraced(x, y, e, param));
}

static jl_value_t *intersect_all(jl_value_t *x, jl_value_t *y, jl_stenv_t *e)
{
    TRACE_CALL(intersect_all, e, 2, x, y, NULL);
    TRACE_END();
    return strace_ret_val(t, intersect_all__untraced(x, y, e));
}

static jl_value_t *intersect_aside(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int depth)
{
    TRACE_CALL(intersect_aside, e, 2, x, y, NULL);
    TRACE_INT("depth", depth);
    TRACE_END();
    return strace_ret_val(t, intersect_aside__untraced(x, y, e, depth));
}

static jl_value_t *intersect_union(jl_value_t *x, jl_uniontype_t *u, jl_stenv_t *e, int8_t R, jl_param_pos_t param)
{
    TRACE_CALL(intersect_union, e, 2, x, u, NULL);
    TRACE_INT("R", R);
    TRACE_INT("param", param);
    TRACE_END();
    return strace_ret_val(t, intersect_union__untraced(x, u, e, R, param));
}

static jl_value_t *intersect_var(jl_tvar_t *b, jl_value_t *a, jl_stenv_t *e, int8_t R, jl_param_pos_t param)
{
    TRACE_CALL(intersect_var, e, 2, b, a, NULL);
    TRACE_INT("R", R);
    TRACE_INT("param", param);
    TRACE_END();
    return strace_ret_val(t, intersect_var__untraced(b, a, e, R, param));
}

static jl_value_t *intersect_unionall(jl_value_t *ty, jl_unionall_t *u, jl_stenv_t *e, int8_t R, jl_param_pos_t param)
{
    TRACE_CALL(intersect_unionall, e, 2, ty, u, NULL);
    TRACE_INT("R", R);
    TRACE_INT("param", param);
    TRACE_END();
    return strace_ret_val(t, intersect_unionall__untraced(ty, u, e, R, param));
}

// `vb` is pushed inside this call, so its `bind` event follows the `call`.
static jl_value_t *intersect_unionall_(jl_value_t *ty, jl_unionall_t *u, jl_stenv_t *e, int8_t R, jl_param_pos_t param, jl_varbinding_t *vb)
{
    TRACE_CALL(intersect_unionall_, e, 2, ty, u, NULL);
    TRACE_INT("R", R);
    TRACE_INT("param", param);
    TRACE_INT("constraintkind", vb->constraintkind);
    TRACE_END();
    return strace_ret_val(t, intersect_unionall___untraced(ty, u, e, R, param, vb));
}

static jl_value_t *finish_unionall(jl_value_t *res JL_MAYBE_UNROOTED, jl_varbinding_t *vb, jl_unionall_t *u, jl_stenv_t *e)
{
    TRACE_CALL(finish_unionall, e, 2, res, u, NULL);
    TRACE_ATOM("var", vb->var);
    TRACE_END();
    return strace_ret_val(t, finish_unionall__untraced(res, vb, u, e));
}

static jl_value_t *intersect_tuple(jl_datatype_t *xd, jl_datatype_t *yd, jl_stenv_t *e, jl_param_pos_t param)
{
    TRACE_CALL(intersect_tuple, e, 2, xd, yd, NULL);
    TRACE_INT("param", param);
    TRACE_END();
    return strace_ret_val(t, intersect_tuple__untraced(xd, yd, e, param));
}

static jl_value_t *intersect_varargs(jl_vararg_t *vmx, jl_vararg_t *vmy, ssize_t offset, jl_stenv_t *e, jl_param_pos_t param)
{
    TRACE_CALL(intersect_varargs, e, 2, vmx, vmy, NULL);
    TRACE_INT("offset", offset);
    TRACE_INT("param", param);
    TRACE_END();
    return strace_ret_val(t, intersect_varargs__untraced(vmx, vmy, offset, e, param));
}

static jl_value_t *intersect_invariant(jl_value_t *x, jl_value_t *y, jl_stenv_t *e)
{
    TRACE_CALL(intersect_invariant, e, 2, x, y, NULL);
    TRACE_END();
    return strace_ret_val(t, intersect_invariant__untraced(x, y, e));
}

static jl_value_t *intersect_sub_datatype(jl_datatype_t *xd, jl_datatype_t *yd, jl_stenv_t *e, int R, jl_param_pos_t param)
{
    TRACE_CALL(intersect_sub_datatype, e, 2, xd, yd, NULL);
    TRACE_INT("R", R);
    TRACE_INT("param", param);
    TRACE_END();
    return strace_ret_val(t, intersect_sub_datatype__untraced(xd, yd, e, R, param));
}

static jl_value_t *intersect_type_type(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int8_t R)
{
    TRACE_CALL(intersect_type_type, e, 2, x, y, NULL);
    TRACE_INT("R", R);
    TRACE_END();
    return strace_ret_val(t, intersect_type_type__untraced(x, y, e, R));
}

static jl_value_t *set_var_to_const(jl_varbinding_t *bb, jl_value_t *v JL_MAYBE_UNROOTED, jl_stenv_t *e, int R)
{
    TRACE_CALL(set_var_to_const, e, 1, v, NULL, NULL);
    TRACE_VB("vb", bb);
    TRACE_INT("R", R);
    TRACE_END();
    return strace_ret_val(t, set_var_to_const__untraced(bb, v, e, R));
}

static jl_value_t *bound_var_below(jl_tvar_t *tv, jl_varbinding_t *bb, jl_stenv_t *e, int R)
{
    TRACE_CALL(bound_var_below, e, 1, tv, NULL, NULL);
    TRACE_VB("vb", bb);
    TRACE_INT("R", R);
    TRACE_END();
    return strace_ret_val(t, bound_var_below__untraced(tv, bb, e, R));
}

// entry points

static jl_value_t *intersect_types(jl_value_t *x, jl_value_t *y, int emptiness_only)
{
    TRACE_CALL(intersect_types, NULL, 2, x, y, NULL);
    TRACE_INT("emptiness_only", emptiness_only);
    TRACE_END();
    return strace_ret_val(t, intersect_types__untraced(x, y, emptiness_only));
}

JL_DLLEXPORT int jl_subtype_env(jl_value_t *x, jl_value_t *y, jl_value_t **env, int envsz)
{
    TRACE_CALL(jl_subtype_env, NULL, 2, x, y, NULL);
    TRACE_INT("envsz", envsz);
    TRACE_END();
    int sub = jl_subtype_env__untraced(x, y, env, envsz);
    if (t) {
        strace_ret_begin(t);
        strace_kint(t, "res", sub);
        if (envsz) {
            ios_puts(",\"env\":[", &t->log);
            for (int i = 0; i < envsz; i++) {
                int64_t id = strace_atom(t, env[i]);
                if (i)
                    ios_putc(',', &t->log);
                strace_id(&t->log, id);
            }
            ios_putc(']', &t->log);
        }
        strace_end(t);
    }
    return sub;
}

JL_DLLEXPORT int jl_types_equal(jl_value_t *a, jl_value_t *b)
{
    TRACE_CALL(jl_types_equal, NULL, 2, a, b, NULL);
    TRACE_END();
    return strace_ret_int(t, jl_types_equal__untraced(a, b));
}

JL_DLLEXPORT int jl_isa(jl_value_t *x, jl_value_t *ty)
{
    TRACE_CALL(jl_isa, NULL, 2, x, ty, NULL);
    TRACE_END();
    return strace_ret_int(t, jl_isa__untraced(x, ty));
}

jl_value_t *jl_type_intersection_env_s(jl_value_t *a, jl_value_t *b, jl_svec_t **penv, int *issubty)
{
    TRACE_CALL(jl_type_intersection_env_s, NULL, 2, a, b, NULL);
    TRACE_INT("want_env", penv != NULL);
    TRACE_END();
    jl_value_t *res = jl_type_intersection_env_s__untraced(a, b, penv, issubty);
    if (t) {
        strace_ret_begin(t);
        strace_katom(t, "res", res);
        if (issubty)
            strace_kint(t, "issubty", *issubty);
        if (penv && *penv)
            strace_katom(t, "env", (jl_value_t*)*penv);
        strace_end(t);
    }
    return res;
}

#undef TRACE_CALL
#undef TRACE_INT
#undef TRACE_VB
#undef TRACE_ATOM
#undef TRACE_END

static void strace_free(strace_t *t) JL_NOTSAFEPOINT
{
    htable_free(&t->ids);
    arraylist_free(&t->roots);
    arraylist_free(&t->spans);
    ios_close(&t->atoms);
    ios_close(&t->log);
    free(t->frames);
    free(t->vbs);
    free(t);
}

static void strace_stop(strace_t *t) JL_NOTSAFEPOINT
{
    jl_atomic_store_relaxed(&strace_ctx, NULL);
    jl_gc_set_cb_root_scanner(strace_root_scanner, 0);
    strace_free(t);
}

static void strace_env_ids(strace_t *t, ios_t *o, jl_svec_t *env) JL_NOTSAFEPOINT
{
    ios_putc('[', o);
    for (size_t i = 0; i < jl_svec_len(env); i++) {
        int64_t id = strace_atom(t, jl_svecref(env, i));
        if (i)
            ios_putc(',', o);
        strace_id(o, id);
    }
    ios_putc(']', o);
}

JL_DLLEXPORT int jl_subtype_trace_enabled(void)
{
    return 1;
}

JL_DLLEXPORT jl_value_t *jl_subtype_trace(const char *path, const char *kind, jl_value_t *a, jl_value_t *b) JL_CANSAFEPOINT
{
    static const char *const kinds[] = {"subtype", "subtype_env", "types_equal", "isa",
                                        "intersect", "intersect_env", "has_empty_intersection"};
    int k = 0;
    while (k < (int)(sizeof(kinds) / sizeof(*kinds)) && strcmp(kind, kinds[k]) != 0)
        k++;
    if (k == (int)(sizeof(kinds) / sizeof(*kinds)))
        jl_errorf("jl_subtype_trace: unknown query kind \"%s\"", kind);
    if (k != 3)
        JL_TYPECHK(jl_subtype_trace, type, a);
    JL_TYPECHK(jl_subtype_trace, type, b);

    strace_t *t = (strace_t*)calloc_s(sizeof(strace_t));
    t->ptls = jl_current_task->ptls;
    t->cur_event = -1;
    htable_new(&t->ids, 0);
    arraylist_new(&t->roots, 0);
    arraylist_new(&t->spans, 0);
    ios_mem(&t->atoms, 0);
    ios_mem(&t->log, 0);
    strace_t *expected = NULL;
    if (!jl_atomic_cmpswap(&strace_ctx, &expected, t)) {
        strace_free(t);
        jl_error("jl_subtype_trace: another trace is in progress");
    }
    jl_gc_set_cb_root_scanner(strace_root_scanner, 1);
    int64_t ida = strace_atom(t, a);
    int64_t idb = strace_atom(t, b);

    jl_value_t *res = NULL, *env = NULL;
    JL_GC_PUSH2(&res, &env);
    JL_TRY {
        switch (k) {
        case 0:
            res = jl_subtype(a, b) ? jl_true : jl_false;
            break;
        case 1: {
            int sz = jl_subtype_env_size(b);
            jl_value_t **envbuf;
            JL_GC_PUSHARGS(envbuf, sz + 1);
            int sub = jl_subtype_env(a, b, envbuf, sz);
            env = (jl_value_t*)jl_alloc_svec(sz);
            for (int i = 0; i < sz; i++)
                jl_svecset(env, i, envbuf[i]);
            res = (jl_value_t*)jl_svec2(sub ? jl_true : jl_false, env);
            JL_GC_POP();
            break;
        }
        case 2:
            res = jl_types_equal(a, b) ? jl_true : jl_false;
            break;
        case 3:
            res = jl_isa(a, b) ? jl_true : jl_false;
            break;
        case 4:
            res = jl_type_intersection(a, b);
            break;
        case 5:
            res = (jl_value_t*)jl_type_intersection_with_env(a, b);
            break;
        case 6:
            res = jl_has_empty_intersection(a, b) ? jl_true : jl_false;
            break;
        }
    }
    JL_CATCH {
        strace_stop(t);
        jl_rethrow();
    }

    // The result goes first into its own buffer, since it may add atoms.
    ios_t rbuf;
    ios_mem(&rbuf, 0);
    if (jl_is_bool(res)) {
        ios_puts(res == jl_true ? "true" : "false", &rbuf);
    }
    else if (k == 1) {
        ios_printf(&rbuf, "{\"sub\":%s,\"env\":", jl_svecref(res, 0) == jl_true ? "true" : "false");
        strace_env_ids(t, &rbuf, (jl_svec_t*)jl_svecref(res, 1));
        ios_putc('}', &rbuf);
    }
    else if (k == 5) {
        ios_puts("{\"type\":", &rbuf);
        strace_id(&rbuf, strace_atom(t, jl_svecref(res, 0)));
        ios_puts(",\"env\":", &rbuf);
        strace_env_ids(t, &rbuf, (jl_svec_t*)jl_svecref(res, 1));
        ios_putc('}', &rbuf);
    }
    else {
        strace_id(&rbuf, strace_atom(t, res));
    }

    ios_t f;
    int ok = ios_file(&f, path, 0, 1, 1, 0) != NULL;
    if (ok) {
        ios_seek_end(&f);
        ios_printf(&f, "{\"version\":1,\"kind\":\"%s\",\"args\":[%lld,%lld],\"result\":",
                   kind, (long long)ida, (long long)idb);
        ios_write(&f, rbuf.buf, rbuf.size);
        ios_puts(",\"atoms\":[", &f);
        for (size_t i = 0; i < t->roots.len; i++) {
            size_t start = (size_t)t->spans.items[2 * i];
            size_t end = (size_t)t->spans.items[2 * i + 1];
            if (i)
                ios_putc(',', &f);
            ios_write(&f, t->atoms.buf + start, end - start);
        }
        ios_puts("],\"log\":[", &f);
        ios_write(&f, t->log.buf, t->log.size);
        ios_puts("]}\n", &f);
        ios_close(&f);
    }
    ios_close(&rbuf);
    strace_stop(t);
    JL_GC_POP();
    if (!ok)
        jl_errorf("jl_subtype_trace: could not open \"%s\" for writing", path);
    return res;
}

#else

JL_DLLEXPORT int jl_subtype_trace_enabled(void)
{
    return 0;
}

JL_DLLEXPORT jl_value_t *jl_subtype_trace(const char *path, const char *kind, jl_value_t *a, jl_value_t *b) JL_CANSAFEPOINT
{
    jl_error("jl_subtype_trace: Julia was not built with -DJL_SUBTYPE_TRACE");
}

#endif
