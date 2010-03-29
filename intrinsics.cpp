namespace JL_I {
    enum intrinsic {
        // arithmetic
        neg_int8=0, add_int8, sub_int8, mul_int8, div_int8, mod_int8,
        neg_int16, add_int16, sub_int16, mul_int16, div_int16, mod_int16,
        neg_int32, add_int32, sub_int32, mul_int32, div_int32, mod_int32,
        neg_int64, add_int64, sub_int64, mul_int64, div_int64, mod_int64,
        neg_float32, add_float32, sub_float32, mul_float32, div_float32,
        neg_float64, add_float64, sub_float64, mul_float64, div_float64,
        // comparison
        eq_int8, lt_int8,       eq_int16, lt_int16,
        eq_int32, lt_int32,     eq_int64, lt_int64,
        eq_float32, lt_float32, eq_float64, lt_float64,
        ne_float32, ne_float64,
        // conversion
        to_bool,
        to_int8, to_uint8,
        to_int16, to_uint16,
        to_int32, to_uint32,
        to_int64, to_uint64,
        to_float32, to_float64,
    };
};

using namespace JL_I;

static Function *box_int8_func;
static Function *box_uint8_func;
static Function *box_int16_func;
static Function *box_uint16_func;
static Function *box_int32_func;
static Function *box_uint32_func;
static Function *box_int64_func;
static Function *box_uint64_func;
static Function *box_float32_func;
static Function *box_float64_func;
static Function *unbox_int8_func;
static Function *unbox_uint8_func;
static Function *unbox_int16_func;
static Function *unbox_uint16_func;
static Function *unbox_int32_func;
static Function *unbox_uint32_func;
static Function *unbox_int64_func;
static Function *unbox_uint64_func;
static Function *unbox_float32_func;
static Function *unbox_float64_func;

#define HANDLE(intr,n)                                                  \
    case intr: if (nargs!=n) jl_error(#intr": wrong number of arguments");

#define BINARY_OP(typ, inst)                                            \
    return                                                              \
      builder.CreateCall(box_##typ##_func,                              \
        builder.Create##inst(builder.CreateCall(unbox_##typ##_func,     \
                                                emit_expr(args[1],ctx,true)), \
                             builder.CreateCall(unbox_##typ##_func,     \
                                                emit_expr(args[2],ctx,true))))

static Value *emit_intrinsic(intrinsic f, jl_value_t **args, size_t nargs,
                             jl_codectx_t *ctx)
{
    switch (f) {
    HANDLE(neg_int8,1)
        break;
    HANDLE(add_int8,2)
        BINARY_OP(int8, Add);
    HANDLE(sub_int8,2)
        break;
    HANDLE(mul_int8,2)
        break;
    HANDLE(div_int8,2)
        break;
    HANDLE(mod_int8,2)
        break;
    HANDLE(neg_int16,1)
        break;
    HANDLE(add_int16,2)
        break;
    HANDLE(sub_int16,2)
        break;
    HANDLE(mul_int16,2)
        break;
    HANDLE(div_int16,2)
        break;
    HANDLE(mod_int16,2)
        break;
    HANDLE(neg_int32,1)
        break;
    HANDLE(add_int32,2)
        BINARY_OP(int32, Add);
    HANDLE(sub_int32,2)
        break;
    HANDLE(mul_int32,2)
        break;
    HANDLE(div_int32,2)
        break;
    HANDLE(mod_int32,2)
        break;
    HANDLE(neg_int64,1)
        break;
    HANDLE(add_int64,2)
        break;
    HANDLE(sub_int64,2)
        break;
    HANDLE(mul_int64,2)
        break;
    HANDLE(div_int64,2)
        break;
    HANDLE(mod_int64,2)
        break;
    HANDLE(neg_float32,1)
        break;
    HANDLE(add_float32,2)
        break;
    HANDLE(sub_float32,2)
        break;
    HANDLE(mul_float32,2)
        break;
    HANDLE(div_float32,2)
        break;
    HANDLE(neg_float64,1)
        break;
    HANDLE(add_float64,2)
        break;
    HANDLE(sub_float64,2)
        break;
    HANDLE(mul_float64,2)
        break;
    HANDLE(div_float64,2)
        break;
    HANDLE(eq_int8,2)
        break;
    HANDLE(lt_int8,2)
        break;
    HANDLE(eq_int16,2)
        break;
    HANDLE(lt_int16,2)
        break;
    HANDLE(eq_int32,2)
        break;
    HANDLE(lt_int32,2)
        break;
    HANDLE(eq_int64,2)
        break;
    HANDLE(lt_int64,2)
        break;
    HANDLE(eq_float32,2)
        break;
    HANDLE(lt_float32,2)
        break;
    HANDLE(eq_float64,2)
        break;
    HANDLE(lt_float64,2)
        break;
    HANDLE(ne_float32,2)
        break;
    HANDLE(ne_float64,2)
        break;
    HANDLE(to_bool,1)
        break;
    HANDLE(to_int8,1)
        break;
    HANDLE(to_uint8,1)
        break;
    HANDLE(to_int16,1)
        break;
    HANDLE(to_uint16,1)
        break;
    HANDLE(to_int32,1)
        break;
    HANDLE(to_uint32,1)
        break;
    HANDLE(to_int64,1)
        break;
    HANDLE(to_uint64,1)
        break;
    HANDLE(to_float32,1)
        break;
    HANDLE(to_float64,1)
        break;
    }
}

#undef HANDLE

static Function *boxfunc_llvm(FunctionType *ft, const std::string &cname,
                              void *addr)
{
    Function *f =
        Function::Create(ft, Function::ExternalLinkage, cname, jl_Module);
    jl_ExecutionEngine->addGlobalMapping(f, addr);
    return f;
}

static FunctionType *ft1arg(const Type *ret, const Type *arg)
{
    std::vector<const Type*> args1(0);
    args1.push_back(arg);
    return FunctionType::get(ret, args1, false);
}

static void add_intrinsic(const std::string &name, intrinsic f)
{
    jl_value_t *i = jl_box_uint32((uint32_t)f);
    i->type = (jl_type_t*)jl_intrinsic_type;
    jl_set_const(jl_system_module, jl_symbol((char*)name.c_str()), i);
}

#define ADD_I(name) add_intrinsic(#name, name)
#define BOX_F(ct)                                                       \
    box_##ct##_func = boxfunc_llvm(ft1arg(jl_pvalue_llvmt, T_##ct),     \
                                   "jl_box_"#ct, (void*)&jl_box_##ct);  \
    unbox_##ct##_func = boxfunc_llvm(ft1arg(T_##ct, jl_pvalue_llvmt),   \
                                     "jl_unbox_"#ct, (void*)&jl_unbox_##ct)

extern "C" void jl_init_intrinsic_functions()
{
    ADD_I(neg_int8); ADD_I(add_int8); ADD_I(sub_int8); ADD_I(mul_int8);
    ADD_I(div_int8); ADD_I(mod_int8);
    ADD_I(neg_int16); ADD_I(add_int16); ADD_I(sub_int16); ADD_I(mul_int16);
    ADD_I(div_int16); ADD_I(mod_int16);
    ADD_I(neg_int32); ADD_I(add_int32); ADD_I(sub_int32); ADD_I(mul_int32);
    ADD_I(div_int32); ADD_I(mod_int32);
    ADD_I(neg_int64); ADD_I(add_int64); ADD_I(sub_int64); ADD_I(mul_int64);
    ADD_I(div_int64); ADD_I(mod_int64);
    ADD_I(neg_float32); ADD_I(add_float32);
    ADD_I(sub_float32); ADD_I(mul_float32);
    ADD_I(div_float32);
    ADD_I(neg_float64); ADD_I(add_float64);
    ADD_I(sub_float64); ADD_I(mul_float64);
    ADD_I(div_float64);
    ADD_I(eq_int8);  ADD_I(lt_int8);
    ADD_I(eq_int16); ADD_I(lt_int16);
    ADD_I(eq_int32); ADD_I(lt_int32);
    ADD_I(eq_int64); ADD_I(lt_int64);
    ADD_I(eq_float32); ADD_I(lt_float32);
    ADD_I(eq_float64); ADD_I(lt_float64);
    ADD_I(ne_float32); ADD_I(ne_float64);
    ADD_I(to_bool);
    ADD_I(to_int8);    ADD_I(to_uint8);
    ADD_I(to_int16);   ADD_I(to_uint16);
    ADD_I(to_int32);   ADD_I(to_uint32);
    ADD_I(to_int64);   ADD_I(to_uint64);
    ADD_I(to_float32); ADD_I(to_float64);

    BOX_F(int8);  BOX_F(uint8);
    BOX_F(int16); BOX_F(uint16);
    BOX_F(int32); BOX_F(uint32);
    BOX_F(int64); BOX_F(uint64);
    BOX_F(float32); BOX_F(float64);
}
