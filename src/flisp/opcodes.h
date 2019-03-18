#ifndef JL_OPCODES_H
#define JL_OPCODES_H

enum {
    OP_NOP=0, OP_DUP, OP_POP, OP_CALL, OP_TCALL, OP_JMP, OP_BRF, OP_BRT,
    OP_JMPL, OP_BRFL, OP_BRTL, OP_RET,

    OP_EQ, OP_EQV, OP_EQUAL, OP_ATOMP, OP_NOT, OP_NULLP, OP_BOOLEANP,
    OP_SYMBOLP, OP_NUMBERP, OP_BOUNDP, OP_PAIRP, OP_BUILTINP, OP_VECTORP,
    OP_FIXNUMP, OP_FUNCTIONP,

    OP_CONS, OP_LIST, OP_CAR, OP_CDR, OP_SETCAR, OP_SETCDR,
    OP_APPLY,

    OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_IDIV, OP_NUMEQ, OP_LT, OP_COMPARE,

    OP_VECTOR, OP_AREF, OP_ASET,

    OP_LOADT, OP_LOADF, OP_LOADNIL, OP_LOAD0, OP_LOAD1, OP_LOADI8,
    OP_LOADV, OP_LOADVL,
    OP_LOADG, OP_LOADGL,
    OP_LOADA, OP_LOADAL, OP_LOADC, OP_LOADCL,
    OP_SETG, OP_SETGL,
    OP_SETA, OP_SETAL, OP_REMOVED_SETC, OP_REMOVED_SETCL,

    OP_CLOSURE, OP_ARGC, OP_VARGC, OP_TRYCATCH, OP_FOR,
    OP_TAPPLY, OP_ADD2, OP_SUB2, OP_NEG, OP_LARGC, OP_LVARGC,
    OP_LOADA0, OP_LOADA1, OP_LOADC0, OP_LOADC1, OP_CALLL, OP_TCALLL,
    OP_BRNE, OP_BRNEL, OP_CADR, OP_BRNN, OP_BRNNL, OP_BRN, OP_BRNL,
    OP_OPTARGS, OP_BRBOUND, OP_KEYARGS, OP_BOX, OP_BOXL, OP_SHIFT,

    OP_BOOL_CONST_T, OP_BOOL_CONST_F, OP_THE_EMPTY_LIST, OP_EOF_OBJECT,

    N_OPCODES
};

#ifdef USE_COMPUTED_GOTO
#define VM_LABELS                                                       \
    static const void *vm_labels[] = {                                  \
    NULL, &&L_OP_DUP, &&L_OP_POP, &&L_OP_CALL, &&L_OP_TCALL, &&L_OP_JMP,\
    &&L_OP_BRF, &&L_OP_BRT,                                             \
    &&L_OP_JMPL, &&L_OP_BRFL, &&L_OP_BRTL, &&L_OP_RET,                  \
                                                                        \
    &&L_OP_EQ, &&L_OP_EQV, &&L_OP_EQUAL, &&L_OP_ATOMP, &&L_OP_NOT,      \
    &&L_OP_NULLP, &&L_OP_BOOLEANP,                                      \
    &&L_OP_SYMBOLP, &&L_OP_NUMBERP, &&L_OP_BOUNDP, &&L_OP_PAIRP,        \
    &&L_OP_BUILTINP, &&L_OP_VECTORP,                                    \
    &&L_OP_FIXNUMP, &&L_OP_FUNCTIONP,                                   \
                                                                        \
    &&L_OP_CONS, &&L_OP_LIST, &&L_OP_CAR, &&L_OP_CDR, &&L_OP_SETCAR,    \
    &&L_OP_SETCDR, &&L_OP_APPLY,                                        \
                                                                        \
    &&L_OP_ADD, &&L_OP_SUB, &&L_OP_MUL, &&L_OP_DIV, &&L_OP_IDIV, &&L_OP_NUMEQ, \
    &&L_OP_LT, &&L_OP_COMPARE,                                          \
                                                                        \
    &&L_OP_VECTOR, &&L_OP_AREF, &&L_OP_ASET,                            \
                                                                        \
    &&L_OP_LOADT, &&L_OP_LOADF, &&L_OP_LOADNIL, &&L_OP_LOAD0, &&L_OP_LOAD1, \
    &&L_OP_LOADI8,                                                      \
    &&L_OP_LOADV, &&L_OP_LOADVL,                                        \
    &&L_OP_LOADG, &&L_OP_LOADGL,                                        \
    &&L_OP_LOADA, &&L_OP_LOADAL, &&L_OP_LOADC, &&L_OP_LOADCL,           \
    &&L_OP_SETG, &&L_OP_SETGL,                                          \
    &&L_OP_SETA, &&L_OP_SETAL, NULL, NULL,                              \
                                                                        \
    &&L_OP_CLOSURE, &&L_OP_ARGC, &&L_OP_VARGC, &&L_OP_TRYCATCH,         \
    &&L_OP_FOR,                                                         \
    &&L_OP_TAPPLY, &&L_OP_ADD2, &&L_OP_SUB2, &&L_OP_NEG, &&L_OP_LARGC,  \
    &&L_OP_LVARGC,                                                      \
    &&L_OP_LOADA0, &&L_OP_LOADA1, &&L_OP_LOADC0, &&L_OP_LOADC1,         \
    &&L_OP_CALLL, &&L_OP_TCALLL, &&L_OP_BRNE, &&L_OP_BRNEL, &&L_OP_CADR,\
    &&L_OP_BRNN, &&L_OP_BRNNL, &&L_OP_BRN, &&L_OP_BRNL,                 \
    &&L_OP_OPTARGS, &&L_OP_BRBOUND, &&L_OP_KEYARGS,                     \
    &&L_OP_BOX, &&L_OP_BOXL, &&L_OP_SHIFT                               \
    }

#define VM_APPLY_LABELS                                                 \
    static void *vm_apply_labels[] = {                                  \
NULL, &&L_OP_DUP, &&L_OP_POP, &&L_OP_CALL, &&L_OP_TCALL, &&L_OP_JMP, \
    &&L_OP_BRF, &&L_OP_BRT,                                             \
    &&L_OP_JMPL, &&L_OP_BRFL, &&L_OP_BRTL, &&L_OP_RET,                  \
                                                                        \
    &&L_OP_EQ, &&L_OP_EQV, &&L_OP_EQUAL, &&L_OP_ATOMP, &&L_OP_NOT,      \
    &&L_OP_NULLP, &&L_OP_BOOLEANP,                                      \
    &&L_OP_SYMBOLP, &&L_OP_NUMBERP, &&L_OP_BOUNDP, &&L_OP_PAIRP,        \
    &&L_OP_BUILTINP, &&L_OP_VECTORP,                                    \
    &&L_OP_FIXNUMP, &&L_OP_FUNCTIONP,                                   \
                                                                        \
    &&L_OP_CONS, &&apply_list, &&L_OP_CAR, &&L_OP_CDR, &&L_OP_SETCAR,    \
    &&L_OP_SETCDR, &&apply_apply,                                        \
                                                                        \
    &&apply_add, &&apply_sub, &&apply_mul, &&apply_div, &&L_OP_IDIV, &&L_OP_NUMEQ, \
    &&L_OP_LT, &&L_OP_COMPARE,                                          \
                                                                        \
    &&apply_vector, &&L_OP_AREF, &&L_OP_ASET                            \
    }
#else
#define VM_LABELS
#define VM_APPLY_LABELS
#endif

#endif
