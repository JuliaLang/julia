#ifndef OS_DETECT_H
#define OS_DETECT_H

/* This file uses is used by both Julia and C */

/* LOGIC PRIMITIVES */
#define JL_BOOL_0 0
#define JL_BOOL_1 1
#define JL_BOOL_2 1
#define JL_BOOL_3 1
#define JL_BOOL_4 1
#define JL_BOOL_5 1
#define JL_BOOL_6 1
#define JL_BOOL_7 1
#define JL_BOOL_8 1
#define JL_BOOL_9 1
#define JL_BOOL(x) JL_BOOL_##x
#define I(x) x

#define JL_IF_0(T,F) F
#define JL_IF_1(T,F) T
#define JL_IF2(C,T,F) JL_IF_##C(T,F)
#define JL_IF(C,T,F) JL_IF2(C,T,F)

/* OS MAP - to add an OS just append entry to map.
All other functions will be updated automagically, but detection by variables must be added to jl_current_os
X(NUM,C-Var,Julia name) - General INFO
XX(ISUNIX)              - OS Traits
  */
#define NUM_OS = 4
#define NOP(x)
#define JL_OS_MAP2(X,XX)        \
    X(0,Windows)        XX(0)   \
    X(1,Linux)          XX(1)   \
    X(2,FreeBSD)        XX(1)   \
    X(3,OSX)           XX(1)
#define JL_OS_MAP(X) JL_OS_MAP2(X,NOP)
#define OS_INDEX_MAP(x) x

#if defined(__WIN32__)
    #define OS_CURRENT Windows
#elif defined(__linux)
    #define OS_CURRENT  Linux
#elif defined(__FreeBSD__)
    #define OS_CURRENT  FreeBSD
#elif defined(__APPLE__)
    #define OS_CURRENT OSX
#else
    #define OS_CURRENT Unknown
#endif

#ifndef JULIA
#define OS_GEN(NUM,NAME) NAME=NUM,
enum OS {
    JL_OS_MAP(OS_GEN)
};

#undef OS_GEN
#define OS_NAME_GEN(NUM,NAME) #NAME ,
const char *OS_NAME[NUM_OS] {
    JL_OS_MAP(OS_NAME_GEN)
};
#undef OS_NAME_GEN
#define OS_CUR_GEN(NUM,NAME) JL_IF(VAR,return NUM,)
DLLEXPORT const unsigned char jl_current_os() const
{

}

#define OS_CASE(NUM,NAME) case NUM:
#define SWITCH_BODY(num,CASE,BODY)  \
{                                   \
    switch(num) {                   \
    JL_OS_MAP2(OS_CASE,MACRO)       \
    }                               \
}                                   \

#define OS_ISUNIX(ISUNIX) return ISUNIX;
DLLEXPORT char jl_is_unix(const unsigned char osnum)
SWITCH_BODY(osnum,OS_CASE,OS_ISUNIX)

const char *jl_os_name(const unsigned char osnum) const
{
    return OS_NAME[OS_INDEX_MAP(osnum)]
}
#else
#define PREFIX2(NAME) JL_OS_##NAME
#define PREFIX(NAME) PREFIX2(NAME)
#define CONSTANT_MAP(NUM,NAME) const JL_OS_##NAME = uint8(NUM);\n
JL_OS_MAP(CONSTANT_MAP)
const CURRENT_OS = PREFIX(OS_CURRENT);

#define OS_NAME_IFELSE(NUM,NAME) JL_IF(JL_BOOL(NUM),elseif,if) (osnum==PREFIX(NAME)) return #NAME; \n
function _jl_os_name(osnum::Uint8)
JL_OS_MAP(OS_NAME_IFELSE)
else
return "Unknown"
end
end
_jl_os_name(osnum::Integer) = _jl_os_name(uint8(osnum))
#endif
#endif // OS_DETECT_H
