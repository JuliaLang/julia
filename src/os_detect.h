#ifndef OS_DETECT_H
#define OS_DETECT_H

/* This file uses is used by both Julia and C
   After a major refactor, the C parts are now no longer necessary
   and have thus been removed. Should you want to add them again, they
   are avaiable as src/os_detect.h in git commit fbf1348369cb5c79810ff3015ac711c9dcdef2ca */

/* LOGIC PRIMITIVES */

/* These logic primitives may be used to do basic if/else in the C preprocessor
   This can be useful for creating if/else type structures in C and Julia.
   Currently there's support for up to 10 OSs. If you need more add a correspondig JL_BOOL_# */

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
#define JL_TF_0 false
#define JL_TF_1 true
#define JL_TF(x) JL_TF2(x)
#define JL_TF2(x) JL_TF_##x
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
    X(3,Darwin)         XX(1)
#define JL_OS_MAP(X) JL_OS_MAP2(X,NOP)
#define OS_INDEX_MAP(x) x

#if defined(__WIN32__)
    #define OS_CURRENT Windows
#elif defined(__linux__)
    #define OS_CURRENT  Linux
#elif defined(__FreeBSD__)
    #define OS_CURRENT  FreeBSD
#elif defined(__APPLE__)
    #define OS_CURRENT Darwin
#else
    #define OS_CURRENT Unknown
#endif

#ifndef JULIA
/** REMOVED - SEE ABOVE COMMENT */
#else

const OS_NAME = :OS_CURRENT

#define OS_NAME_IFELSE(NUM,NAME) JL_IF(JL_BOOL(NUM),elseif,if) (os==:NAME) return
#define ATTR(IS_UNIX) JL_TF(JL_BOOL(IS_UNIX)); \n
function _jl_is_unix(os::Symbol)
JL_OS_MAP2(OS_NAME_IFELSE,ATTR)
else
error("Unknown Operating System")
end
end

#endif
#endif // OS_DETECT_H
