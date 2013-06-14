#ifndef OS_DETECT_H
#define OS_DETECT_H
#include "platform.h"

/* This file is used by Julia */

#if defined(_OS_WINDOWS_)
    #define OS_CURRENT Windows
#elif defined(__linux__)
    #define OS_CURRENT  Linux
#elif defined(__FreeBSD__)
    #define OS_CURRENT  FreeBSD
#elif defined(__APPLE__)
    #define OS_CURRENT Darwin
#else
    #define OS_CURRENT Unknown
    #warning OS_CURRENT is Unknown
#endif
const OS_NAME = :OS_CURRENT

#endif // OS_DETECT_H
