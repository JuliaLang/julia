/*
  multi.c
  starting and managing multiple processes, concurrency support
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <setjmp.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#include <signal.h>
#include <libgen.h>
#include <unistd.h>
#ifdef BOEHM_GC
#include <gc.h>
#endif
#include "llt.h"
#include "julia.h"

void jl__not__used__()
{
    // force inclusion of lib/socket.o in executable
    short p=0;
    open_any_tcp_port(&p);
}

DLLEXPORT
int jl_wait_msg(int fd)
{
    fd_set fds, efds;
    FD_ZERO(&fds);
    FD_ZERO(&efds);
    FD_SET(fd, &fds);
    FD_SET(fd, &efds);
    select(fd+1, &fds, NULL, &efds, NULL);
    if (FD_ISSET(fd, &efds)) {
        ios_printf(ios_stderr, "error fd\n");
        return 1;
    }
    if (FD_ISSET(fd, &fds)) {
        return 0;
    }
    return 1;
}
