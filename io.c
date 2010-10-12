/*
  io.c
  I/O utility functions
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

DLLEXPORT
int jl_read_avail(ios_t *s)
{
    int fd = s->fd;
    fd_set fds;
    struct timeval tout;
    tout.tv_sec = 0;
    tout.tv_usec = 0;
    FD_ZERO(&fds);
    FD_SET(fd, &fds);
    select(fd+1, &fds, NULL, NULL, &tout);
    if (FD_ISSET(fd, &fds))
        return 1;
    return 0;
}

// --- io constructors ---

DLLEXPORT
void *jl_new_fdio(int fd)
{
    ios_t *s = (ios_t*)alloc_cobj(sizeof(ios_t));
    ios_fd(s, fd, 0);
    return s;
}

DLLEXPORT
void *jl_new_fileio(char *fname, int rd, int wr, int create, int trunc)
{
    ios_t *s = (ios_t*)alloc_cobj(sizeof(ios_t));
    if (ios_file(s, fname, rd, wr, create, trunc) == NULL)
        jl_errorf("could not open file %s", fname);
    return s;
}

DLLEXPORT
void *jl_new_memio(uint32_t sz)
{
    ios_t *s = (ios_t*)alloc_cobj(sizeof(ios_t));
    if (ios_mem(s, sz) == NULL)
        jl_errorf("error creating memory I/O stream");
    return s;
}

DLLEXPORT
int32_t jl_nb_available(ios_t *s)
{
    return (int32_t)(s->size - s->bpos);
}
