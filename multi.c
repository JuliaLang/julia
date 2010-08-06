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

extern char *julia_home;

#define JULIA_PORT 9009

static void worker_error(char *msg)
{
    ios_printf(ios_stderr, "%s\n", msg);
    exit(1);
}

void jl_worker_loop(int wrfd);

DLLEXPORT
short jl_start_worker()
{
    int fds[2];
    pipe(fds);
    int rdfd = fds[0];
    int wrfd = fds[1];

    //char cmd[1024];
    //cmd[0]='\0';
    //snprintf(cmd, sizeof(cmd), "%s/julia", julia_home);

    //char *argv[] = { cmd, "-w", NULL };

    if (!fork()) {
        /*
        if (execv(cmd, argv) < 0) {
            worker_error("could not start julia process");
        }
        */
        jl_worker_loop(wrfd);
        exit(0);
    }
    else {
        char buf[2];
        read(rdfd, buf, 2);
        short port = ((short)buf[0]<<8) | ((short)buf[1]);
        ios_printf(ios_stdout, "started worker on port %hu\n", port);
        close(rdfd);

        return port;
    }
}

void jl_worker_loop(int wrfd)
{
    short port = JULIA_PORT;
    int sockfd = open_any_tcp_port(&port);
    if (sockfd == -1)
        worker_error("could not bind socket");

    char buf[2] = { ((unsigned short)port)>>8, port&0xff };
    write(wrfd, buf, 2);
    close(wrfd);

    int connectfd = accept(sockfd, NULL, NULL);

    jl_function_t *wfunc =
        (jl_function_t*)*(jl_get_bindingp(jl_system_module,
                                          jl_symbol("jl_worker")));
    jl_value_t *jfd = jl_box_int32(connectfd);
    jl_apply(wfunc, &jfd, 1);

    close(connectfd);
    close(sockfd);
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
