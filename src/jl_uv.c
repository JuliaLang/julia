#include "platform.h"

/*
 * There is no need to define WINVER because it is already defined in Makefile.
 */
#if defined(_COMPILER_MINGW_)
#define WINVER                 _WIN32_WINNT
#define _WIN32_WINDOWS         _WIN32_WINNT
#endif

#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#ifdef _OS_WINDOWS_
#include <ws2tcpip.h>
#include <malloc.h>
#else
#include "errno.h"
#include <unistd.h>
#include <sys/socket.h>
#endif

#include "julia.h"
#include "support/ios.h"
#include "uv.h"

#ifdef __cplusplus
#include <cstring>
extern "C" {
#endif

/** This file contains wrappers for most of libuv's stream functionailty. Once we can allocate structs in Julia, this file will be removed */

DLLEXPORT int jl_run_once(uv_loop_t *loop)
{
    loop->stop_flag = 0;
    if (loop) return uv_run(loop,UV_RUN_ONCE);
    else return 0;
}

DLLEXPORT void jl_run_event_loop(uv_loop_t *loop)
{
    loop->stop_flag = 0;
    if (loop) uv_run(loop,UV_RUN_DEFAULT);
}

DLLEXPORT int jl_process_events(uv_loop_t *loop)
{
    loop->stop_flag = 0;
    if (loop) return uv_run(loop,UV_RUN_NOWAIT);
    else return 0;
}

DLLEXPORT void jl_uv_associate_julia_struct(uv_handle_t *handle, jl_value_t *data)
{
    handle->data = data;
}

DLLEXPORT void jl_uv_req_set_data(uv_req_t *req, void *data)
{
    req->data = data;
}

DLLEXPORT void jl_uv_disassociate_julia_struct(uv_handle_t *handle)
{
    handle->data = NULL;
}

DLLEXPORT void *jl_uv_handle_data(uv_handle_t *handle)
{
    return handle->data;
}

DLLEXPORT void *jl_uv_req_data(uv_req_t *req)
{
    return req->data;
}

#ifdef __APPLE__
#include <crt_externs.h>
#else
#if defined(_COMPILER_MINGW_)
extern char **environ;
#endif
#endif

DLLEXPORT int jl_spawn(char *name, char **argv, uv_loop_t *loop,
                       uv_process_t *proc, jl_value_t *julia_struct,
                       uv_handle_type stdin_type, uv_pipe_t *stdin_pipe,
                       uv_handle_type stdout_type, uv_pipe_t *stdout_pipe,
                       uv_handle_type stderr_type, uv_pipe_t *stderr_pipe, 
                       int detach, void *return_cb)
{
#ifdef __APPLE__
    char **environ = *_NSGetEnviron();
#endif
    uv_process_options_t opts;
    uv_stdio_container_t stdio[3];
    int error;
    opts.file = name;
#ifndef _OS_WINDOWS_
    opts.env = environ;
#else
    opts.env = NULL;
#endif
    opts.cwd = NULL;
    opts.args = argv;
    opts.flags = 0;
    if (detach)
        opts.flags |= UV_PROCESS_DETACHED;
    opts.stdio = stdio;
    opts.stdio_count = 3;
    stdio[0].type = stdin_type;
    stdio[0].data.stream = (uv_stream_t*)(stdin_pipe);
    stdio[1].type = stdout_type;
    stdio[1].data.stream = (uv_stream_t*)(stdout_pipe);
    stdio[2].type = stderr_type;
    stdio[2].data.stream = (uv_stream_t*)(stderr_pipe);
    //opts.detached = 0; #This has been removed upstream to be uncommented once it is possible again
    opts.exit_cb = return_cb;
    error = uv_spawn(loop,proc,opts);
    return error;
}

#ifdef _OS_WINDOWS_
#include <time.h>
DLLEXPORT struct tm* localtime_r(const time_t *t, struct tm *tm)
{
    auto struct tm *tmp = localtime(t); //localtime is reentrant on windows
    if (tmp)
        *tm = *tmp;
    return tmp;
}
#endif

DLLEXPORT int jl_puts(char *str, uv_stream_t *stream)
{
    return jl_write(stream,str,strlen(str));
}

DLLEXPORT int jl_pututf8(uv_stream_t *s, uint32_t wchar )
{
    char buf[8];
    if (wchar < 0x80)
        return jl_putc((int)wchar, s);
    size_t n = u8_toutf8(buf, 8, &wchar, 1);
    return jl_write(s, buf, n);
}

static char chars[] = {
      0,  1,  2,  3,  4,  5,  6,  7,
      8,  9, 10, 11, 12, 13, 14, 15,
     16, 17, 18, 19, 20, 21, 22, 23,
     24, 25, 26, 27, 28, 29, 30, 31,
     32, 33, 34, 35, 36, 37, 38, 39,
     40, 41, 42, 43, 44, 45, 46, 47,
     48, 49, 50, 51, 52, 53, 54, 55,
     56, 57, 58, 59, 60, 61, 62, 63,
     64, 65, 66, 67, 68, 69, 70, 71,
     72, 73, 74, 75, 76, 77, 78, 79,
     80, 81, 82, 83, 84, 85, 86, 87,
     88, 89, 90, 91, 92, 93, 94, 95,
     96, 97, 98, 99,100,101,102,103,
    104,105,106,107,108,109,110,111,
    112,113,114,115,116,117,118,119,
    120,121,122,123,124,125,126,127,
    128,129,130,131,132,133,134,135,
    136,137,138,139,140,141,142,143,
    144,145,146,147,148,149,150,151,
    152,153,154,155,156,157,158,159,
    160,161,162,163,164,165,166,167,
    168,169,170,171,172,173,174,175,
    176,177,178,179,180,181,182,183,
    184,185,186,187,188,189,190,191,
    192,193,194,195,196,197,198,199,
    200,201,202,203,204,205,206,207,
    208,209,210,211,212,213,214,215,
    216,217,218,219,220,221,222,223,
    224,225,226,227,228,229,230,231,
    232,233,234,235,236,237,238,239,
    240,241,242,243,244,245,246,247,
    248,249,250,251,252,253,254,255
};

static void jl_free_buffer(uv_write_t* req, int status)
{
    free(req);
}

DLLEXPORT int jl_putc(unsigned char c, uv_stream_t *stream)
{
    if (stream!=0) {
        if (stream->type<UV_HANDLE_TYPE_MAX) { //is uv handle
            JL_SIGATOMIC_BEGIN();
            uv_write_t *uvw = malloc(sizeof(uv_write_t));
            uvw->data=0;
            uv_buf_t buf[]  = {{.base = chars+c,.len=1}};
            int err = uv_write(uvw,stream,buf,1,&jl_free_buffer);
            JL_SIGATOMIC_END();
            return err ? 0 : 1;
        }
        else {
            ios_t *handle = (ios_t*)stream;
            return ios_putc(c,handle);
        }
    }
    return 0;
}

DLLEXPORT size_t jl_write(uv_stream_t *stream, const char *str, size_t n)
{
    //TODO: BAD!! Needed because Julia can't yet detect null stdio
    if (stream == 0)
        return 0;
    if (stream->type<UV_HANDLE_TYPE_MAX) { //is uv handle
        JL_SIGATOMIC_BEGIN();
        uv_write_t *uvw = malloc(sizeof(uv_write_t)+n);
        char *data = (char*)(uvw+1);
        memcpy(data,str,n);
        uv_buf_t buf[]  = {{.base = data,.len=n}};
        uvw->data = NULL;
        int err = uv_write(uvw,stream,buf,1,&jl_free_buffer);
        JL_SIGATOMIC_END();
        return err ? 0 : n;
    }
    else {
        ios_t *handle = (ios_t*)stream;
        return ios_write(handle,str,n);
    }
}

extern int vasprintf(char **str, const char *fmt, va_list ap);

int jl_vprintf(uv_stream_t *s, const char *format, va_list args)
{
    char *str=NULL;
    int c;
    va_list al;
#if defined(_OS_WINDOWS_) && !defined(_COMPILER_MINGW_)
    al = args;
#else
    va_copy(al, args);
#endif

    c = vasprintf(&str, format, al);

    if (c >= 0) {
        jl_write(s, str, c);
        LLT_FREE(str);
    }
    va_end(al);
    return c;
}

int jl_printf(uv_stream_t *s, const char *format, ...)
{
    va_list args;
    int c;

    va_start(args, format);
    c = jl_vprintf(s, format, args);
    va_end(args);
    return c;
}

char *jl_bufptr(ios_t *s)
{
    return s->buf;
}

DLLEXPORT void uv_atexit_hook();
DLLEXPORT void jl_exit(int exitcode)
{
    /*if (jl_io_loop) {
        jl_process_events(&jl_io_loop);
    }*/
    uv_tty_reset_mode();
    uv_atexit_hook();
    exit(exitcode);
}

//NOTE: This function expects port/host to be in network byte-order (Big Endian)
DLLEXPORT int jl_tcp_bind(uv_tcp_t* handle, uint16_t port, uint32_t host)
{
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(struct sockaddr_in));
    addr.sin_port = port;
    addr.sin_addr.s_addr = host;
    addr.sin_family = AF_INET;
    int err = uv_tcp_bind(handle,addr);
    return err;
}

DLLEXPORT void getlocalip(char *buf, size_t len)
{
    uv_err_t err;
    uv_interface_address_t * ifAddrStruct=NULL;
    struct sockaddr_in ifa;
    void * tmpAddrPtr=NULL;
    int count=0;

    err = uv_interface_addresses(&ifAddrStruct,&count);
    if (err.code!=0)
        if (ifAddrStruct!=NULL)
            uv_free_interface_addresses(ifAddrStruct,count);

    for (int i = 0; i < count; i++) {
        ifa = (ifAddrStruct+i)->address.address4;
        if (ifa.sin_family==AF_INET) { // check it is IP4
            // is a valid IP4 Address
#ifndef _OS_WINDOWS_
            tmpAddrPtr=&(ifa.sin_addr);
            inet_ntop(AF_INET, tmpAddrPtr, buf, len); //Not available on WinXP
#else
            strncpy(buf,inet_ntoa(ifa.sin_addr),len-1);
            buf[len]=0;
#endif

            if (strcmp(buf,"127.0.0.1")) //TODO: use (ifa.internal == false)
                break;
            //printf("%s IP Address %s\n", ifa->ifa_name, addressBuffer);
        }
        /*
        else if (ifa->ifa_addr && ifa->ifa_addr->sa_family==AF_INET6) { // check it is IP6
            // is a valid IP6 Address
            tmpAddrPtr=&((struct sockaddr_in *)ifa->ifa_addr)->sin_addr;
            char addressBuffer[INET6_ADDRSTRLEN];
            inet_ntop(AF_INET6, tmpAddrPtr, addressBuffer, INET6_ADDRSTRLEN);
            printf("%s IP Address %s\n", ifa->ifa_name, addressBuffer);
        }
        */
    }
    if (ifAddrStruct!=NULL) uv_free_interface_addresses(ifAddrStruct,count);
}

DLLEXPORT int jl_getaddrinfo(uv_loop_t *loop, const char *host, const char *service, void *data, void *cb)
{
    uv_getaddrinfo_t *req = malloc(sizeof(uv_getaddrinfo_t));
    struct addrinfo hints;

    memset (&hints, 0, sizeof (hints));
    hints.ai_family = AF_INET; //ipv4 for now
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags |= AI_CANONNAME;

    req->data = data;

    return uv_getaddrinfo(loop,req,cb,host,service,&hints);
}

DLLEXPORT struct sockaddr *jl_sockaddr_from_addrinfo(struct addrinfo *addrinfo)
{
    struct sockaddr *addr = malloc(sizeof(struct sockaddr));
    memcpy(addr,addrinfo->ai_addr,sizeof(struct sockaddr));
    return addr;
}

DLLEXPORT int jl_sockaddr_is_ip4(struct sockaddr *addr)
{
    return (addr->sa_family==AF_INET);
}

DLLEXPORT unsigned int jl_sockaddr_host4(struct sockaddr *addr)
{
    return ((struct sockaddr_in*)addr)->sin_addr.s_addr;
}


DLLEXPORT void jl_sockaddr_set_port(struct sockaddr *addr,uint16_t port)
{
    if (addr->sa_family==AF_INET) {
        ((struct sockaddr_in*)addr)->sin_port=port;
    }
    else {
        ((struct sockaddr_in6*)addr)->sin6_port=port;
    }
}

DLLEXPORT int jl_tcp4_connect(uv_tcp_t *handle,uint32_t host, uint16_t port,void *data,void *callback)
{
    struct sockaddr_in addr;
    uv_connect_t *req = malloc(sizeof(uv_connect_t));
    req->data = data;
    memset(&addr, 0, sizeof(struct sockaddr_in));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = host;
    addr.sin_port = port;
    return uv_tcp_connect(req,handle,addr,callback);
}

DLLEXPORT char *jl_ios_buf_base(ios_t *ios)
{
    return ios->buf;
}

DLLEXPORT uv_lib_t *jl_wrap_raw_dl_handle(void *handle)
{
    uv_lib_t *lib = malloc(sizeof(uv_lib_t));
    lib->handle=handle;
    lib->errmsg=NULL;
    return lib;
}

#ifdef __cplusplus
}
#endif
