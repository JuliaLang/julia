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

// To be removed once we upgrade libuv
#define uv_stat_t uv_statbuf_t

/** libuv callbacks */

/*
 * Notes for adding new callbacks
 * - Make sure to type annotate the callback, so we'll get the one in the new
 *   Base module rather than the old one.
 *
 */

//These callbacks are implemented in stream.jl
#define JL_CB_TYPES(XX) \
	XX(close) \
	XX(return_spawn) \
	XX(readcb) \
	XX(alloc_buf) \
	XX(connectcb) \
	XX(connectioncb) \
	XX(asynccb) \
    XX(getaddrinfo) \
    XX(pollcb) \
    XX(fspollcb) \
    XX(isopen) \
    XX(fseventscb)
//TODO add UDP and other missing callbacks

#define JULIA_HOOK_(m,hook)  ((jl_function_t*)jl_get_global(m, jl_symbol("_uv_hook_" #hook)))
#define JULIA_HOOK(hook) jl_uvhook_##hook
#define XX(hook) static jl_function_t *JULIA_HOOK(hook) = 0;
JL_CB_TYPES(XX)
#undef XX
DLLEXPORT void jl_get_uv_hooks()
{
    if (JULIA_HOOK(close)) return; // only do this once
#define XX(hook) JULIA_HOOK(hook) = JULIA_HOOK_(jl_base_module, hook);
    JL_CB_TYPES(XX)
#undef XX
}
#undef JL_CB_TYPES

int base_module_conflict = 0; //set to 1 if Base is getting redefined since it means there are two place to try the callbacks
// warning: this is defined without the standard do {...} while (0) wrapper, since I wanted ret to escape
// warning: during bootstrapping, callbacks will be called twice if a MethodError occured at ANY time during callback call
// Use:  JULIA_CB(hook, arg1, numberOfAdditionalArgs, arg2Type, arg2, ..., argNType, argN)
#define JULIA_CB(hook, ...) \
    jl_value_t *ret; \
    if (!base_module_conflict) { \
        ret = jl_callback_call(JULIA_HOOK(hook),__VA_ARGS__); \
    } else { \
        JL_TRY { \
            ret = jl_callback_call(JULIA_HOOK(hook),__VA_ARGS__); \
            /* jl_puts(#hook " original succeeded\n",jl_uv_stderr); */ \
        } \
        JL_CATCH { \
            if (jl_typeof(jl_exception_in_transit) == (jl_value_t*)jl_methoderror_type) { \
                /* jl_puts("\n" #hook " being retried with new Base bindings --> ",jl_uv_stderr); */ \
                jl_function_t *cb_func = JULIA_HOOK_((jl_module_t*)jl_get_global(jl_main_module, jl_symbol("Base")), hook); \
                ret = jl_callback_call(cb_func,__VA_ARGS__); \
                /* jl_puts(#hook " succeeded\n",jl_uv_stderr); */ \
            } else { \
                jl_rethrow(); \
            } \
        } \
    }

jl_value_t *jl_callback_call(jl_function_t *f,jl_value_t *val,int count,...)
{
    if (val != 0)
        count += 1;
    jl_value_t **argv;
    JL_GC_PUSHARGS(argv,count);
    memset(argv, 0, count*sizeof(jl_value_t*));
    jl_value_t *v;
    va_list argp;
    va_start(argp,count);
    int i;
    argv[0]=val;
    for(i=((val==0)?0:1); i<count; ++i) {
        switch(va_arg(argp,int)) {
        case CB_PTR:
            argv[i] = jl_box_voidpointer(va_arg(argp,void*));
            break;
        case CB_INT32:
            argv[i] = jl_box_int32(va_arg(argp,int32_t));
            break;
        case CB_INT64:
            argv[i] = jl_box_int64(va_arg(argp,int64_t));
            break;
        default: jl_error("callback: only Ints and Pointers are supported at this time");
            //excecution never reaches here
            break;
        }
    }
    v = jl_apply(f,(jl_value_t**)argv,count);
    JL_GC_POP();
    return v;
}

void closeHandle(uv_handle_t* handle)
{
    if(handle->data) {
        JULIA_CB(close,handle->data,0); (void)ret;
    }
    free(handle);
}

void shutdownCallback(uv_shutdown_t* req, int status)
{
    uv_close((uv_handle_t*) req->handle,&closeHandle);
    free(req);
}

void jl_return_spawn(uv_process_t *p, int exit_status, int term_signal)
{
    JULIA_CB(return_spawn,p->data,2,CB_INT32,exit_status,CB_INT32,term_signal);
    (void)ret;
}

void jl_readcb(uv_stream_t *handle, ssize_t nread, uv_buf_t buf)
{
    JULIA_CB(readcb,handle->data,3,CB_INT,nread,CB_PTR,(buf.base),CB_INT32,buf.len);
    (void)ret;
}

uv_buf_t jl_alloc_buf(uv_handle_t *handle, size_t suggested_size)
{
    uv_buf_t buf;
    JULIA_CB(alloc_buf,handle->data,1,CB_INT32,suggested_size);
    if (!jl_is_tuple(ret) || !jl_is_pointer(jl_t0(ret)) || !jl_is_int32(jl_t1(ret))) {
        jl_error("jl_alloc_buf: Julia function returned invalid value for buffer allocation callback");
    }
    buf.base = jl_unbox_voidpointer(jl_t0(ret));
    buf.len = jl_unbox_int32(jl_t1(ret));
    return buf;
}

void jl_connectcb(uv_connect_t *connect, int status)
{
    JULIA_CB(connectcb,connect->handle->data,1,CB_INT32,status);
    (void)ret;
}

void jl_connectioncb(uv_stream_t *stream, int status)
{
    JULIA_CB(connectioncb,stream->data,1,CB_INT32,status);
    (void)ret;
}

void jl_getaddrinfocb(uv_getaddrinfo_t *req,int status, struct addrinfo *addr)
{
    JULIA_CB(getaddrinfo,req->data,2,CB_PTR,addr,CB_INT32,status);
    (void)ret;
}

void jl_asynccb(uv_handle_t *handle, int status)
{
    JULIA_CB(asynccb,handle->data,1,CB_INT32,status);
    (void)ret;
}

void jl_pollcb(uv_poll_t *handle, int status, int events)
{
    JULIA_CB(pollcb,handle->data,2,CB_INT32,status,CB_INT32,events)
    (void)ret;
}

void jl_fspollcb(uv_fs_poll_t* handle, int status, const uv_stat_t* prev, const uv_stat_t* curr)
{
    JULIA_CB(fspollcb,handle->data,3,CB_INT32,status,CB_PTR,prev,CB_PTR,curr)
    (void)ret;
}


void jl_fseventscb(uv_fs_event_t* handle, const char* filename, int events, int status)
{
    JULIA_CB(fseventscb,handle->data,3,CB_PTR,filename,CB_INT32,events,CB_INT32,status)
    (void)ret;
}

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

DLLEXPORT void jl_close_uv(uv_handle_t *handle)
{
    if (!handle || uv_is_closing(handle))
       return;
    if (handle->type==UV_TTY)
        uv_tty_set_mode((uv_tty_t*)handle,0);

    if ( (handle->type == UV_NAMED_PIPE || handle->type == UV_TCP) && uv_is_writable( (uv_stream_t *) handle)) { 
        // Make sure that the stream has not already been marked closed in Julia.
        // A double shutdown would cause the process to hang on exit.
        JULIA_CB(isopen, handle->data, 0);
        if (!jl_is_int32(ret)) {
            jl_error("jl_close_uv: _uv_hook_isopen must return an int32.");
        }
        if (!jl_unbox_int32(ret)){
            return;
        }

        uv_shutdown_t *req = malloc(sizeof(uv_shutdown_t));
        int err = uv_shutdown(req, (uv_stream_t*)handle, &shutdownCallback);
        if (err != 0) {
            printf("shutdown err: %s\n", uv_strerror(uv_last_error(jl_global_event_loop())));
            uv_close(handle, &closeHandle);
        }
    }
    else {
        uv_close(handle,&closeHandle);
    }
}

DLLEXPORT void jl_uv_associate_julia_struct(uv_handle_t *handle, jl_value_t *data)
{
    handle->data = data;
}

DLLEXPORT void jl_uv_disassociate_julia_struct(uv_handle_t *handle)
{
    handle->data = NULL;
}

DLLEXPORT int32_t jl_start_reading(uv_stream_t *handle)
{
    if (!handle)
        return -2;
    return uv_read_start(handle,&jl_alloc_buf,&jl_readcb);
}

DLLEXPORT int jl_listen(uv_stream_t* stream, int backlog)
{
    return uv_listen(stream,backlog,&jl_connectioncb);
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
                       uv_handle_type stderr_type, uv_pipe_t *stderr_pipe, int detach)
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
    opts.exit_cb = &jl_return_spawn;
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

DLLEXPORT uv_loop_t *jl_new_event_loop()
{
    return uv_loop_new();
}

DLLEXPORT uv_loop_t *jl_global_event_loop()
{
    return jl_io_loop;
}

DLLEXPORT void jl_async_send(uv_async_t *handle)
{
    if (handle) uv_async_send(handle);
}

DLLEXPORT int jl_idle_start(uv_idle_t *idle)
{
    if (!idle||(idle)->data)
        jl_error("jl_idle_start: Invalid handle");
    return uv_idle_start(idle,(uv_idle_cb)&jl_asynccb);
}

DLLEXPORT int jl_poll_start(uv_poll_t* handle, int32_t events)
{
    return uv_poll_start(handle, events, &jl_pollcb);
}

DLLEXPORT int jl_fs_poll_start(uv_fs_poll_t* handle, char *file, uint32_t interval)
{
    return uv_fs_poll_start(handle,&jl_fspollcb,file,interval);
}

DLLEXPORT int jl_fs_event_init(uv_loop_t* loop, uv_fs_event_t* handle,
    const char* filename, int flags)
{
    return uv_fs_event_init(loop,handle,filename,&jl_fseventscb,flags);
}


DLLEXPORT void jl_uv_req_set_data(uv_req_t *req, void *data)
{
    req->data = data;
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

DLLEXPORT int jl_cwd(char *buffer, size_t size)
{
    return (uv_cwd(buffer,size)).code;
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

DLLEXPORT int jl_getaddrinfo(uv_loop_t *loop, const char *host, const char *service, jl_function_t *cb)
{
    uv_getaddrinfo_t *req = malloc(sizeof(uv_getaddrinfo_t));
    struct addrinfo hints;

    memset (&hints, 0, sizeof (hints));
    hints.ai_family = AF_INET; //ipv4 for now
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags |= AI_CANONNAME;

    req->data = cb;

    return uv_getaddrinfo(loop,req,jl_getaddrinfocb,host,service,&hints);
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

DLLEXPORT int jl_tcp4_connect(uv_tcp_t *handle,uint32_t host, uint16_t port)
{
    struct sockaddr_in addr;
    uv_connect_t *req = malloc(sizeof(uv_connect_t));
    memset(&addr, 0, sizeof(struct sockaddr_in));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = host;
    addr.sin_port = port;
    return uv_tcp_connect(req,handle,addr,&jl_connectcb);
}

DLLEXPORT int jl_connect_raw(uv_tcp_t *handle,struct sockaddr *addr)
{
    uv_connect_t *req = malloc(sizeof(uv_connect_t));
    if (addr->sa_family==AF_INET) {
        return uv_tcp_connect(req,handle,*((struct sockaddr_in*)addr),&jl_connectcb);
    }
    else {
        return uv_tcp_connect6(req,handle,*((struct sockaddr_in6*)addr),&jl_connectcb);
    }
    free(req);
    return -2; //error! Only IPv4 and IPv6 are implemented atm
}

DLLEXPORT int jl_last_errno(uv_loop_t *loop)
{
    return (uv_last_error(loop)).code;
}

DLLEXPORT int jl_last_system_errno(uv_loop_t *loop)
{
    return (uv_last_error(loop)).sys_errno_;
}

DLLEXPORT const char *jl_uv_strerror(int a, int b)
{
    uv_err_t err = {a,b};
    return uv_strerror(err);
}

DLLEXPORT const char *jl_uv_err_name(int a, int b)
{
    uv_err_t err = {a,b};
    return uv_err_name(err);
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
