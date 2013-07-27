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
    XX(fseventscb) \
    XX(writecb) \
    XX(writecb_task)
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

DLLEXPORT void jl_uv_closeHandle(uv_handle_t* handle)
{
    if (handle->data) {
        JULIA_CB(close,handle->data,0); (void)ret;
    }
    free(handle);
}

DLLEXPORT void jl_uv_shutdownCallback(uv_shutdown_t* req, int status)
{
    uv_close((uv_handle_t*) req->handle, &jl_uv_closeHandle);
    free(req);
}

DLLEXPORT void jl_uv_return_spawn(uv_process_t *p, int exit_status, int term_signal)
{
    JULIA_CB(return_spawn,p->data,2,CB_INT32,exit_status,CB_INT32,term_signal);
    (void)ret;
}

DLLEXPORT void jl_uv_readcb(uv_stream_t *handle, ssize_t nread, uv_buf_t buf)
{
    JULIA_CB(readcb,handle->data,3,CB_INT,nread,CB_PTR,(buf.base),CB_INT32,buf.len);
    (void)ret;
}

DLLEXPORT uv_buf_t jl_uv_alloc_buf(uv_handle_t *handle, size_t suggested_size)
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

DLLEXPORT void jl_uv_connectcb(uv_connect_t *connect, int status)
{
    JULIA_CB(connectcb,connect->handle->data,1,CB_INT32,status);
    free(connect);
    (void)ret;
}

DLLEXPORT void jl_uv_connectioncb(uv_stream_t *stream, int status)
{
    JULIA_CB(connectioncb,stream->data,1,CB_INT32,status);
    (void)ret;
}

DLLEXPORT void jl_uv_getaddrinfocb(uv_getaddrinfo_t *req,int status, struct addrinfo *addr)
{
    JULIA_CB(getaddrinfo,req->data,2,CB_PTR,addr,CB_INT32,status);
    (void)ret;
}

DLLEXPORT void jl_uv_asynccb(uv_handle_t *handle, int status)
{
    JULIA_CB(asynccb,handle->data,1,CB_INT32,status);
    (void)ret;
}

DLLEXPORT void jl_uv_pollcb(uv_poll_t *handle, int status, int events)
{
    JULIA_CB(pollcb,handle->data,2,CB_INT32,status,CB_INT32,events)
    (void)ret;
}

DLLEXPORT void jl_uv_fspollcb(uv_fs_poll_t* handle, int status, const uv_stat_t* prev, const uv_stat_t* curr)
{
    JULIA_CB(fspollcb,handle->data,3,CB_INT32,status,CB_PTR,prev,CB_PTR,curr)
    (void)ret;
}


DLLEXPORT void jl_uv_fseventscb(uv_fs_event_t* handle, const char* filename, int events, int status)
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

DLLEXPORT int jl_init_pipe(uv_pipe_t *pipe, int writable, int readable, int julia_only, jl_value_t *julia_struct)
{
     int flags = 0;
     flags |= writable ? UV_PIPE_WRITABLE : 0;
     flags |= readable ? UV_PIPE_READABLE : 0;
     if (!julia_only)
         flags |= UV_PIPE_SPAWN_SAFE;
     int err = uv_pipe_init(jl_io_loop, pipe, flags);
     pipe->data = julia_struct;//will be initilized on io
     return err;
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
        int err = uv_shutdown(req, (uv_stream_t*)handle, &jl_uv_shutdownCallback);
        if (err != 0) {
            printf("shutdown err: %s\n", uv_strerror(err));
            uv_close(handle, &jl_uv_closeHandle);
        }
    }
    else {
        uv_close(handle,&jl_uv_closeHandle);
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

DLLEXPORT int jl_listen(uv_stream_t* stream, int backlog)
{
    return uv_listen(stream,backlog,&jl_uv_connectioncb);
}

DLLEXPORT int jl_spawn(char *name, char **argv, uv_loop_t *loop,
                       uv_process_t *proc, jl_value_t *julia_struct,
                       uv_handle_type stdin_type, uv_pipe_t *stdin_pipe,
                       uv_handle_type stdout_type, uv_pipe_t *stdout_pipe,
                       uv_handle_type stderr_type, uv_pipe_t *stderr_pipe, 
                       int detach, char **env)
{
    uv_process_options_t opts;
    uv_stdio_container_t stdio[3];
    int error;
    opts.file = name;
    opts.env = env;
#ifdef _OS_WINDOWS_
    opts.flags = 0;
#else
    opts.flags = UV_PROCESS_RESET_SIGPIPE;
#endif
    opts.cwd = NULL;
    opts.args = argv;
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
    opts.exit_cb = &jl_uv_return_spawn;
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

/*DLLEXPORT uv_loop_t *jl_new_event_loop()
{
    return uv_loop_new();
}*/

DLLEXPORT uv_loop_t *jl_global_event_loop()
{
    return jl_io_loop;
}

DLLEXPORT int jl_poll_start(uv_poll_t* handle, int32_t events)
{
    return uv_poll_start(handle, events, &jl_uv_pollcb);
}

DLLEXPORT int jl_fs_poll_start(uv_fs_poll_t* handle, char *file, uint32_t interval)
{
    return uv_fs_poll_start(handle,&jl_uv_fspollcb,file,interval);
}

DLLEXPORT int jl_fs_event_init(uv_loop_t* loop, uv_fs_event_t* handle,
    const char* filename, int flags)
{
    return uv_fs_event_init(loop,handle,filename,&jl_uv_fseventscb,flags);
}

DLLEXPORT int jl_fs_unlink(char *path)
{
    uv_fs_t req;
    int ret = uv_fs_unlink(jl_io_loop, &req, path, NULL);
    uv_fs_req_cleanup(&req);
    return ret;
}

DLLEXPORT int jl_fs_write(int handle, char *buf, size_t len, size_t offset)
{
    uv_fs_t req;
    int ret = uv_fs_write(jl_io_loop, &req, handle, buf, len, offset, NULL);
    uv_fs_req_cleanup(&req);
    return ret;
}

DLLEXPORT int jl_fs_write_byte(int handle, char c)
{
    uv_fs_t req;
    int ret = uv_fs_write(jl_io_loop, &req, handle, &c, 1, -1, NULL);
    uv_fs_req_cleanup(&req);
    return ret;
}

DLLEXPORT int jl_fs_read(int handle, char *buf, size_t len)
{
    uv_fs_t req;
    int ret = uv_fs_read(jl_io_loop, &req, handle, buf, len, -1, NULL);
    uv_fs_req_cleanup(&req);
    return ret;
}

DLLEXPORT int jl_fs_read_byte(int handle)
{
    uv_fs_t req;
    char buf;
    int ret = uv_fs_read(jl_io_loop, &req, handle, &buf, 1, -1, NULL);
    uv_fs_req_cleanup(&req);
    if (ret == -1)
        return ret;
    return (int)buf;
}

DLLEXPORT int jl_fs_close(int handle)
{
    uv_fs_t req;
    int ret = uv_fs_close(jl_io_loop, &req, handle, NULL);
    uv_fs_req_cleanup(&req);
    return ret;
}

//units are in ms
DLLEXPORT int jl_puts(char *str, uv_stream_t *stream)
{
    return jl_write(stream,str,strlen(str));
}

DLLEXPORT void jl_uv_writecb(uv_write_t* req, int status)
{
    JULIA_CB(writecb, req->handle->data, 2, CB_PTR, req, CB_INT32, status)
    free(req);
    (void)ret;
}

DLLEXPORT void jl_uv_writecb_task(uv_write_t* req, int status)
{
    JULIA_CB(writecb_task, req->handle->data, 2, CB_PTR, req, CB_INT32, status)
    free(req);
    (void)ret;
}

DLLEXPORT int jl_write_copy(uv_stream_t *stream, const char *str, size_t n, uv_write_t *uvw, void *writecb)
{
    JL_SIGATOMIC_BEGIN();
    char *data = (char*)(uvw+1);
    memcpy(data,str,n);
    uv_buf_t buf[]  = {{.base = data,.len=n}};
    uvw->data = NULL;
    int err = uv_write(uvw,stream,buf,1,writecb);  
    JL_SIGATOMIC_END();
    return err;
}

DLLEXPORT int jl_putc(unsigned char c, uv_stream_t *stream)
{
    int err;
    if (stream!=0) {
        if (stream->type<UV_HANDLE_TYPE_MAX) { //is uv handle
            if (stream->type == UV_FILE) {
                JL_SIGATOMIC_BEGIN();
                jl_uv_file_t *file = (jl_uv_file_t *)stream;
                // Do a blocking write for now
                uv_fs_t req;
                err = uv_fs_write(file->loop, &req, file->file, &c, 1, -1, NULL);
                JL_SIGATOMIC_END();
                return err ? 0 : 1;
            }
            else {
                uv_write_t *uvw = malloc(sizeof(uv_write_t)+1);
                err = jl_write_copy(stream,(char*)&c,1,uvw,&jl_uv_writecb);
                if (err < 0) {
                    free(uvw);
                    return 0;
                }
                return 1;
            }
        }
        else {
            ios_t *handle = (ios_t*)stream;
            return ios_putc(c,handle);
        }
    }
    return 0;
}

DLLEXPORT int jl_write_no_copy(uv_stream_t *stream, char *data, size_t n, uv_write_t *uvw, void *writecb)
{
    uv_buf_t buf[]  = {{.base = data,.len=n}};
    JL_SIGATOMIC_BEGIN();
    int err = uv_write(uvw,stream,buf,1,writecb);
    uvw->data = NULL;
    JL_SIGATOMIC_END();
    return err;
}

DLLEXPORT int jl_putc_copy(unsigned char c, uv_stream_t *stream, void *uvw, void *writecb)
{
    return jl_write_copy(stream,(char *)&c,1,uvw,writecb);
}

DLLEXPORT int jl_pututf8(uv_stream_t *s, uint32_t wchar )
{
    char buf[8];
    if (wchar < 0x80)
        return jl_putc((int)wchar, s);
    size_t n = u8_toutf8(buf, 8, &wchar, 1);
    return jl_write(s, buf, n);
}

DLLEXPORT int jl_pututf8_copy(uv_stream_t *s, uint32_t wchar, void *uvw, void *writecb)
{
    char buf[8];
    if (wchar < 0x80)
        return jl_putc_copy((int)wchar, s, uvw, writecb);
    size_t n = u8_toutf8(buf, 8, &wchar, 1);
    return jl_write_copy(s, buf, n, uvw, writecb);
}

DLLEXPORT size_t jl_write(uv_stream_t *stream, const char *str, size_t n)
{
    int err;
    //TODO: BAD!! Needed because Julia can't yet detect null stdio
    if (stream == 0)
        return 0;
    if (stream->type<UV_HANDLE_TYPE_MAX) { //is uv handle
        if (stream->type == UV_FILE) {
            JL_SIGATOMIC_BEGIN();
            jl_uv_file_t *file = (jl_uv_file_t *)stream;
            // Do a blocking write for now
            uv_fs_t req;
            err = uv_fs_write(file->loop, &req, file->file, (void*)str, n, -1, NULL);
            JL_SIGATOMIC_END();
            return err ? 0 : n;
        }
        else {
            uv_write_t *uvw = malloc(sizeof(uv_write_t)+n);
            err = jl_write_copy(stream,str,n,uvw,&jl_uv_writecb);
            if (err < 0) {
                free(uvw);
                return 0;
            }
            return n;
        }
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

DLLEXPORT int jl_cwd(char *buffer, size_t size)
{
    return uv_cwd(buffer,size);
}

DLLEXPORT int jl_getpid()
{
#ifdef _OS_WINDOWS_
    return GetCurrentProcessId();
#else
    return getpid();
#endif
}

//NOTE: These function expects port/host to be in network byte-order (Big Endian)
DLLEXPORT int jl_tcp_bind(uv_tcp_t* handle, uint16_t port, uint32_t host)
{
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(struct sockaddr_in));
    addr.sin_port = port;
    addr.sin_addr.s_addr = host;
    addr.sin_family = AF_INET;
    return uv_tcp_bind(handle, addr);
}
DLLEXPORT int jl_tcp_bind6(uv_tcp_t* handle, uint16_t port, void *host)
{
    struct sockaddr_in6 addr;
    memset(&addr, 0, sizeof(struct sockaddr_in6));
    addr.sin6_port = port;
    memcpy(&addr.sin6_addr, host, 16);
    addr.sin6_family = AF_INET6;
    return uv_tcp_bind6(handle, addr);
}

DLLEXPORT int jl_uv_sizeof_interface_address()
{
    return sizeof(uv_interface_address_t);
}

DLLEXPORT int jl_uv_interface_addresses(uv_interface_address_t **ifAddrStruct,int *count)
{
    return uv_interface_addresses(ifAddrStruct,count);
}

DLLEXPORT int jl_uv_interface_address_is_internal(uv_interface_address_t *addr)
{
    return addr->is_internal;
}

DLLEXPORT struct sockaddr_in *jl_uv_interface_address_sockaddr(uv_interface_address_t *ifa)
{
    return &ifa->address.address4;
}

DLLEXPORT int jl_getaddrinfo(uv_loop_t *loop, const char *host, const char *service, jl_function_t *cb)
{
    uv_getaddrinfo_t *req = malloc(sizeof(uv_getaddrinfo_t));
    struct addrinfo hints;

    memset (&hints, 0, sizeof (hints));
    hints.ai_family = PF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags |= AI_CANONNAME;

    req->data = cb;

    return uv_getaddrinfo(loop,req,jl_uv_getaddrinfocb,host,service,&hints);
}

DLLEXPORT struct sockaddr *jl_sockaddr_from_addrinfo(struct addrinfo *addrinfo)
{
    return addrinfo->ai_addr;
}
DLLEXPORT struct addrinfo *jl_next_from_addrinfo(struct addrinfo *addrinfo)
{
    return addrinfo->ai_next;
}

DLLEXPORT int jl_sockaddr_in_is_ip4(struct sockaddr_in *addr)
{
    return (addr->sin_family==AF_INET);
}

DLLEXPORT int jl_sockaddr_in_is_ip6(struct sockaddr_in *addr)
{
    return (addr->sin_family==AF_INET6);
}

DLLEXPORT int jl_sockaddr_is_ip4(struct sockaddr_storage *addr)
{
    return (addr->ss_family==AF_INET);
}

DLLEXPORT int jl_sockaddr_is_ip6(struct sockaddr_storage *addr)
{
    return (addr->ss_family==AF_INET6);
}

DLLEXPORT unsigned int jl_sockaddr_host4(struct sockaddr_in *addr)
{
    return addr->sin_addr.s_addr;
}

DLLEXPORT unsigned int jl_sockaddr_host6(struct sockaddr_in6 *addr, char *host)
{
    memcpy(host, &addr->sin6_addr, 16);
    return addr->sin6_scope_id;
}

DLLEXPORT void jl_sockaddr_set_port(struct sockaddr_storage *addr,uint16_t port)
{
    if (addr->ss_family==AF_INET) {
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
    req->data = 0;
    memset(&addr, 0, sizeof(struct sockaddr_in));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = host;
    addr.sin_port = port;
    return uv_tcp_connect(req,handle,addr,&jl_uv_connectcb);
}

DLLEXPORT int jl_tcp6_connect(uv_tcp_t *handle, void *host, uint16_t port)
{
    struct sockaddr_in6 addr;
    uv_connect_t *req = malloc(sizeof(uv_connect_t));
    req->data = 0;
    memset(&addr, 0, sizeof(struct sockaddr_in6));
    addr.sin6_family = AF_INET6;
    memcpy(&addr.sin6_addr, host, 16);
    addr.sin6_port = port;
    return uv_tcp_connect6(req,handle,addr,&jl_uv_connectcb);
}

DLLEXPORT int jl_connect_raw(uv_tcp_t *handle,struct sockaddr_storage *addr)
{
    uv_connect_t *req = malloc(sizeof(uv_connect_t));
    req->data = 0;
    if (addr->ss_family==AF_INET) {
        return uv_tcp_connect(req,handle,*((struct sockaddr_in*)addr),&jl_uv_connectcb);
    }
    else {
        return uv_tcp_connect6(req,handle,*((struct sockaddr_in6*)addr),&jl_uv_connectcb);
    }
    free(req);
    return -2; //error! Only IPv4 and IPv6 are implemented atm
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

#ifndef _OS_WINDOWS_

DLLEXPORT int jl_uv_unix_fd_is_watched(int fd, uv_poll_t *handle, uv_loop_t *loop)
{
    if (fd > loop->nwatchers)
        return 0;
    if (loop->watchers[fd] == NULL)
        return 0;
    if (loop->watchers[fd] == &handle->io_watcher)
        return 0;
    return 1;
}

#endif

DLLEXPORT uv_handle_type jl_uv_handle_type(uv_handle_t *handle)
{
    return handle->type;
}

DLLEXPORT void jl_uv_req_set_data(uv_req_t *req, void *data)
{
    req->data = data;
}
 
 
DLLEXPORT void *jl_uv_req_data(uv_req_t *req)
{
    return req->data;
}

DLLEXPORT void *jl_uv_handle_data(uv_handle_t *handle)
{
    return handle->data;
}

#ifdef __cplusplus
}
#endif
