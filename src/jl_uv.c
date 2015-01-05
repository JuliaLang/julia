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
#include <assert.h>

#ifdef _OS_WINDOWS_
#include <ws2tcpip.h>
#include <malloc.h>
#else
#include "errno.h"
#include <unistd.h>
#include <sys/socket.h>
#endif

#include "julia.h"
#include "julia_internal.h"
#include "support/ios.h"
#include "uv.h"

#ifdef __cplusplus
#include <cstring>
extern "C" {
#endif

/** libuv callbacks */

enum CALLBACK_TYPE { CB_PTR, CB_INT32, CB_UINT32, CB_INT64, CB_UINT64 };
#ifdef _P64
#define CB_INT CB_INT64
#define CB_UINT CB_UINT64
#else
#define CB_INT CB_INT32
#define CB_UINT CB_UINT32
#endif

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
    XX(writecb_task) \
    XX(recv) \
    XX(send)
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

extern jl_module_t *jl_old_base_module;
// Use:  JULIA_CB(hook, arg1, numberOfAdditionalArgs, arg2Type, arg2, ..., argNType, argN)
#define JULIA_CB(hook,val, ...) \
    (!jl_old_base_module ? ( \
        jl_callback_call(JULIA_HOOK(hook),(jl_value_t*)val,__VA_ARGS__) \
    ) : ( \
        jl_callback_call( \
            JULIA_HOOK_(jl_base_relative_to(((jl_datatype_t*)jl_typeof(val))->name->module), hook), \
            (jl_value_t*)val,__VA_ARGS__) \
    ))

jl_value_t *jl_callback_call(jl_function_t *f,jl_value_t *val,int count,...)
{
    if (val != 0)
        count += 1;
    else
        return NULL;
    jl_value_t **argv;
    JL_GC_PUSHARGS(argv,count);
    memset(argv, 0, count*sizeof(jl_value_t*));
    jl_value_t *v;
    va_list argp;
    va_start(argp,count);
    int i;
    assert(val != 0);
    argv[0]=val;
    for(i=((val==0)?0:1); i<count; ++i) {
        switch(va_arg(argp,int)) {
        case CB_PTR:
            argv[i] = jl_box_voidpointer(va_arg(argp,void*));
            break;
        case CB_INT32:
            argv[i] = jl_box_int32(va_arg(argp,int32_t));
            break;
        case CB_UINT32:
            argv[i] = jl_box_uint32(va_arg(argp,uint32_t));
            break;
        case CB_INT64:
            argv[i] = jl_box_int64(va_arg(argp,int64_t));
            break;
        case CB_UINT64:
            argv[i] = jl_box_uint64(va_arg(argp,uint64_t));
            break;
        default: jl_error("callback: only Ints and Pointers are supported at this time");
            //excecution never reaches here
            break;
        }
    }
    va_end(argp);
    v = jl_apply(f,(jl_value_t**)argv,count);
    JL_GC_POP();
    return v;
}

DLLEXPORT void jl_uv_closeHandle(uv_handle_t *handle)
{
    if (handle->data) {
        JULIA_CB(close,handle->data,0);
    }
    free(handle);
}

DLLEXPORT void jl_uv_shutdownCallback(uv_shutdown_t *req, int status)
{
    /*
     * This happens if the remote machine closes the connecition while we're
     * in the shutdown request (in that case we call uv_close, thus cancelling this)
     * request.
     */
    if (status != UV__ECANCELED && !uv_is_closing((uv_handle_t*)req->handle)) {
        uv_close((uv_handle_t*)req->handle, &jl_uv_closeHandle);
    }
    free(req);
}

DLLEXPORT void jl_uv_return_spawn(uv_process_t *p, int64_t exit_status, int term_signal)
{
    JULIA_CB(return_spawn,p->data,2,CB_INT64,exit_status,CB_INT32,term_signal);
}

DLLEXPORT void jl_uv_readcb(uv_stream_t *handle, ssize_t nread, const uv_buf_t *buf)
{
    JULIA_CB(readcb,handle->data,3,CB_INT,nread,CB_PTR,(buf->base),CB_UINT,buf->len);
}

DLLEXPORT void jl_uv_alloc_buf(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf)
{
    if (handle->data) {
        jl_value_t *ret = JULIA_CB(alloc_buf,handle->data,1,CB_UINT,suggested_size);
        assert(jl_is_tuple(ret) && jl_is_pointer(jl_t0(ret)));
        buf->base = (char*)jl_unbox_voidpointer(jl_t0(ret));
#ifdef _P64
        assert(jl_is_uint64(jl_t1(ret)));
        buf->len = jl_unbox_uint64(jl_t1(ret));
#else
        assert(jl_is_uint32(jl_t1(ret)));
        buf->len = jl_unbox_uint32(jl_t1(ret));
#endif
    }
    else {
        buf->len = 0;
    }
}

DLLEXPORT void jl_uv_connectcb(uv_connect_t *connect, int status)
{
    JULIA_CB(connectcb,connect->handle->data,1,CB_INT32,status);
    free(connect);
}

DLLEXPORT void jl_uv_connectioncb(uv_stream_t *stream, int status)
{
    JULIA_CB(connectioncb,stream->data,1,CB_INT32,status);
}

DLLEXPORT void jl_uv_getaddrinfocb(uv_getaddrinfo_t *req,int status, struct addrinfo *addr)
{
    JULIA_CB(getaddrinfo,req->data,2,CB_PTR,addr,CB_INT32,status);
}

DLLEXPORT void jl_uv_asynccb(uv_handle_t *handle)
{
    JULIA_CB(asynccb,handle->data,0);
}

DLLEXPORT void jl_uv_pollcb(uv_poll_t *handle, int status, int events)
{
    JULIA_CB(pollcb,handle->data,2,CB_INT32,status,CB_INT32,events);
}

DLLEXPORT void jl_uv_fspollcb(uv_fs_poll_t *handle, int status, const uv_stat_t *prev, const uv_stat_t *curr)
{
    JULIA_CB(fspollcb,handle->data,3,CB_INT32,status,CB_PTR,prev,CB_PTR,curr);
}


DLLEXPORT void jl_uv_fseventscb(uv_fs_event_t *handle, const char *filename, int events, int status)
{
    JULIA_CB(fseventscb,handle->data,3,CB_PTR,filename,CB_INT32,events,CB_INT32,status);
}

DLLEXPORT void jl_uv_recvcb(uv_udp_t *handle, ssize_t nread, const uv_buf_t *buf, struct sockaddr* addr, unsigned flags)
{
    JULIA_CB(recv,handle->data,5,CB_INT,nread,CB_PTR,(buf->base),CB_UINT,buf->len,CB_PTR,addr,CB_INT32,flags);
}

DLLEXPORT void jl_uv_sendcb(uv_udp_send_t *handle, int status)
{
    JULIA_CB(send,handle->data,1,CB_INT32,status);
    free(handle);
}

/** This file contains wrappers for most of libuv's stream functionailty. Once we can allocate structs in Julia, this file will be removed */

DLLEXPORT int jl_run_once(uv_loop_t *loop)
{
    if (loop) {
        loop->stop_flag = 0;
        return uv_run(loop,UV_RUN_ONCE);
    }
    else return 0;
}

DLLEXPORT void jl_run_event_loop(uv_loop_t *loop)
{
    if (loop) {
        loop->stop_flag = 0;
        uv_run(loop,UV_RUN_DEFAULT);
    }
}

DLLEXPORT int jl_process_events(uv_loop_t *loop)
{
    if (loop) {
        loop->stop_flag = 0;
        return uv_run(loop,UV_RUN_NOWAIT);
    }
    else return 0;
}

DLLEXPORT int jl_init_pipe(uv_pipe_t *pipe, int writable, int readable, int julia_only)
{
     int flags = 0;
     if (writable)
         flags |= UV_PIPE_WRITABLE;
     if (readable)
         flags |= UV_PIPE_READABLE;
     if (!julia_only)
         flags |= UV_PIPE_SPAWN_SAFE;
     int err = uv_pipe_init(jl_io_loop, pipe, flags);
     return err;
}

DLLEXPORT void jl_close_uv(uv_handle_t *handle)
{
    if (handle->type==UV_TTY)
        uv_tty_set_mode((uv_tty_t*)handle,0);

    if ((handle->type == UV_NAMED_PIPE || handle->type == UV_TCP) &&
        uv_is_writable((uv_stream_t*)handle)) {
        uv_shutdown_t *req = (uv_shutdown_t*)malloc(sizeof(uv_shutdown_t));
        req->data = 0;
        /*
         * We are explicity ignoring the error here for the following reason:
         * There is only two scenarios in which this returns an error:
         * a) In case the stream is already shut down, in which case we're likely
         *    in the process of closing this stream (since there's no other call to
         *    uv_shutdown).
         * b) In case the stream is already closed, in which case uv_close would
         *    cause an assertion failure.
         */
        uv_shutdown(req, (uv_stream_t*)handle, &jl_uv_shutdownCallback);
    }
    else if (handle->type == UV_FILE) {
        uv_fs_t req;
        jl_uv_file_t *fd = (jl_uv_file_t*)handle;
        if (fd->file != -1) {
            uv_fs_close(handle->loop, &req, fd->file, NULL);
            fd->file = -1;
        }
    }
    else if (!uv_is_closing((uv_handle_t*)handle)) {
        uv_close(handle,&jl_uv_closeHandle);
    }
}

DLLEXPORT void jl_forceclose_uv(uv_handle_t *handle)
{
    uv_close(handle,&jl_uv_closeHandle);
}

DLLEXPORT void jl_uv_associate_julia_struct(uv_handle_t *handle, jl_value_t *data)
{
    handle->data = data;
}

DLLEXPORT void jl_uv_disassociate_julia_struct(uv_handle_t *handle)
{
    handle->data = NULL;
}

DLLEXPORT int jl_listen(uv_stream_t *stream, int backlog)
{
    return uv_listen(stream,backlog,&jl_uv_connectioncb);
}

DLLEXPORT int jl_spawn(char *name, char **argv, uv_loop_t *loop,
                       uv_process_t *proc, jl_value_t *julia_struct,
                       uv_handle_type stdin_type, uv_pipe_t *stdin_pipe,
                       uv_handle_type stdout_type, uv_pipe_t *stdout_pipe,
                       uv_handle_type stderr_type, uv_pipe_t *stderr_pipe,
                       int detach, char **env, char *cwd)
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
    opts.cwd = cwd;
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
    error = uv_spawn(loop,proc,&opts);
    return error;
}

#ifdef _OS_WINDOWS_
#include <time.h>
DLLEXPORT struct tm* localtime_r(const time_t *t, struct tm *tm)
{
    struct tm *tmp = localtime(t); //localtime is reentrant on windows
    if (tmp)
        *tm = *tmp;
    return tmp;
}
#endif

/*DLLEXPORT uv_loop_t *jl_new_event_loop()
{
    return uv_loop_new();
}*/

DLLEXPORT uv_loop_t *jl_global_event_loop(void)
{
    return jl_io_loop;
}

DLLEXPORT int jl_poll_start(uv_poll_t *handle, int32_t events)
{
    return uv_poll_start(handle, events, &jl_uv_pollcb);
}

DLLEXPORT int jl_fs_poll_start(uv_fs_poll_t *handle, char *file, uint32_t interval)
{
    return uv_fs_poll_start(handle,&jl_uv_fspollcb,file,interval);
}

DLLEXPORT int jl_fs_event_init(uv_loop_t *loop, uv_fs_event_t *handle,
                               const char *filename, int flags)
{
    uv_fs_event_init(loop,handle);
    return uv_fs_event_start(handle,&jl_uv_fseventscb,filename,flags);
}

DLLEXPORT int jl_fs_unlink(char *path)
{
    uv_fs_t req;
    JL_SIGATOMIC_BEGIN();
    int ret = uv_fs_unlink(jl_io_loop, &req, path, NULL);
    uv_fs_req_cleanup(&req);
    JL_SIGATOMIC_END();
    return ret;
}

DLLEXPORT int jl_fs_rename(char *src_path, char *dst_path)
{
    uv_fs_t req;
    JL_SIGATOMIC_BEGIN();
    int ret = uv_fs_rename(jl_io_loop, &req, src_path, dst_path, NULL);
    uv_fs_req_cleanup(&req);
    JL_SIGATOMIC_END();
    return ret;
}

DLLEXPORT int jl_fs_sendfile(int src_fd, int dst_fd,
                             int64_t in_offset, size_t len)
{
    uv_fs_t req;
    JL_SIGATOMIC_BEGIN();
    int ret = uv_fs_sendfile(jl_io_loop, &req, dst_fd, src_fd,
                             in_offset, len, NULL);
    uv_fs_req_cleanup(&req);
    JL_SIGATOMIC_END();
    return ret;
}

DLLEXPORT int jl_fs_symlink(char *path, char *new_path, int flags)
{
    uv_fs_t req;
    int ret = uv_fs_symlink(jl_io_loop, &req, path, new_path, flags, NULL);
    uv_fs_req_cleanup(&req);
    return ret;
}

DLLEXPORT int jl_fs_chmod(char *path, int mode)
{
    uv_fs_t req;
    int ret = uv_fs_chmod(jl_io_loop, &req, path, mode, NULL);
    uv_fs_req_cleanup(&req);
    return ret;
}

DLLEXPORT int jl_fs_write(int handle, char *data, size_t len, int64_t offset)
{
    uv_fs_t req;
    uv_buf_t buf[1];
    buf[0].base = data;
    buf[0].len = len;
    int ret = uv_fs_write(jl_io_loop, &req, handle, buf, 1, offset, NULL);
    uv_fs_req_cleanup(&req);
    return ret;
}

DLLEXPORT int jl_fs_write_byte(int handle, char c)
{
    uv_fs_t req;
    uv_buf_t buf[1];
    buf[0].base = &c;
    buf[0].len = 1;
    int ret = uv_fs_write(jl_io_loop, &req, handle, buf, 1, -1, NULL);
    uv_fs_req_cleanup(&req);
    return ret;
}

DLLEXPORT int jl_fs_read(int handle, char *data, size_t len)
{
    uv_fs_t req;
    uv_buf_t buf[1];
    buf[0].base = data;
    buf[0].len = len;
    int ret = uv_fs_read(jl_io_loop, &req, handle, buf, 1, -1, NULL);
    uv_fs_req_cleanup(&req);
    return ret;
}

DLLEXPORT int jl_fs_read_byte(int handle)
{
    uv_fs_t req;
    char c;
    uv_buf_t buf[1];
    buf[0].base = &c;
    buf[0].len = 1;
    int ret = uv_fs_read(jl_io_loop, &req, handle, buf, 1, -1, NULL);
    uv_fs_req_cleanup(&req);
    if (ret == -1)
        return ret;
    return (int)c;
}

DLLEXPORT int jl_fs_close(int handle)
{
    uv_fs_t req;
    int ret = uv_fs_close(jl_io_loop, &req, handle, NULL);
    uv_fs_req_cleanup(&req);
    return ret;
}

//units are in ms
DLLEXPORT int jl_puts(const char *str, uv_stream_t *stream)
{
    if (!stream) return 0;
    return jl_write(stream,str,strlen(str));
}

DLLEXPORT void jl_uv_writecb(uv_write_t *req, int status)
{
    if (req->data) {
        JULIA_CB(writecb, req->data, 2, CB_PTR, req, CB_INT32, status);
    }
    free(req);
}

DLLEXPORT void jl_uv_writecb_task(uv_write_t *req, int status)
{
    JULIA_CB(writecb_task, req->handle->data, 2, CB_PTR, req, CB_INT32, status);
    free(req);
}

DLLEXPORT int jl_write_copy(uv_stream_t *stream, const char *str, size_t n, uv_write_t *uvw, void *writecb)
{
    JL_SIGATOMIC_BEGIN();
    char *data = (char*)(uvw+1);
    memcpy(data,str,n);
    uv_buf_t buf[1];
    buf[0].base = data;
    buf[0].len = n;
    uvw->data = NULL;
    int err = uv_write(uvw,stream,buf,1,(uv_write_cb)writecb);
    JL_SIGATOMIC_END();
    return err;
}

DLLEXPORT int jl_putc(char c, uv_stream_t *stream)
{
    int err;
    if (stream!=0) {
        if (stream->type<UV_HANDLE_TYPE_MAX) { //is uv handle
            if (stream->type == UV_FILE) {
                JL_SIGATOMIC_BEGIN();
                jl_uv_file_t *file = (jl_uv_file_t *)stream;
                // Do a blocking write for now
                uv_fs_t req;
                uv_buf_t buf[1];
                buf[0].base = &c;
                buf[0].len = 1;
                err = uv_fs_write(file->loop, &req, file->file, buf, 1, -1, NULL);
                JL_SIGATOMIC_END();
                return err ? 0 : 1;
            }
            else {
                uv_write_t *uvw = (uv_write_t*)malloc(sizeof(uv_write_t)+1);
                err = jl_write_copy(stream,(char*)&c,1,uvw, (void*)&jl_uv_writecb);
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
    uv_buf_t buf[1];
    buf[0].base = data;
    buf[0].len = n;
    JL_SIGATOMIC_BEGIN();
    int err = uv_write(uvw,stream,buf,1,(uv_write_cb)writecb);
    uvw->data = NULL;
    JL_SIGATOMIC_END();
    return err;
}

DLLEXPORT int jl_putc_copy(unsigned char c, uv_stream_t *stream, void *uvw, void *writecb)
{
    return jl_write_copy(stream,(char *)&c,1,(uv_write_t*)uvw,writecb);
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
    return jl_write_copy(s, buf, n, (uv_write_t*)uvw, writecb);
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
            uv_buf_t buf[1];
            buf[0].base = (char*)str;
            buf[0].len = n;
            err = uv_fs_write(file->loop, &req, file->file, buf, 1, -1, NULL);
            JL_SIGATOMIC_END();
            return err ? 0 : n;
        }
        else {
            uv_write_t *uvw = (uv_write_t*)malloc(sizeof(uv_write_t)+n);
            err = jl_write_copy(stream,str,n,uvw, (void*)&jl_uv_writecb);
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

DLLEXPORT void jl_exit(int exitcode)
{
    uv_tty_reset_mode();
    jl_atexit_hook();
    exit(exitcode);
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
DLLEXPORT int jl_tcp_bind(uv_tcp_t *handle, uint16_t port, uint32_t host, unsigned int flags)
{
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(struct sockaddr_in));
    addr.sin_port = port;
    addr.sin_addr.s_addr = host;
    addr.sin_family = AF_INET;
    return uv_tcp_bind(handle, (struct sockaddr*)&addr, flags);
}
DLLEXPORT int jl_tcp_bind6(uv_tcp_t *handle, uint16_t port, void *host, unsigned int flags)
{
    struct sockaddr_in6 addr;
    memset(&addr, 0, sizeof(struct sockaddr_in6));
    addr.sin6_port = port;
    memcpy(&addr.sin6_addr, host, 16);
    addr.sin6_family = AF_INET6;
    return uv_tcp_bind(handle, (struct sockaddr*)&addr, flags);
}

DLLEXPORT int jl_udp_bind(uv_udp_t *handle, uint16_t port, uint32_t host, uint32_t flags)
{
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(struct sockaddr_in));
    addr.sin_port = port;
    addr.sin_addr.s_addr = host;
    addr.sin_family = AF_INET;
    return uv_udp_bind(handle, (struct sockaddr*)&addr, flags);
}
DLLEXPORT int jl_udp_bind6(uv_udp_t *handle, uint16_t port, void *host, uint32_t flags)
{
    struct sockaddr_in6 addr;
    memset(&addr, 0, sizeof(struct sockaddr_in6));
    addr.sin6_port = port;
    memcpy(&addr.sin6_addr, host, 16);
    addr.sin6_family = AF_INET6;
    return uv_udp_bind(handle, (struct sockaddr*)&addr, flags);
}

DLLEXPORT int jl_udp_send(uv_udp_t *handle, uint16_t port, uint32_t host, void *data, uint32_t size)
{
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(struct sockaddr_in));
    addr.sin_port = port;
    addr.sin_addr.s_addr = host;
    addr.sin_family = AF_INET;
    uv_buf_t buf[1];
    buf[0].base = (char *) data;
    buf[0].len = size;
    uv_udp_send_t *req = (uv_udp_send_t*)malloc(sizeof(uv_udp_send_t));
    req->data = handle->data;
    return uv_udp_send(req, handle, buf, 1, (struct sockaddr*)&addr, &jl_uv_sendcb);
}

DLLEXPORT int jl_udp_send6(uv_udp_t *handle, uint16_t port, void *host, void *data, uint32_t size)
{
    struct sockaddr_in6 addr;
    memset(&addr, 0, sizeof(struct sockaddr_in6));
    addr.sin6_port = port;
    memcpy(&addr.sin6_addr, host, 16);
    addr.sin6_family = AF_INET6;
    uv_buf_t buf[1];
    buf[0].base = (char *) data;
    buf[0].len = size;
    uv_udp_send_t *req = (uv_udp_send_t *) malloc(sizeof(uv_udp_send_t));
    req->data = handle->data;
    return uv_udp_send(req, handle, buf, 1, (struct sockaddr*)&addr, &jl_uv_sendcb);
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
    uv_getaddrinfo_t *req = (uv_getaddrinfo_t*)malloc(sizeof(uv_getaddrinfo_t));
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
    uv_connect_t *req = (uv_connect_t*)malloc(sizeof(uv_connect_t));
    req->data = 0;
    memset(&addr, 0, sizeof(struct sockaddr_in));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = host;
    addr.sin_port = port;
    return uv_tcp_connect(req,handle,(struct sockaddr*)&addr,&jl_uv_connectcb);
}

DLLEXPORT int jl_tcp6_connect(uv_tcp_t *handle, void *host, uint16_t port)
{
    struct sockaddr_in6 addr;
    uv_connect_t *req = (uv_connect_t*)malloc(sizeof(uv_connect_t));
    req->data = 0;
    memset(&addr, 0, sizeof(struct sockaddr_in6));
    addr.sin6_family = AF_INET6;
    memcpy(&addr.sin6_addr, host, 16);
    addr.sin6_port = port;
    return uv_tcp_connect(req,handle,(struct sockaddr*)&addr,&jl_uv_connectcb);
}

DLLEXPORT int jl_connect_raw(uv_tcp_t *handle,struct sockaddr_storage *addr)
{
    uv_connect_t *req = (uv_connect_t*)malloc(sizeof(uv_connect_t));
    req->data = 0;
    return uv_tcp_connect(req,handle,(struct sockaddr*)addr,&jl_uv_connectcb);
}

#ifdef _OS_LINUX_
DLLEXPORT int jl_tcp_quickack(uv_tcp_t *handle, int on)
{
    int fd = (handle)->io_watcher.fd;
    if (fd != -1) {
        if (setsockopt(fd, IPPROTO_TCP, TCP_QUICKACK, &on, sizeof(on))) {
            return -1;
        }
    }
    return 0;
}
#endif

DLLEXPORT char *jl_ios_buf_base(ios_t *ios)
{
    return ios->buf;
}

DLLEXPORT jl_uv_libhandle jl_wrap_raw_dl_handle(void *handle)
{
    uv_lib_t *lib = (uv_lib_t*)malloc(sizeof(uv_lib_t));
    #ifdef _OS_WINDOWS_
    lib->handle=(HMODULE)handle;
    #else
    lib->handle=handle;
    #endif
    lib->errmsg=NULL;
    return (jl_uv_libhandle) lib;
}

#ifndef _OS_WINDOWS_

DLLEXPORT int jl_uv_unix_fd_is_watched(int fd, uv_poll_t *handle, uv_loop_t *loop)
{
    if (fd >= loop->nwatchers)
        return 0;
    if (loop->watchers[fd] == NULL)
        return 0;
    if (loop->watchers[fd] == &handle->io_watcher)
        return 0;
    return 1;
}

#endif

#ifdef _OS_WINDOWS_
static inline int ishexchar(char c)
{
   if (c >= '0' && c <= '9') return 1;
   if (c >= 'a' && c <= 'z') return 1;
   return 0;
}

DLLEXPORT int jl_ispty(uv_pipe_t *pipe)
{
    if (pipe->type != UV_NAMED_PIPE) return 0;
    size_t len = 0;
    if (uv_pipe_getsockname(pipe, NULL, &len) != UV_ENOBUFS) return 0;
    char *name = (char *) alloca(len);
    if (uv_pipe_getsockname(pipe, name, &len)) return 0;
    // return true if name matches regex:
    // ^\\\\?\\pipe\\(msys|cygwin)-[0-9a-z]{16}-[pt]ty[1-9][0-9]*-
    //JL_PRINTF(JL_STDERR,"pipe_name: %s\n", name);
    int n = 0;
    if (!strncmp(name,"\\\\?\\pipe\\msys-",14))
        n = 14;
    else if (!strncmp(name,"\\\\?\\pipe\\cygwin-",16))
        n = 16;
    else
        return 0;
    //JL_PRINTF(JL_STDERR,"prefix pass\n");
    name += n;
    for (int n = 0; n < 16; n++)
        if (!ishexchar(*name++)) return 0;
    //JL_PRINTF(JL_STDERR,"hex pass\n");
    if ((*name++)!='-') return 0;
    if (*name != 'p' && *name != 't') return 0;
    name++;
    if (*name++ != 't' || *name++ != 'y') return 0;
    //JL_PRINTF(JL_STDERR,"tty pass\n");
    return 1;
}
#endif

DLLEXPORT uv_handle_type jl_uv_handle_type(uv_handle_t *handle)
{
#ifdef _OS_WINDOWS_
    if (jl_ispty((uv_pipe_t*)handle))
        return UV_TTY;
#endif
    return handle->type;
}

DLLEXPORT int jl_tty_set_mode(uv_tty_t *handle, int mode)
{
    if (handle->type != UV_TTY) return 0;
    return uv_tty_set_mode(handle, mode);
}

DLLEXPORT uv_file jl_uv_file_handle(jl_uv_file_t *f)
{
    return f->file;
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

#ifndef _OS_WINDOWS_
#if defined(__APPLE__)
int uv___stream_fd(uv_stream_t *handle);
#define uv__stream_fd(handle) (uv___stream_fd((uv_stream_t*)(handle)))
#else
#define uv__stream_fd(handle) ((handle)->io_watcher.fd)
#endif /* defined(__APPLE__) */
DLLEXPORT int jl_uv_handle(uv_stream_t *handle)
{
    return uv__stream_fd(handle);
}
#else
DLLEXPORT HANDLE jl_uv_handle(uv_stream_t *handle)
{
    switch (handle->type) {
    case UV_TTY:
        return ((uv_tty_t*)handle)->handle;
    case UV_NAMED_PIPE:
        return ((uv_pipe_t*)handle)->handle;
    default:
        return INVALID_HANDLE_VALUE;
    }
}
#endif
#ifdef __cplusplus
}
#endif
