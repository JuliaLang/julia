// This file is a part of Julia. License is MIT: http://julialang.org/license

#include "platform.h"

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

#if defined(_COMPILER_MICROSOFT_) && !defined(write)
#include <io.h>
#define write _write
#endif

#ifndef static_assert
#  ifndef __cplusplus
#    define static_assert(...)
// Remove the following gcc special handling when we officially requires
// gcc 4.7 (for c++11) and -std=gnu11
#    ifdef __GNUC__
#      if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 6)
#        undef static_assert
#        define static_assert _Static_assert
#      endif
#    endif
#  endif
#endif

#ifdef __cplusplus
#include <cstring>
extern "C" {
#endif

extern jl_module_t *jl_old_base_module;
static jl_value_t *close_cb = NULL;

static void jl_uv_call_close_callback(void *val)
{
    jl_value_t *cb;
    if (!jl_old_base_module) {
        if (close_cb == NULL)
            close_cb = jl_get_global(jl_base_module, jl_symbol("_uv_hook_close"));
        cb = close_cb;
    }
    else {
        cb = jl_get_global(jl_base_relative_to(((jl_datatype_t*)jl_typeof(val))->name->module), jl_symbol("_uv_hook_close"));
    }
    assert(cb && jl_is_function(cb));
    jl_apply((jl_function_t*)cb, (jl_value_t**)&val, 1);
}

DLLEXPORT void jl_uv_closeHandle(uv_handle_t *handle)
{
    // if the user killed a stdio handle,
    // revert back to direct stdio FILE* writes
    // so that errors can still be reported
    if (handle == (uv_handle_t*)JL_STDIN)
        JL_STDIN = (JL_STREAM*)STDIN_FILENO;
    if (handle == (uv_handle_t*)JL_STDOUT)
        JL_STDOUT = (JL_STREAM*)STDOUT_FILENO;
    if (handle == (uv_handle_t*)JL_STDERR)
        JL_STDERR = (JL_STREAM*)STDERR_FILENO;
    // also let the client app do its own cleanup
    if (handle->type != UV_FILE && handle->data)
        jl_uv_call_close_callback(handle->data);
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

// getters and setters
DLLEXPORT void *jl_uv_process_data(uv_process_t *p) { return p->data; }
DLLEXPORT void *jl_uv_buf_base(const uv_buf_t *buf) { return buf->base; }
DLLEXPORT size_t jl_uv_buf_len(const uv_buf_t *buf) { return buf->len; }
DLLEXPORT void jl_uv_buf_set_base(uv_buf_t *buf, char *b) { buf->base = b; }
DLLEXPORT void jl_uv_buf_set_len(uv_buf_t *buf, size_t n) { buf->len = n; }
DLLEXPORT void *jl_uv_connect_handle(uv_connect_t *connect) { return connect->handle; }
DLLEXPORT void *jl_uv_getaddrinfo_data(uv_getaddrinfo_t *req) { return req->data; }
DLLEXPORT uv_file jl_uv_file_handle(jl_uv_file_t *f) { return f->file; }
DLLEXPORT void *jl_uv_req_data(uv_req_t *req) { return req->data; }
DLLEXPORT void jl_uv_req_set_data(uv_req_t *req, void *data) { req->data = data; }
DLLEXPORT void *jl_uv_handle_data(uv_handle_t *handle) { return handle->data; }
DLLEXPORT void *jl_uv_write_handle(uv_write_t *req) { return req->handle; }

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
    if (handle->type == UV_FILE) {
        uv_fs_t req;
        jl_uv_file_t *fd = (jl_uv_file_t*)handle;
        if (fd->file != -1) {
            uv_fs_close(handle->loop, &req, fd->file, NULL);
            fd->file = -1;
        }
        jl_uv_closeHandle(handle); // synchronous (ok since the callback is known to not interact with any global state)
        return;
    }

    if (handle->type == UV_NAMED_PIPE || handle->type == UV_TCP) {
        if (((uv_stream_t*)handle)->shutdown_req) {
            // don't close the stream while attempting a graceful shutdown
            return;
        }
        if (uv_is_writable((uv_stream_t*)handle)) {
            // attempt graceful shutdown of writable streams to give them a chance to flush first
            uv_shutdown_t *req = (uv_shutdown_t*)malloc(sizeof(uv_shutdown_t));
            req->data = 0;
            /*
             * We are explicitly ignoring the error here for the following reason:
             * There is only two scenarios in which this returns an error:
             * a) In case the stream is already shut down, in which case we're likely
             *    in the process of closing this stream (since there's no other call to
             *    uv_shutdown).
             * b) In case the stream is already closed, in which case uv_close would
             *    cause an assertion failure.
             */
            uv_shutdown(req, (uv_stream_t*)handle, &jl_uv_shutdownCallback);
            return;
        }
    }

    if (!uv_is_closing((uv_handle_t*)handle)) {
        // avoid double-closing the stream
        if (handle->type == UV_TTY)
            uv_tty_set_mode((uv_tty_t*)handle,0);
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

DLLEXPORT int jl_spawn(char *name, char **argv, uv_loop_t *loop,
                       uv_process_t *proc, jl_value_t *julia_struct,
                       uv_handle_type stdin_type, uv_pipe_t *stdin_pipe,
                       uv_handle_type stdout_type, uv_pipe_t *stdout_pipe,
                       uv_handle_type stderr_type, uv_pipe_t *stderr_pipe,
                       int flags, char **env, char *cwd, uv_exit_cb cb)
{
    uv_process_options_t opts;
    uv_stdio_container_t stdio[3];
    int error;
    opts.file = name;
    opts.env = env;
#ifdef _OS_WINDOWS_
    opts.flags = flags;
#else
    opts.flags = flags | UV_PROCESS_RESET_SIGPIPE;
#endif
    opts.cwd = cwd;
    opts.args = argv;
    opts.stdio = stdio;
    opts.stdio_count = 3;
    stdio[0].type = stdin_type;
    stdio[0].data.stream = (uv_stream_t*)(stdin_pipe);
    stdio[1].type = stdout_type;
    stdio[1].data.stream = (uv_stream_t*)(stdout_pipe);
    stdio[2].type = stderr_type;
    stdio[2].data.stream = (uv_stream_t*)(stderr_pipe);
    opts.exit_cb = cb;
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

DLLEXPORT uv_loop_t *jl_global_event_loop(void)
{
    return jl_io_loop;
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

DLLEXPORT int jl_fs_rename(const char *src_path, const char *dst_path)
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

DLLEXPORT int jl_fs_write(int handle, const char *data, size_t len, int64_t offset)
{
    uv_fs_t req;
    uv_buf_t buf[1];
    buf[0].base = (char*)data;
    buf[0].len = len;
    int ret = uv_fs_write(jl_io_loop, &req, handle, buf, 1, offset, NULL);
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

DLLEXPORT int jl_uv_write(uv_stream_t *stream, const char *data, size_t n, uv_write_t *uvw, void *writecb)
{
    uv_buf_t buf[1];
    buf[0].base = (char*)data;
    buf[0].len = n;
    JL_SIGATOMIC_BEGIN();
    int err = uv_write(uvw,stream,buf,1,(uv_write_cb)writecb);
    JL_SIGATOMIC_END();
    return err;
}

DLLEXPORT void jl_uv_writecb(uv_write_t *req, int status)
{
    free(req);
    if (status < 0) {
        jl_safe_printf("jl_uv_writecb() ERROR: %s %s\n",
                       uv_strerror(status), uv_err_name(status));
    }
}

// Note: jl_write() is called only by jl_vprintf().
// See: doc/devdocs/stdio.rst

static void jl_write(uv_stream_t *stream, const char *str, size_t n)
{
    assert(stream);
    static_assert(offsetof(uv_stream_t,type) == offsetof(ios_t,bm) &&
        sizeof(((uv_stream_t*)0)->type) == sizeof(((ios_t*)0)->bm),
            "UV and ios layout mismatch");

    uv_file fd = 0;

    // Fallback for output during early initialisation...
    if (stream == (void*)STDOUT_FILENO || stream == (void*)STDERR_FILENO) {
        if (!jl_io_loop) jl_io_loop = uv_default_loop();
        fd = (uv_file)(size_t)stream;
    }
    else if (stream->type == UV_FILE) {
        fd = ((jl_uv_file_t*)stream)->file;
    }

    if (fd) {
        // Write to file descriptor...
        jl_fs_write(fd, str, n, -1);
    }
    else if (stream->type > UV_HANDLE_TYPE_MAX) {
        // Write to ios.c stream...
        // This is needed because caller jl_static_show() in builtins.c can be
        // called from fl_print in flisp/print.c (via cvalue_printdata()),
        // and cvalue_printdata() passes ios_t* to jl_static_show().
        ios_write((ios_t*)stream, str, n);
    }
    else {
        // Write to libuv stream...
        uv_write_t *req = (uv_write_t*)malloc(sizeof(uv_write_t)+n);
        char *data = (char*)(req+1);
        memcpy(data,str,n);
        uv_buf_t buf[1];
        buf[0].base = data;
        buf[0].len = n;
        req->data = NULL;
        JL_SIGATOMIC_BEGIN();
        int status = uv_write(req, stream, buf, 1, (uv_write_cb)jl_uv_writecb);
        JL_SIGATOMIC_END();
        if (status < 0) {
            jl_uv_writecb(req, status);
        }
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
        free(str);
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

DLLEXPORT void jl_safe_printf(const char *fmt, ...)
{
    static char buf[1000];
    buf[0] = '\0';

    va_list args;
    va_start(args, fmt);
    // Not async signal safe on some platforms?
    vsnprintf(buf, sizeof(buf), fmt, args);
    va_end(args);

    buf[999] = '\0';
    if (write(STDERR_FILENO, buf, strlen(buf)) < 0) {
        // nothing we can do; ignore the failure
    }
}

DLLEXPORT void jl_exit(int exitcode)
{
    uv_tty_reset_mode();
    jl_atexit_hook(exitcode);
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

DLLEXPORT int jl_tcp_getsockname(uv_tcp_t *handle, uint16_t* port, void* host, uint32_t* family)
{
    int namelen;
    struct sockaddr_storage addr;
    memset(&addr, 0, sizeof(struct sockaddr_storage));
    namelen = sizeof addr;
    int res = uv_tcp_getsockname(handle, (struct sockaddr*)&addr, &namelen);
    *family = addr.ss_family;
    if (addr.ss_family == AF_INET) {
        struct sockaddr_in* addr4 = (struct sockaddr_in*)&addr;
        *port = addr4->sin_port;
        memcpy(host, &(addr4->sin_addr), 4);
    } else if (addr.ss_family == AF_INET6) {
        struct sockaddr_in6* addr6 = (struct sockaddr_in6*)&addr;
        *port = addr6->sin6_port;
        memcpy(host, &(addr6->sin6_addr), 16);
    } else {
        return -1;
    }
    return res;
}

DLLEXPORT int jl_tcp_getpeername(uv_tcp_t *handle, uint16_t* port, void* host, uint32_t* family)
{
    int namelen;
    struct sockaddr_storage addr;
    memset(&addr, 0, sizeof(struct sockaddr_storage));
    namelen = sizeof addr;
    int res = uv_tcp_getpeername(handle, (struct sockaddr*)&addr, &namelen);
    *family = addr.ss_family;
    if (addr.ss_family == AF_INET) {
        struct sockaddr_in* addr4 = (struct sockaddr_in*)&addr;
        *port = addr4->sin_port;
        memcpy(host, &(addr4->sin_addr), 4);
    } else if (addr.ss_family == AF_INET6) {
        struct sockaddr_in6* addr6 = (struct sockaddr_in6*)&addr;
        *port = addr6->sin6_port;
        memcpy(host, &(addr6->sin6_addr), 16);
    } else {
        return -1;
    }
    return res;
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

DLLEXPORT int jl_udp_send(uv_udp_t *handle, uint16_t port, uint32_t host, void *data, uint32_t size, uv_udp_send_cb cb)
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
    return uv_udp_send(req, handle, buf, 1, (struct sockaddr*)&addr, cb);
}

DLLEXPORT int jl_udp_send6(uv_udp_t *handle, uint16_t port, void *host, void *data, uint32_t size, uv_udp_send_cb cb)
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
    return uv_udp_send(req, handle, buf, 1, (struct sockaddr*)&addr, cb);
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

DLLEXPORT int jl_getaddrinfo(uv_loop_t *loop, const char *host, const char *service, jl_function_t *cb, uv_getaddrinfo_cb uvcb)
{
    uv_getaddrinfo_t *req = (uv_getaddrinfo_t*)malloc(sizeof(uv_getaddrinfo_t));
    struct addrinfo hints;

    memset (&hints, 0, sizeof (hints));
    hints.ai_family = PF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags |= AI_CANONNAME;

    req->data = cb;

    return uv_getaddrinfo(loop,req,uvcb,host,service,&hints);
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
    if (addr->ss_family==AF_INET)
        ((struct sockaddr_in*)addr)->sin_port = port;
    else
        ((struct sockaddr_in6*)addr)->sin6_port = port;
}

DLLEXPORT int jl_tcp4_connect(uv_tcp_t *handle,uint32_t host, uint16_t port, uv_connect_cb cb)
{
    struct sockaddr_in addr;
    uv_connect_t *req = (uv_connect_t*)malloc(sizeof(uv_connect_t));
    req->data = 0;
    memset(&addr, 0, sizeof(struct sockaddr_in));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = host;
    addr.sin_port = port;
    return uv_tcp_connect(req,handle,(struct sockaddr*)&addr,cb);
}

DLLEXPORT int jl_tcp6_connect(uv_tcp_t *handle, void *host, uint16_t port, uv_connect_cb cb)
{
    struct sockaddr_in6 addr;
    uv_connect_t *req = (uv_connect_t*)malloc(sizeof(uv_connect_t));
    req->data = 0;
    memset(&addr, 0, sizeof(struct sockaddr_in6));
    addr.sin6_family = AF_INET6;
    memcpy(&addr.sin6_addr, host, 16);
    addr.sin6_port = port;
    return uv_tcp_connect(req,handle,(struct sockaddr*)&addr,cb);
}

DLLEXPORT int jl_connect_raw(uv_tcp_t *handle,struct sockaddr_storage *addr, uv_connect_cb cb)
{
    uv_connect_t *req = (uv_connect_t*)malloc(sizeof(uv_connect_t));
    req->data = 0;
    return uv_tcp_connect(req,handle,(struct sockaddr*)addr,cb);
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

DLLEXPORT int jl_tcp_reuseport(uv_tcp_t *handle)
{
#if defined(SO_REUSEPORT)
    int fd = (handle)->io_watcher.fd;
    int yes = 1;
    if (setsockopt(fd, SOL_SOCKET, SO_REUSEPORT, &yes, sizeof(yes))) {
        return -1;
    }
    return 0;
#else
    return 1;
#endif
}

#ifndef _OS_WINDOWS_

DLLEXPORT int jl_uv_unix_fd_is_watched(int fd, uv_poll_t *handle, uv_loop_t *loop)
{
    if (fd >= loop->nwatchers)
        return 0;
    if (loop->watchers[fd] == NULL)
        return 0;
    if (handle && loop->watchers[fd] == &handle->io_watcher)
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
    //jl_printf(JL_STDERR,"pipe_name: %s\n", name);
    int n = 0;
    if (!strncmp(name,"\\\\?\\pipe\\msys-",14))
        n = 14;
    else if (!strncmp(name,"\\\\?\\pipe\\cygwin-",16))
        n = 16;
    else
        return 0;
    //jl_printf(JL_STDERR,"prefix pass\n");
    name += n;
    for (int n = 0; n < 16; n++)
        if (!ishexchar(*name++)) return 0;
    //jl_printf(JL_STDERR,"hex pass\n");
    if ((*name++)!='-') return 0;
    if (*name != 'p' && *name != 't') return 0;
    name++;
    if (*name++ != 't' || *name++ != 'y') return 0;
    //jl_printf(JL_STDERR,"tty pass\n");
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
