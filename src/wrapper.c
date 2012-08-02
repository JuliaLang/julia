#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <malloc.h>

#ifdef __WIN32__
#include <malloc.h>
#else
#include "errno.h"
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

#define DEFINE_JULIA_HOOK(hook) static jl_function_t *jl_uvhook_##hook = 0;
#define JULIA_HOOK(hook) \
    (jl_uvhook_##hook ? jl_uvhook_##hook : (jl_uvhook_##hook = ((jl_function_t*) jl_get_global(jl_base_module,jl_symbol("_uv_hook_" #hook)))))

jl_value_t *jl_callback_call(jl_function_t *f,jl_value_t *val,int count,...)
{
    jl_value_t **argv = alloca((count+1)*sizeof(jl_value_t*));
    memset(argv+1, 0, count);
    va_list argp;
    va_start(argp,count);
    jl_value_t *v=0;
    int i;
    argv[0]=val;
    JL_GC_PUSHARGS(argv,count+1);
    for(i=1; i<(count+1); ++i) {
        switch(va_arg(argp,int)) {
        case CB_PTR:
            v = jl_box_pointer(va_arg(argp,void*));
            break;
        case CB_INT32:
            v = jl_box_int32(va_arg(argp,int32_t));
            break;
        case CB_INT64:
            v = jl_box_int64(va_arg(argp,int64_t));
            break;
        default: jl_error("callback: only Ints and Pointers are supported at this time");
            //excecution never reaches here
            break;
        }
        argv[i]=v;
    }
    v = jl_apply(f,(jl_value_t**)argv,count+1);
    JL_GC_POP();
    return v;
}

//These callbacks are implemented in stream.jl
DEFINE_JULIA_HOOK(close)
DEFINE_JULIA_HOOK(return_spawn)
DEFINE_JULIA_HOOK(readcb)
DEFINE_JULIA_HOOK(alloc_buf)
DEFINE_JULIA_HOOK(connectcb)
DEFINE_JULIA_HOOK(connectioncb)
DEFINE_JULIA_HOOK(asynccb)

void closeHandle(uv_handle_t* handle)
{
#ifndef __WIN32__
    ev_invoke_pending(handle->loop->ev);
#endif
    jl_callback_call(JULIA_HOOK(close),handle->data,0);
    //TODO: maybe notify Julia handle to close itself
    free(handle);
}


void jl_return_spawn(uv_process_t *p, int exit_status, int term_signal) {
    jl_callback_call(JULIA_HOOK(return_spawn),p->data,2,CB_INT32,exit_status,CB_INT32,term_signal);
    uv_close((uv_handle_t*)p,&closeHandle);
}

void jl_readcb(uv_stream_t *handle, ssize_t nread, uv_buf_t buf)
{
    jl_callback_call(JULIA_HOOK(readcb),handle->data,3,CB_INT,nread,CB_PTR,(buf.base),CB_INT32,buf.len);
}

uv_buf_t jl_alloc_buf(uv_handle_t *handle, size_t suggested_size) {
    uv_buf_t buf;
    jl_value_t *val = jl_callback_call(JULIA_HOOK(alloc_buf),handle->data,1,CB_INT32,suggested_size);
    if(!jl_is_tuple(val) || !jl_is_pointer(jl_t0(val)) || !jl_is_int32(jl_t1(val)))
        jl_error("jl_alloc_buf: Julia function returned invalid value for buffer allocation callback");
    buf.base = jl_unbox_pointer(jl_t0(val));
    buf.len = jl_unbox_int32(jl_t1(val));
    return buf;
}

void jl_connectcb(uv_connect_t *connect, int status)
{
    jl_callback_call(JULIA_HOOK(connectcb),connect->handle->data,1,CB_INT32,status);
}

void jl_connectioncb(uv_stream_t *stream, int status)
{
    jl_callback_call(JULIA_HOOK(connectioncb),stream->data,1,CB_INT32,status);
}

void jl_asynccb(uv_handle_t *handle, int status)
{
    jl_callback_call(JULIA_HOOK(asynccb),handle->data,1,CB_INT32,status);
}

/** libuv constructors */
DLLEXPORT uv_async_t *jl_make_async(uv_loop_t *loop,jl_value_t *julia_struct)
{
    if(!loop)
        return 0;
    uv_async_t *async = malloc(sizeof(uv_async_t));
    if(uv_async_init(loop,async,(uv_async_cb)&jl_asynccb)) {
        free(async);
        return 0;
    }
    async->data=julia_struct;
    return async;
}

DLLEXPORT uv_timer_t *jl_make_timer(uv_loop_t *loop, jl_value_t *julia_struct)
{
    if(!loop)
        return 0;
    uv_timer_t *timer = malloc(sizeof(uv_timer_t));
    if(uv_timer_init(loop,timer)) {
        free(timer);
        return 0;
    }
    timer->data=julia_struct;
    return timer;
}

DLLEXPORT uv_idle_t *jl_idle_init(uv_loop_t *loop, jl_value_t *julia_struct)
{
    if(!loop)
        return 0;
    uv_idle_t *idle = malloc(sizeof(uv_idle_t));
    if(uv_idle_init(loop,idle)) {
        free(idle);
        return 0;
    }
    idle->data = julia_struct;
    return idle;
}

DLLEXPORT uv_tcp_t *jl_make_tcp(uv_loop_t* loop, jl_value_t *julia_struct)
{
    if(!loop)
        return 0;
    uv_tcp_t *tcp = malloc(sizeof(uv_tcp_t));
    if(uv_tcp_init(loop,tcp)) {
        free(tcp);
        return 0;
    }
    tcp->data=julia_struct;
    return tcp;
}

/** This file contains wrappers for most of libuv's stream functionailty. Once we can allocate structs in Julia, this file will be removed */

DLLEXPORT void jl_run_event_loop(uv_loop_t *loop)
{
    restore_signals();
    if(loop) uv_run(loop);
}

DLLEXPORT void jl_process_events(uv_loop_t *loop)
{
    restore_signals();
    if(loop) uv_run_once(loop);
}

DLLEXPORT uv_pipe_t *jl_init_pipe(uv_pipe_t *pipe, int writable, int julia_only, jl_value_t *julia_struct)
{
     int flags;
     flags = writable ? UV_PIPE_WRITEABLE : UV_PIPE_READABLE;
     if (!julia_only)
         flags |= UV_PIPE_SPAWN_SAFE;
     uv_pipe_init(jl_event_loop, pipe, flags);
     pipe->data = julia_struct;//will be initilized on io
     return pipe;
}

DLLEXPORT void jl_close_uv(uv_handle_t *handle)
{
    if(handle) uv_close(handle,&closeHandle);
    if(handle->type==UV_TTY)
        uv_tty_reset_mode();
}

DLLEXPORT void jl_uv_associate_julia_struct(uv_handle_t *handle, jl_value_t *data)
{
    handle->data = data;
}

DLLEXPORT int16_t jl_start_reading(uv_stream_t *handle)
{
    if(!handle)
        return -2;
    return uv_read_start(handle,&jl_alloc_buf,&jl_readcb);
}

DLLEXPORT int jl_listen(uv_stream_t* stream, int backlog)
{
    return uv_listen(stream,backlog,&jl_connectioncb);
}
#ifdef __APPLE__
#include <crt_externs.h>
#endif
DLLEXPORT uv_process_t *jl_spawn(char *name, char **argv, uv_loop_t *loop,
                                 jl_value_t *julia_struct,
                                 uv_pipe_t *stdin_pipe,
                                 uv_pipe_t *stdout_pipe,
                                 uv_pipe_t *stderr_pipe)
{
#ifdef __APPLE__
    char **environ = *_NSGetEnviron();
#endif
    uv_process_t *proc = malloc(sizeof(uv_process_t));
    uv_process_options_t opts;
    uv_stdio_container_t stdio[3];
    int error;
    opts.file = name;
#ifndef __WIN32__
    opts.env = environ;
#else
    opts.env = NULL;
#endif
    opts.cwd = NULL;
    opts.args = argv;
    opts.flags = 0;
    opts.stdio = stdio;
    opts.stdio_count = 3;
    stdio[0].type = UV_STREAM;
    stdio[0].data.stream = (uv_stream_t*)(stdin_pipe);
    stdio[1].type = UV_STREAM;
    stdio[1].data.stream = (uv_stream_t*)(stdout_pipe);
    stdio[2].type = UV_STREAM;
    stdio[2].data.stream = (uv_stream_t*)(stderr_pipe);
    //opts.detached = 0; #This has been removed upstream to be uncommented once it is possible again
    opts.exit_cb = &jl_return_spawn;
    error = uv_spawn(loop,proc,opts);
    if(error) {
        free(proc);
        jl_errorf("Failed to create process %s: %d",name,error);
    }
    proc->data = julia_struct;
    return proc;
}

#ifdef __WIN32__
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

DLLEXPORT uv_loop_t *jl_local_event_loop()
{
    return jl_event_loop;
}

DLLEXPORT void jl_async_send(uv_async_t *handle) {
    if(handle) uv_async_send(handle);
}

DLLEXPORT int jl_idle_start(uv_idle_t *idle)
{
    if(!idle||(idle)->data)
        jl_error("jl_idle_start: Invalid handle");
    return uv_idle_start(idle,(uv_idle_cb)&jl_asynccb);
}

//units are in ms
DLLEXPORT int jl_timer_start(uv_timer_t* timer, int64_t timeout, int64_t repeat)
{
    return uv_timer_start(timer,(uv_timer_cb)&jl_asynccb,timeout,repeat);
}

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
      0,  1,  2,  3,   4,  5,  6,  7,
      8,  9, 10, 11, 12, 13, 13, 15,
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
    167,177,178,179,180,181,182,183,
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

void jl_free_buffer() {}

DLLEXPORT int jl_putc(unsigned char c, uv_stream_t *stream)
{
    if(stream->type<UV_HANDLE_TYPE_MAX) { //is uv handle
        uv_write_t *uvw = malloc(sizeof(uv_write_t));
        uv_buf_t buf[]  = {{.base = chars+c,.len=1}};
        return uv_write(uvw,stream,buf,1,&jl_free_buffer);
    } else {
        ios_t *handle = (ios_t*)stream;
        return ios_putc(c,handle);
    }
}

DLLEXPORT int jl_write(uv_stream_t *stream,char *str,size_t n)
{
    if(stream->type<UV_HANDLE_TYPE_MAX) { //is uv handle
        uv_write_t *uvw = malloc(sizeof(uv_write_t));
        uv_buf_t buf[]  = {{.base = str,.len=n}};
        return uv_write(uvw,stream,buf,1,&jl_free_buffer);
    } else {
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
    va_copy(al, args);

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

DLLEXPORT size_t jl_sizeof_uv_stream_t()
{
    return sizeof(uv_stream_t);
}

DLLEXPORT size_t jl_sizeof_uv_pipe_t()
{
    return sizeof(uv_pipe_t);
}

DLLEXPORT void jl_exit(int exitcode)
{
    /*if(jl_io_loop) {
        jl_process_events(&jl_io_loop);
    }*/
    uv_tty_reset_mode();
    exit(exitcode);
}

DLLEXPORT int jl_cwd(char *buffer, size_t size)
{
    return (uv_cwd(buffer,size)).code;
}

DLLEXPORT int jl_getpid()
{
#ifdef __WIN32__
    return GetCurrentProcessId();
#else
    return getpid();
#endif
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

#ifndef __WIN32__
#include <sys/types.h>
#include <ifaddrs.h>
DLLEXPORT
void getlocalip(char *buf, size_t len)
{
    struct ifaddrs * ifAddrStruct=NULL;
    struct ifaddrs * ifa=NULL;
    void * tmpAddrPtr=NULL;
    buf[0] = '\0';

    getifaddrs(&ifAddrStruct);

    for (ifa = ifAddrStruct; ifa != NULL; ifa = ifa->ifa_next) {
        if (ifa->ifa_addr && ifa->ifa_addr->sa_family==AF_INET) { // check it is IP4
            // is a valid IP4 Address
            tmpAddrPtr=&((struct sockaddr_in *)ifa->ifa_addr)->sin_addr;
            inet_ntop(AF_INET, tmpAddrPtr, buf, len);
            if (strcmp(buf,"127.0.0.1"))
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
    if (ifAddrStruct!=NULL) freeifaddrs(ifAddrStruct);
}
#endif

/*
void jl_addinfo_cb(uv_getaddrinfo_t* handle, int status, struct addrinfo* res)
{
    if(handle->data)
    {
        jl_callback_call(((jl_handle_opts_t*)handle->data)->cb,2,CB_PTR,res,CB_INT32,status);
        free(handle->data);

    }
    free(handle);
    uv_freeaddrinfo(res);
}

DLLEXPORT int jl_getaddrinfo(uv_loop_t *loop, const char *host, const char *service, jl_function_t *cb)
{
    uv_getaddrinfo_t *req = malloc(sizeof(uv_getaddrinfo_t));
    jl_handle_opts_t *opts = malloc(sizeof(jl_handle_opts_t));
    struct addrinfo hints;

    memset (&hints, 0, sizeof (hints));
    hints.ai_family = AF_INET; //ipv4 for now
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags |= AI_CANONNAME;

    opts->cb = cb;

    req->data = opts;

    return uv_getaddrinfo(loop,req,jl_addinfo_cb,host,service,&hints);
}*/

DLLEXPORT struct sockaddr *jl_sockaddr_from_addrinfo(struct addrinfo *addrinfo)
{
    struct sockaddr*addr=malloc(sizeof(struct sockaddr));
    memcpy(addr,addrinfo->ai_addr,sizeof(struct sockaddr));
    return addr;
}

DLLEXPORT void jl_sockaddr_set_port(struct sockaddr *addr,uint16_t port)
{
    if(addr->sa_family==AF_INET)
    {
        ((struct sockaddr_in*)addr)->sin_port=port;
    } else {
        ((struct sockaddr_in6*)addr)->sin6_port=port;
    }
}



DLLEXPORT int jl_connect_raw(uv_tcp_t *handle,struct sockaddr *addr,jl_function_t *connectcb)
{
    uv_connect_t *req = malloc(sizeof(uv_connect_t));
    if(addr->sa_family==AF_INET)
    {
        return uv_tcp_connect(req,handle,*((struct sockaddr_in*)addr),&jl_connectcb);
    } else {
        return uv_tcp_connect6(req,handle,*((struct sockaddr_in6*)addr),&jl_connectcb);
    }
    return -2; //error! Only IPv4 and IPv6 are implemented atm
}

DLLEXPORT int jl_last_errno(uv_loop_t *loop)
{
    return (uv_last_error(loop)).code;
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

//#include "os_detect.h"

#ifdef __cplusplus
}
#endif
