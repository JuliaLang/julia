#include <stddef.h>
#include <stdint.h>

#include "julia.h"
#include "support/ios.h"
#include "uv.h"

#ifdef __cplusplus
extern "C" {
#endif

/** This file contains wrappers for most of libuv's stream functionailty. Once we can allocate structs in Julia, this file will be removed */

typedef struct {
    jl_callback_t *readcb;
    ios_t *stream;
} jl_pipe_opts_t;

typedef struct {
    jl_callback_t *exitcb;
    jl_callback_t *closecb;
    uv_pipe_t* in;
    uv_pipe_t* out;
} jl_proc_opts_t;

typedef struct {
    jl_callback_t *callcb;
} jl_async_opts_t;


static uv_buf_t jl_alloc_buf(uv_handle_t* handle, size_t suggested_size) {
    uv_buf_t buf;
    if(!handle->data)
        jl_error("jl_alloc_buf: Missing data");
    jl_pipe_opts_t *opts = (jl_pipe_opts_t*)handle->data;
    buf.len = ios_fillprep(opts->stream,suggested_size);
    buf.base = opts->stream->buf + opts->stream->bpos;
    return buf;
}


void closeHandle(uv_handle_t* handle)
{
    switch(handle->type) {
    case UV_NAMED_PIPE:
        if(handle->data) {
            jl_pipe_opts_t *opts=handle->data;
            free(opts->readcb);
            free(opts);
        } break;
    case UV_PROCESS:
        if(handle->data) {
            jl_proc_opts_t *opts=handle->data;
            if(opts->closecb) {
                jl_callback_call(opts->closecb,handle);
                free(opts->closecb);
            }
            free(opts->exitcb);
            free(opts);
            //pipes have to be closed where they are created to support reusing them
        } break;
    case UV_IDLE: //fall through to async
    case UV_ASYNC:
        if(handle->data) {
            jl_async_opts_t *opts=handle->data;
            if(opts->callcb)
                free(opts->callcb);
            free(opts);
        }
    default:
        break;
    }
    free(handle);
}

DLLEXPORT void jl_run_event_loop(uv_loop_t **loop)
{
    if(loop) uv_run(*loop);
}

DLLEXPORT void jl_process_events(uv_loop_t **loop)
{
    if(loop) uv_run_once(*loop);
}


void jl_return_spawn(uv_process_t *p, int exit_status, int term_signal) {
    jl_proc_opts_t *opts = p->data;
    if(opts) {
        if(opts->exitcb)
            jl_callback_call(opts->exitcb,p,exit_status,term_signal);
    }
    uv_close(p,&closeHandle);
}

void jl_callback_call(jl_callback_t *cb,...)
{
    va_list argp;
    va_start(argp,cb);
    jl_value_t **argv = malloc(sizeof(jl_value_t*)*cb->types->length);
    jl_value_t *v;
    argv[0]=cb->state;
    for(int i = 0; i<cb->types->length; ++i) {
        v = alloc_2w(); //only primitives
        v->type = jl_tupleref(cb->types,i);
        *jl_bits_data(v)=va_arg(argp,void*);
        argv[i+1]=v;
    }
    JL_GC_PUSH(argv);
    jl_apply(cb->function,(jl_value_t**)argv,cb->types->length+1);
    JL_GC_POP();
}

void jl_readcb(uv_stream_t *handle, ssize_t nread, uv_buf_t buf)
{
    jl_pipe_opts_t *opts;
    if(handle->data) {
        opts = handle->data;
        if(nread>0) { //no error/EOF
            opts->stream->size+=nread;
            opts->stream->bpos+=nread;
        }
        if(opts->readcb) {
            jl_callback_call(opts->readcb,nread,((jl_pipe_opts_t*)handle->data)->stream->buf,(buf.base),buf.len);
        }
    }
}

DLLEXPORT uv_pipe_t *jl_make_pipe(ios_t *stream)
{
    jl_pipe_opts_t *opts = malloc(sizeof(jl_pipe_opts_t));
    uv_pipe_t *pipe = malloc(sizeof(uv_pipe_t));
    uv_pipe_init(jl_event_loop,pipe,0);
    pipe->data = 0;//will be initilized on io
#ifdef __WIN32__
    pipe->handle=0;
#endif
    return pipe;
}

DLLEXPORT void jl_close_uv(uv_handle_t **handle)
{
    if(handle) uv_close(*handle,&closeHandle);
}

DLLEXPORT uint16_t jl_start_reading(uv_stream_t **handle, ios_t *iohandle,void **callback)
{
    if(!handle||(*handle)->data)
        return -2;
    jl_pipe_opts_t *opts = malloc(sizeof(jl_pipe_opts_t));
    opts->readcb=callback?(jl_callback_t*)*callback:0;
    opts->stream = iohandle;
    (*handle)->data = opts;
    return uv_read_start(*handle,&jl_alloc_buf,&jl_readcb);
}

DLLEXPORT uint16_t jl_stop_reading(uv_stream_t **handle)
{
    if(!handle)
        return -1;
    int err = uv_read_stop(*handle);
    if(!(*handle)->data)
        return -2;
    ((jl_pipe_opts_t*)(*handle)->data)->readcb=0;
    return err;

}

DLLEXPORT void jl_callback(void **callback)
{
    if(callback) {
        jl_callback_t *cb=(jl_callback_t *)*(callback);
        jl_callback_call(cb,"test_callback.j");
    }
}

DLLEXPORT uv_process_t *jl_spawn(char *name, char **argv, uv_pipe_t **stdin_pipe, uv_pipe_t **stdout_pipe, void **exitcb, void **closecb)
{
    jl_proc_opts_t *jlopts=malloc(sizeof(jl_proc_opts_t));
    uv_process_t *proc = malloc(sizeof(uv_process_t));
    uv_process_options_t opts;
    int error;
    opts.file = name;
    opts.env = NULL;
    opts.cwd = NULL;
    opts.args = argv;
    opts.stdin_stream = jlopts->in = stdin_pipe ? *stdin_pipe : 0;
    opts.stdout_stream = jlopts->out = stdout_pipe ? *stdout_pipe :0;
    opts.stderr_stream = NULL;
    //opts.detached = 0; #This has been removed upstream to be uncommented once it is possible again
    opts.exit_cb = &jl_return_spawn;
    jlopts->exitcb=exitcb?*exitcb:0;
    jlopts->closecb=closecb?*closecb:0;
    error = uv_spawn(jl_event_loop,proc,opts);
    if(error)
        jl_errorf("Failed to create process %s: %d",name,error);
    proc->data = jlopts;
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

void jl_async_callback(uv_handle_t *handle, int status)
{
    if(handle->data) {
        jl_async_opts_t* opts = handle->data;
        if(opts->callcb)
            jl_callback_call(opts->callcb,handle,status);
    }
}

DLLEXPORT uv_async_t *jl_make_async(uv_loop_t *loop,jl_callback_t **cb)
{
    if(!loop)
        return 0;
    uv_async_t *async = malloc(sizeof(uv_async_t));
    jl_async_opts_t *opts = malloc(sizeof(jl_async_opts_t));
    opts->callcb = cb?*cb:0;
    uv_async_init(loop,async,&jl_async_callback);
    async->data=opts;
    return async;
}

DLLEXPORT void jl_async_send(uv_async_t **handle) {
    if(handle) uv_async_send(*handle);
}

DLLEXPORT int jl_idle_init(uv_loop_t *loop)
{
    if(!loop)
        return -2;
    uv_idle_t *idle = malloc(sizeof(uv_idle_t));
    uv_idle_init(loop,idle);
    idle->data = 0;
    return idle;
}

DLLEXPORT int jl_idle_start(uv_idle_t **idle, jl_callback_t **cb)
{
    if(!idle||(*idle)->data)
        return -2;
    jl_async_opts_t *opts = malloc(sizeof(jl_async_opts_t));
    opts->callcb = cb?*cb:0;
    (*idle)->data=opts;
    return uv_idle_start(*idle,&jl_async_callback);
}

DLLEXPORT int jl_idle_stop(uv_idle_t **idle) {
    if(!idle)
        return -2;
    if((*idle)->data) {
        free(((jl_async_opts_t *)(*idle)->data)->callcb);
        free((*idle)->data);
    }
    return uv_idle_stop(*idle);
}

void jl_free_buffer(uv_write_t *uvw, int status) {
    free(uvw);
}

DLLEXPORT int jl_puts(char *str, uv_stream_t *stream)
{
    jl_write(stream,str,strlen(str));
}

DLLEXPORT int jl_pututf8(uv_stream_t *s, uint32_t wchar )
{
    char buf[8];
    if (wchar < 0x80)
        return jl_putc((int)wchar, s);
    size_t n = u8_toutf8(buf, 8, &wchar, 1);
    return jl_write(s, buf, n);
}

static unsigned char chars[] = {
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
    240,241,424,243,244,245,246,247,
    248,249,250,251,252,253,254,255
};

DLLEXPORT int jl_putc(char c, uv_stream_t *stream)
{
    if(stream->type<UV_FS_EVENT) { //is uv handle
        uv_write_t *uvw = malloc(sizeof(uv_write_t));
        uv_buf_t buf[]  = {{.base = chars+c,.len=1}};
        return uv_write(uvw,stream,buf,1,&jl_free_buffer);
    } else {
        ios_t *handle = stream;
        ios_putc(c,handle);
    }
}

DLLEXPORT int jl_write(uv_stream_t *stream,char *str,size_t n)
{
    if(stream->type<UV_FS_EVENT) { //is uv handle
        uv_write_t *uvw = malloc(sizeof(uv_write_t));
        uv_buf_t buf[]  = {{.base = str,.len=n}};
        return uv_write(uvw,stream,buf,1,&jl_free_buffer);
    } else
    {
        ios_t *handle = stream;
        ios_write(handle,str,n);
    }
}

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

DLLEXPORT size_t jl_sizeof_uv_stream_t()
{
    return sizeof(uv_stream_t*);
}

DLLEXPORT void jl_exit(int exitcode)
{
    if(jl_io_loop) {
        jl_process_events(&jl_io_loop);
    }
    exit(exitcode);
}

#ifdef __cplusplus
}
#endif
