#include <stddef.h>
#include <stdint.h>

#include "julia.h"
#include "support/ios.h"
#include "uv.h"

#ifdef __cplusplus
extern "C" {
#endif

/** This file contains wrappers for most of libuv's stream functionailty. Once we can allocate structs in Julia, this file will be removed */

DLLEXPORT char* jl_getenv(char *name)
{
    return getenv(name);
}

typedef struct {
    jl_callback_t *readcb;
    ios_t *stream;
} jl_pipe_opts_t;

typedef struct {
    jl_callback_t *exitcb;
    uv_pipe_t* in;
    uv_pipe_t* out;
} jl_proc_opts_t;

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
            ios_close(opts->stream);
            free(opts->readcb);
        } break;
    case UV_PROCESS:
        if(handle->data) {
            jl_proc_opts_t *opts=handle->data;
            free(opts->exitcb);
            //pipes have to be closed where they are created to support reusing them
        }
    default:
        break;
    }
    free(handle);
}

DLLEXPORT void jl_run_event_loop()
{
    uv_run(jl_event_loop);
}

DLLEXPORT void jl_process_events()
{
    uv_run_once(jl_event_loop);
}


void jl_return_spawn(uv_process_t *p, int exit_status, int term_signal) {
    jl_proc_opts_t *opts;
    int c = uv_loop_refcount(jl_event_loop);
    if(p->data) {
        opts = p->data;
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
        if(opts->readcb) {
            jl_callback_call(opts->readcb,nread,((jl_pipe_opts_t*)handle->data)->stream->buf,(buf.base),buf.len);
        }
    }
}

DLLEXPORT uv_pipe_t *jl_make_pipe()
{
    uv_pipe_t *pipe = malloc(sizeof(uv_pipe_t));
    return uv_pipe_init(jl_event_loop,pipe,1);
}

DLLEXPORT void jl_close_uv(uv_handle_t *handle)
{
    uv_close(handle,&closeHandle);
}

DLLEXPORT uint16_t jl_start_reading(uv_stream_t *handle, ios_t *iohande,void **callback)
{
    if(handle->data)
        return -2;
    jl_pipe_opts_t *opts = malloc(sizeof(jl_pipe_opts_t));
    if(callback)
        opts->readcb=(jl_callback_t*)*callback;
    uv_read_start(handle,&jl_alloc_buf,jl_readcb);
}

DLLEXPORT void jl_callback(void **callback)
{
    if(callback) {
        jl_callback_t *cb=(jl_callback_t *)*(callback);
        jl_callback_call(cb,"test_callback.j");
    }
}

DLLEXPORT uv_process_t *jl_spawn(char *name, char **argv, uv_pipe_t **stdin_pipe, uv_pipe_t **stdout_pipe, void **callback)
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
    opts.detached = 0;
    opts.exit_cb = &jl_return_spawn;
    jlopts->exitcb=callback;
    error = uv_spawn(jl_event_loop,proc,opts);
    if(error)
        jl_errorf("Failed to create process %s: %d",name,error);
    proc->data = jlopts;
    return proc;
}


#ifdef __cplusplus
}
#endif
