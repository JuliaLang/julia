#include "julia.h"
#include "uv.h"

const int sizeof_uv_thread_t = sizeof(uv_thread_t);

typedef struct _function_pack_t {
  jl_function_t *F;
  jl_value_t   **args;
  uint32_t       nargs;
  jl_value_t    *ret;
} function_pack_t;

void _function_pack_apply(void *vp)
{
  function_pack_t *fp = (function_pack_t *) vp;
  fp->ret = jl_apply(fp->F, fp->args, fp->nargs);
} 

JL_CALLABLE(jl_f_thread_create)
{
  JL_NARGSV(thread_create, 1);
  JL_TYPECHK(thread_create, function, args[0]);
  function_pack_t thread_data;
  thread_data.F = (jl_function_t*) args[0];
  thread_data.args = args+1;
  thread_data.nargs = nargs-1;

  jl_array_t *jl_threadid = jl_alloc_array_1d(jl_array_uint8_type, sizeof_uv_thread_t);
  /*
  if (uv_thread_create((uv_thread_t *) jl_array_data(jl_threadid), _function_pack_apply, &thread_data) != 0)
    jl_error("thread_create: error launching thread");
  */
  JL_PRINTF(JL_STDOUT, "You want a new thread, but I'm going to fake it.\n");
  _function_pack_apply((void*) &thread_data);

  return (jl_value_t*)jl_threadid;
}

JL_CALLABLE(jl_f_thread_join)
{
  JL_NARGS(thread_join, 1, 1);
  JL_TYPECHK(thread_join, array, args[0]);

  /*
  if (uv_thread_join((uv_thread_t *) jl_array_data(args[0])) != 0)
    jl_error("thread_join: error joining thread");
  */
  JL_PRINTF(JL_STDOUT, "I'd be happy to join that for you, if there were anything to join.\n");
  
  return (jl_value_t*)jl_null;
}
