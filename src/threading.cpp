#include "julia.h"
#include <stdio.h>
#include <math.h>
#include <iostream>
#include "uv.h"

extern "C" {

extern jl_function_t *jl_get_specialization(jl_function_t *f, jl_tuple_t *types);
extern jl_tuple_t *arg_type_tuple(jl_value_t **args, size_t nargs);

static uv_mutex_t global_mutex;
static uv_mutex_t gc_mutex;
static long gc_thread_id = -1;
static int nested_gc = 0;
static long mainthread_id = -1;
uv_mutex_t inference_mutex;
uv_mutex_t cache_mutex;

void jl_init_threading()
{
  uv_mutex_init(&global_mutex);
  uv_mutex_init(&gc_mutex);
  uv_mutex_init(&inference_mutex);
  uv_mutex_init(&cache_mutex);
  mainthread_id = uv_thread_self();
}

void jl_global_lock()
{
  uv_mutex_lock(&global_mutex);
}

void jl_global_unlock()
{
  uv_mutex_unlock(&global_mutex);
}

int jl_gc_lock()
{
  if(!nested_gc && gc_thread_id != uv_thread_self() && mainthread_id != uv_thread_self())
  {
    uv_mutex_lock(&gc_mutex);
    nested_gc = 1;
    gc_thread_id = uv_thread_self();
    return 1;
  }
  return 0;
}

void jl_gc_unlock()
{
  nested_gc = 0;
  gc_thread_id = -1;
  uv_mutex_unlock(&gc_mutex);
}

void run_thread(void* t)
{
  jl_function_t* f = ((jl_thread_t*)t)->f;
  jl_tuple_t* targs = ((jl_thread_t*)t)->targs;

  jl_value_t** args = (jl_value_t**) alloca( sizeof(jl_value_t*)*jl_tuple_len(targs));
  for(int l=0; l<jl_tuple_len(targs); l++)
    args[l] = jl_tupleref(targs,l);

  jl_apply(f,args,jl_tuple_len(targs));
}

jl_thread_t* jl_create_thread(jl_function_t* f, jl_tuple_t* targs)
{
  int nargs = jl_tuple_len(targs);
  jl_thread_t* t = (jl_thread_t*) malloc(sizeof(jl_thread_t));
            
  jl_tuple_t* argtypes = arg_type_tuple(&jl_tupleref(targs,0), nargs);
  t->f = jl_get_specialization(f, argtypes);
  //jl_compile(t->f);
  t->targs = targs;
  return t;
}
  
void jl_run_thread(jl_thread_t* t)
{
  uv_thread_create(&(t->t), run_thread, t);
}

void jl_join_thread(jl_thread_t* t)
{
  uv_thread_join(&(t->t));
}

void jl_destroy_thread(jl_thread_t* t)
{
  free(t);
}

static uv_mutex_t parapply_mutex;

typedef struct {
  uv_thread_t t;
  jl_function_t *f; 
  jl_value_t** args; 
  int nargs; 
  size_t start; 
  size_t step; 
  size_t length;
} parapply_thread_t;

void parapply_do_work(void* av)
{
    parapply_thread_t* a = (parapply_thread_t*) av;
    //printf("do_work(start=%ld,stop=%ld) \n", start, a->start+a->length*a->step);
    
    jl_value_t** args = (jl_value_t**) alloca(sizeof(jl_value_t*)*a->nargs);
    for(int i=0; i< a->nargs -1; i++)
      args[i] = a->args[i];
    
    for(size_t i=a->start; i<a->start+a->length*a->step; i+=a->step)
    {
        uv_mutex_lock(&parapply_mutex);
        args[a->nargs-1] = jl_box_int64(i);
        uv_mutex_unlock(&parapply_mutex);
        
        jl_apply(a->f,args,a->nargs);
    }
}

void jl_par_apply(jl_function_t * func, jl_value_t* targs, size_t num_threads, size_t start, size_t step, size_t length)
{
    jl_gc_disable();
    
    uv_mutex_init(&parapply_mutex);
    
    int nargs = jl_tuple_len(targs)+1;
    
    // convert tuple into array
    jl_value_t** args;    
    JL_GC_PUSHARGS(args, nargs);
    for(int l=0; l<jl_tuple_len(targs); l++)
      args[l] = jl_tupleref(targs,l);

    //jl_methtable_t *mt = jl_gf_mtable(func);
    //jl_function_t *mfunc = jl_method_table_assoc_exact(mt, args, nargs);
    
    jl_tuple_t* firstargtypes = arg_type_tuple(&jl_tupleref(targs,0), nargs-1);
    jl_tuple_t* argtypes = jl_alloc_tuple(nargs);
    for(int l=0; l<nargs-1; l++)
      jl_tupleset(argtypes, l, jl_tupleref(firstargtypes,l));
    jl_tupleset(argtypes, nargs-1, jl_int64_type);
    jl_function_t *mfunc = jl_get_specialization(func, argtypes);

    parapply_thread_t* t = (parapply_thread_t*) alloca(sizeof(parapply_thread_t)*num_threads);

    size_t chunk = length / num_threads;
    size_t rem = length;
    for(size_t i=0; i < num_threads; i++)
    {
      t[i].f = mfunc;
      t[i].args = args;
      t[i].nargs = nargs;
      t[i].start = start+i*chunk*step;
      t[i].step = step;
      t[i].length = i < num_threads-1 ? chunk : rem;
   
      uv_thread_create(&(t[i].t), parapply_do_work, &t[i]);
      rem -= chunk;
    }

    // std::cout << "Launched from the main thread \n";

    for(size_t i=0; i < num_threads; i++)
    {
      uv_thread_join(&(t[i].t));
    }

    uv_mutex_destroy(&parapply_mutex);
    
    jl_gc_enable();
    JL_GC_POP();
}




}
