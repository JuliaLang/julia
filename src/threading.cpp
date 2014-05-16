#include "julia.h"
#include "julia_internal.h"

#include <stdio.h>
#include <math.h>
#include "uv.h"

extern "C" {

long jl_main_thread_id = -1;

JL_DEFINE_MUTEX(gc)
JL_DEFINE_MUTEX(codegen)
uv_mutex_t gc_pool_mutex[N_GC_THREADS];

#if N_THREAD_POOL > 0
jl_thread_t thread_pool[N_THREAD_POOL];
int finish_thread_pool = 0;
#endif

#if N_THREAD_POOL > 0
void run_pool_thread(void* t_)
{
    jl_thread_t* t = (jl_thread_t*)t_;
    uv_mutex_lock(&t->m);
    while(true)
    {
        // wait for the next task
        while(!t->busy)
            uv_cond_wait(&t->c, &t->m);
        
        if(finish_thread_pool)
          break;
        
        uv_mutex_unlock(&t->m);

        jl_value_t** args = (jl_value_t**) alloca( sizeof(jl_value_t*)*jl_tuple_len(t->targs));
        for(int l=0; l<jl_tuple_len(t->targs); l++)
            args[l] = jl_tupleref(t->targs,l);

        jl_apply(t->f,args,jl_tuple_len(t->targs));
        
        t->busy = 0;
        uv_cond_signal(&t->c);
        uv_mutex_lock(&t->m);
    }
    uv_mutex_unlock(&t->m);
}
#endif

void run_standalone_thread(void* t)
{
    jl_function_t* f = ((jl_thread_t*)t)->f;
    jl_tuple_t* targs = ((jl_thread_t*)t)->targs;

    jl_value_t** args = (jl_value_t**) alloca( sizeof(jl_value_t*)*jl_tuple_len(targs));
    for(int l=0; l<jl_tuple_len(targs); l++)
      args[l] = jl_tupleref(targs,l);

    jl_apply(f,args,jl_tuple_len(targs));
}

void jl_init_threading()
{
    uv_mutex_init(&gc_mutex);
    uv_mutex_init(&codegen_mutex);
    for(int n=0; n<N_GC_THREADS; n++)
        uv_mutex_init(gc_pool_mutex+n);
    jl_main_thread_id = uv_thread_self();
    
    #if N_THREAD_POOL > 0
    for(int n=0; n<N_THREAD_POOL; n++)
    {
        uv_mutex_init(&thread_pool[n].m);
        uv_cond_init(&thread_pool[n].c);
        thread_pool[n].poolid = n;
        thread_pool[n].busy = 0;
        uv_thread_create(&thread_pool[n].t, run_pool_thread, &thread_pool[n]);
    }
    #endif
}

// where to call this ???
void jl_cleanup_threading()
{
    #if N_THREAD_POOL > 0
    finish_thread_pool = true;
    for(int n=0; n<N_THREAD_POOL; n++)
    {
        uv_cond_signal(&thread_pool[n].c);
        uv_thread_join(&thread_pool[n].t);
        
        uv_mutex_destroy(&thread_pool[n].m);
        uv_cond_destroy(&thread_pool[n].c);        
    }
    #endif
    
    uv_mutex_destroy(&gc_mutex);
    uv_mutex_destroy(&codegen_mutex);
    for(int n=0; n<N_GC_THREADS; n++)
        uv_mutex_destroy(gc_pool_mutex+n);    
}

jl_thread_t* jl_create_thread(jl_function_t* f, jl_tuple_t* targs)
{
    int nargs = jl_tuple_len(targs);
    
    #if N_THREAD_POOL > 0
    for(int n=0; n<N_THREAD_POOL; n++)
    {
      if(!thread_pool[n].busy &&  uv_mutex_trylock(&thread_pool[n].m) == 0)
      {
        thread_pool[n].busy = 1;
        jl_tuple_t* argtypes = arg_type_tuple(&jl_tupleref(targs,0), nargs);
        thread_pool[n].f = jl_get_specialization(f, argtypes);
        jl_compile(thread_pool[n].f); // does this make sense here?
        thread_pool[n].targs = targs;
        
        return &thread_pool[n];
      }
    }
    #endif
    
    // Thread pool is full. Create new thread
    jl_thread_t* t = (jl_thread_t*) malloc(sizeof(jl_thread_t));
    t->poolid = -1; // This tells us that this thread is standalone
    
    jl_tuple_t* argtypes = arg_type_tuple(&jl_tupleref(targs,0), nargs);
    t->f = jl_get_specialization(f, argtypes);
    jl_compile(t->f); // does this make sense here?
    t->targs = targs;
    return t;    
}

void jl_run_thread(jl_thread_t* t)
{
    #if N_THREAD_POOL > 0
    if(t->poolid != -1)
    {
        t->busy = 1;
        uv_cond_signal(&t->c); // notify thread that it now can proceed
        uv_mutex_unlock(&t->m);
    }
    else
    #endif
        uv_thread_create(&(t->t), run_standalone_thread, t);
}

void jl_join_thread(jl_thread_t* t)
{
    #if N_THREAD_POOL > 0
    if(t->poolid != -1) {
        uv_mutex_lock(&t->m);
        while(t->busy)
            uv_cond_wait(&t->c, &t->m);       
        uv_mutex_unlock(&t->m);
    }
    else
    #endif
        uv_thread_join(&(t->t));
}

void jl_destroy_thread(jl_thread_t* t)
{
 #if N_THREAD_POOL > 0
    if(t->poolid == -1)
 #endif
        free(t);
}

// locks

uv_mutex_t* jl_create_mutex()
{
    uv_mutex_t* m = (uv_mutex_t*) malloc(sizeof(uv_mutex_t));
    uv_mutex_init(m);
    return m;
}

void jl_lock_mutex(uv_mutex_t* m)
{
    uv_mutex_lock(m);
}

void jl_unlock_mutex(uv_mutex_t* m)
{
    uv_mutex_unlock(m);
}

void jl_destroy_mutex(uv_mutex_t* m)
{
    uv_mutex_destroy(m);
    free(m);
}

}
