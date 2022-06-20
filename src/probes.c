#include <string.h>
#include "julia.h"
#include "julia_internal.h"

// FIXME: htable
arraylist_t *jl_probes;
jl_mutex_t jl_probes_mutex;

void jl_init_probes(void) {
    jl_probes = arraylist_new((arraylist_t *)malloc(sizeof(arraylist_t)), 1);
    JL_MUTEX_INIT(&jl_probes_mutex);
}

JL_DLLEXPORT jl_probe_spec_t *jl_probe_register(const char *name) {
    JL_LOCK(&jl_probes_mutex);

    jl_probe_spec_t *spec;
    for (int i = 0; i < jl_probes->len; i++) {
        spec = (jl_probe_spec_t *)jl_probes->items[i];
        if (strcmp(spec->name, name) == 0) {
            JL_UNLOCK(&jl_probes_mutex);
            return spec;
        }
    }
    spec = (jl_probe_spec_t *)calloc(1, sizeof(jl_probe_spec_t));
    spec->name = strdup(name);
    //spec->probe_addr = calloc(1, sizeof(void*));
    //spec->semaphore_addr = calloc(1, sizeof(int64_t));
    arraylist_push(jl_probes, (void *)spec);

    JL_UNLOCK(&jl_probes_mutex);
    return spec;
}

JL_DLLEXPORT jl_probe_spec_t *jl_probe_lookup(const char *name) {
    JL_LOCK(&jl_probes_mutex);
    for (int i = 0; i < jl_probes->len; i++) {
        jl_probe_spec_t *spec = (jl_probe_spec_t *)jl_probes->items[i];
        if (strcmp(spec->name, name) == 0) {
            JL_UNLOCK(&jl_probes_mutex);
            return spec;
        }
    }
    JL_UNLOCK(&jl_probes_mutex);
    return (jl_probe_spec_t *)NULL;
}
