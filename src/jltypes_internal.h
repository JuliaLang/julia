#ifndef _JLTYPES_INTERNAL_H_
#define _JLTYPES_INTERNAL_H_

typedef struct _typekey_stack_t {
    jl_tag_type_t *type;
    struct _typekey_stack_t *next;
} typekey_stack_t;

void jl_cache_type_(jl_tag_type_t *type);

int  jl_get_t_uid_ctr(void);
void jl_set_t_uid_ctr(int i);
uint32_t jl_get_gs_ctr(void);
void jl_set_gs_ctr(uint32_t ctr);

#endif
