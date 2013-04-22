#ifndef JLTYPES_INTERNAL_H
#define JLTYPES_INTERNAL_H

jl_value_t *jl_cache_type_(jl_datatype_t *type);

int  jl_get_t_uid_ctr(void);
void jl_set_t_uid_ctr(int i);
uint32_t jl_get_gs_ctr(void);
void jl_set_gs_ctr(uint32_t ctr);

#endif
