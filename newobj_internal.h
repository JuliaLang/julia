#ifndef _NEWOBJ_INTERNAL_H_
#define _NEWOBJ_INTERNAL_H_

static inline jl_value_t *newobj(jl_type_t *type, size_t nfields)
{
    jl_value_t *jv = (jl_value_t*)allocb((1+nfields) * sizeof(void*));
    jv->type = type;
    return jv;
}

#define TAG_TYPE_NW (NWORDS(sizeof(jl_tag_type_t))-1)
#define STRUCT_TYPE_NW (NWORDS(sizeof(jl_struct_type_t))-1)
#define BITS_TYPE_NW (NWORDS(sizeof(jl_bits_type_t))-1)

int jl_assign_type_uid();

#endif
