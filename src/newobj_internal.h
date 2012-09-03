#ifndef NEWOBJ_INTERNAL_H
#define NEWOBJ_INTERNAL_H

static inline jl_value_t *newobj(jl_type_t *type, size_t nfields)
{
    jl_value_t *jv = (jl_value_t*)allocobj((1+nfields) * sizeof(void*));
    jv->type = type;
    return jv;
}

static inline jl_value_t *newstruct(jl_struct_type_t *type)
{
    jl_value_t *jv = (jl_value_t*)allocobj(sizeof(void*) + type->size);
    jv->type = (jl_type_t*)type;
    return jv;
}

#define TAG_TYPE_NW (NWORDS(sizeof(jl_tag_type_t))-1)
#define BITS_TYPE_NW (NWORDS(sizeof(jl_bits_type_t))-1)

int jl_assign_type_uid(void);

#endif
