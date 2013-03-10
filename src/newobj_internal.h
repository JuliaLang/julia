#ifndef NEWOBJ_INTERNAL_H
#define NEWOBJ_INTERNAL_H

static inline jl_value_t *newobj(jl_value_t *type, size_t nfields)
{
    jl_value_t *jv = (jl_value_t*)allocobj((1+nfields) * sizeof(void*));
    jv->type = type;
    return jv;
}

static inline jl_value_t *newstruct(jl_datatype_t *type)
{
    jl_value_t *jv = (jl_value_t*)allocobj(sizeof(void*) + type->size);
    jv->type = (jl_value_t*)type;
    return jv;
}

int jl_assign_type_uid(void);

#endif
