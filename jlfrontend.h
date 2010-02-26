___SCMOBJ jl_scm_parse_string(char *str);
___SCMOBJ jl_scm_parse_file(char *fname);

___SCMOBJ jl_get_sym(char *name);
char *jl_scm_str(___SCMOBJ x);
uint64_t jl_scm_uint64(___SCMOBJ x);
double jl_scm_float64(___SCMOBJ x);
int jl_scm_integerp(___SCMOBJ x);
