#include <stdio.h>
#include <stdlib.h>
#include <gmp.h>

void _jl_gmp_free(void *p)
{
    void (*freefunc) (void *, size_t);
    mp_get_memory_functions (NULL, NULL, &freefunc);
    freefunc(p, 0);
}

extern void* _jl_mpz_init()
{
  mpz_t* integ = malloc(sizeof(mpz_t));
  mpz_init(*integ);
  return integ;
}

extern void _jl_mpz_clear(mpz_t* rop) {
  mpz_clear(*rop);
}

extern void _jl_mpz_set_string(mpz_t* rop, char* s) {
  mpz_set_str(*rop, s, 0);
}

extern void _jl_mpz_set_ui(mpz_t* rop, unsigned long int op) {
  mpz_set_ui(*rop, op);
} 

extern unsigned long _jl_mpz_get_ui(mpz_t* rop) {
  return mpz_get_ui(*rop);
} 

extern void _jl_mpz_set_si(mpz_t* rop, long int op) {
  mpz_set_si(*rop, op);
} 

extern long _jl_mpz_get_si(mpz_t* rop) {
  return mpz_get_si(*rop);
} 

extern void _jl_mpz_add(mpz_t* rop, mpz_t* op1, mpz_t* op2) {
  mpz_add(*rop, *op1, *op2);
}

extern void _jl_mpz_sub(mpz_t* rop, mpz_t* op1, mpz_t* op2) {
  mpz_sub(*rop, *op1, *op2);
}

extern void _jl_mpz_mul(mpz_t* rop, mpz_t* op1, mpz_t* op2) {
  mpz_mul(*rop, *op1, *op2);
}

extern void _jl_mpz_div(mpz_t* rop, mpz_t* op1, mpz_t* op2) {
  mpz_fdiv_q(*rop, *op1, *op2);
}

extern void _jl_mpz_divmod(mpz_t* rop1, mpz_t* rop2, mpz_t* op1, mpz_t* op2) {
  mpz_divmod(*rop1, *rop2, *op1, *op2);
}
  
extern void _jl_mpz_gcd(mpz_t* rop, mpz_t* op1, mpz_t* op2) {
    mpz_gcd(*rop, *op1, *op2);
}

extern void _jl_mpz_gcdext(mpz_t *g, mpz_t *s, mpz_t *t, mpz_t *a, mpz_t *b) {
    mpz_gcdext(*g, *s, *t, *a, *b);
}

extern void _jl_mpz_rem(mpz_t* rop, mpz_t* op1, mpz_t* op2) {
  mpz_fdiv_r(*rop, *op1, *op2);
}

extern void _jl_mpz_neg(mpz_t* rop, mpz_t* op1) {
  mpz_neg(*rop, *op1);
}

extern void _jl_mpz_abs(mpz_t* rop, mpz_t* op1) {
  mpz_abs(*rop, *op1);
}

extern int _jl_mpz_cmp(mpz_t* op1, mpz_t* op2) {
  return mpz_cmp(*op1, *op2);
}

extern void _jl_mpz_lshift(mpz_t* rop, mpz_t* base, unsigned long int count) {
  mpz_mul_2exp(*rop, *base, count);
}

extern void _jl_mpz_pow_ui(mpz_t* rop, mpz_t* base, unsigned long int exp) {
  mpz_pow_ui(*rop, *base, exp);
}

extern void _jl_mpz_sqrt(mpz_t* rop, mpz_t* op) {
    mpz_sqrt(*rop, *op);
}

extern void _jl_mpz_fac_ui(mpz_t* rop, unsigned long int op) {
  mpz_fac_ui(*rop, op);
}

extern void _jl_mpz_bin_ui(mpz_t* rop, mpz_t* n, unsigned long int k) {
  mpz_bin_ui(*rop, *n, k);
}

extern void _jl_mpz_bin_uiui(mpz_t* rop, unsigned long int n, unsigned long int k) {
  mpz_bin_uiui(*rop, n, k);
}

extern char*  _jl_mpz_printf(mpz_t* rop) {
  char* pp;
  int s = gmp_asprintf(&pp, "%Zd", *rop);
  return pp;
}

//// MPF

extern void* _jl_mpf_init()
{
    mpf_t* flt = malloc(sizeof(mpf_t));
    mpf_init(*flt);
    return flt;
}

extern void _jl_mpf_clear(mpf_t* rop) {
    mpf_clear(*rop);
}

extern void _jl_mpf_set_string(mpf_t* rop, char* s) {
    mpf_set_str(*rop, s, 0);
}

extern void _jl_mpf_set_ui(mpf_t* rop, unsigned long int op) {
    mpf_set_ui(*rop, op);
} 

extern void _jl_mpf_set_si(mpf_t* rop, signed long int op) {
    mpf_set_si(*rop, op);
} 

extern void _jl_mpf_set_d(mpf_t* rop, double op) {
    mpf_set_d(*rop, op);
}

extern void _jl_mpf_set_z(mpf_t* rop, mpz_t* op) {
    mpf_set_z(*rop, *op);
}

extern void _jl_mpf_add(mpf_t* rop, mpf_t* op1, mpf_t* op2) {
    mpf_add(*rop, *op1, *op2);
}

extern void _jl_mpf_sub(mpf_t* rop, mpf_t* op1, mpf_t* op2) {
    mpf_sub(*rop, *op1, *op2);
}

extern void _jl_mpf_mul(mpf_t* rop, mpf_t* op1, mpf_t* op2) {
    mpf_mul(*rop, *op1, *op2);
}

extern void _jl_mpf_div(mpf_t* rop, mpf_t* op1, mpf_t* op2) {
    mpf_div(*rop, *op1, *op2);
}

extern void _jl_mpf_neg(mpf_t* rop, mpf_t* op1) {
    mpf_neg(*rop, *op1);
}

extern void _jl_mpf_abs(mpf_t* rop, mpf_t* op1) {
    mpf_abs(*rop, *op1);
}

extern int _jl_mpf_cmp(mpf_t* op1, mpf_t* op2) {
    return mpf_cmp(*op1, *op2);
}

extern void _jl_mpf_pow_ui(mpf_t* rop, mpf_t* base, unsigned long int exp) {
    mpf_pow_ui(*rop, *base, exp);
}

extern void _jl_mpf_sqrt(mpf_t* rop, mpf_t* op) {
    mpf_sqrt(*rop, *op);
}

extern char*  _jl_mpf_printf(mpf_t* rop) {
    char* pp;
    gmp_asprintf(&pp, "%.Ff", *rop);
    return pp;
}


//Quick and dirty test of the gmp wrapper code
int main( int argc, const char* argv[] )
{
  void* rop = _jl_mpf_init();
  void* op1 = _jl_mpf_init();
    
  _jl_mpf_set_string(op1, "123456789123456789123456789123456789");

  void* op2 = _jl_mpf_init();
  _jl_mpf_set_string(op2, "12345");

  _jl_mpf_add(rop, op1, op2);

  printf("The sum is %s\n", _jl_mpf_printf(rop));

  _jl_mpf_clear(rop);
}



