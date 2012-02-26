#include <stdio.h>
#include <stdlib.h>
#include <gmp.h>

extern void* _jl_mpz_init()
{
  mpz_t* integ = malloc(sizeof(mpz_t));
  mpz_init(*integ);
  return integ;
}

extern void _jl_mpz_set_string(mpz_t* rop, char* s) {
  mpz_set_str(*rop, s, 0);
}

extern void _jl_mpz_set_ui(mpz_t* rop, unsigned long int op) {
  mpz_set_ui(*rop, op);
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

extern void _jl_mpz_neg(mpz_t* rop, mpz_t* op1) {
  mpz_neg(*rop, *op1);
}

extern void _jl_mpz_abs(mpz_t* rop, mpz_t* op1) {
  mpz_abs(*rop, *op1);
}

extern char*  _jl_mpz_printf(mpz_t* rop) {
  char** pp;
  gmp_asprintf(pp, "%Zd", *rop);
  return *pp;
}

//Quick and dirty test of the gmp wrapper code
int main( int argc, const char* argv[] )
{
  void* rop = _jl_mpz_init();

  void* op1 = _jl_mpz_init();
  _jl_mpz_set_string(op1, "123456789123456789123456789123456789");

  void* op2 = _jl_mpz_init();
  _jl_mpz_set_string(op2, "12345");

  _jl_mpz_add(rop, op1, op2);

  printf("The sum is %s\n", _jl_mpz_printf(rop));
}



