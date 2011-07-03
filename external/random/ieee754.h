#ifndef __IEEE754_H_
#define __IEEE754_H_

union ieee754_float {
    float f;

    struct {
#if BYTE_ORDER == BIG_ENDIAN
	unsigned int negative:1;
	unsigned int exponent:8;
	unsigned int mantissa:23;
#endif
#if BYTE_ORDER == LITTLE_ENDIAN
	unsigned int mantissa:23;
	unsigned int exponent:8;
	unsigned int negative:1;
#endif
    } ieee;
};

#define IEEE754_FLOAT_BIAS 0x7f

union ieee754_double {
    double d;

    struct {
#if BYTE_ORDER == BIG_ENDIAN
	unsigned int negative:1;
	unsigned int exponent:11;
	unsigned int mantissa0:20;
	unsigned int mantissa1:32;
#endif
#if BYTE_ORDER == LITTLE_ENDIAN
	unsigned int mantissa1:32;
	unsigned int mantissa0:20;
	unsigned int exponent:11;
	unsigned int negative:1;
#endif
    } ieee;
};

#define IEEE754_DOUBLE_BIAS 0x3ff

union ieee854_long_double {
    long double d;

    struct {
#if BYTE_ORDER == BIG_ENDIAN
	unsigned int negative:1;
	unsigned int exponent:15;
	unsigned int empty:16;
	unsigned int mantissa0:32;
	unsigned int mantissa1:32;
#endif
#if BYTE_ORDER == LITTLE_ENDIAN
	unsigned int mantissa1:32;
	unsigned int mantissa0:32;
	unsigned int exponent:15;
	unsigned int negative:1;
	unsigned int empty:16;
#endif
    } ieee;
};

#define IEEE854_LONG_DOUBLE_BIAS 0x3fff

#endif
