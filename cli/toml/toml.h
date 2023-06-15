/*
  MIT License

  Copyright (c) CK Tan
  https://github.com/cktan/tomlc99

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
*/
#ifndef TOML_H
#define TOML_H

#ifdef _MSC_VER
#pragma warning(disable: 4996)
#endif

#include <stdint.h>
#include <stdio.h>

#ifdef __cplusplus
#define TOML_EXTERN extern "C"
#else
#define TOML_EXTERN extern
#endif

typedef struct toml_timestamp_t toml_timestamp_t;
typedef struct toml_table_t toml_table_t;
typedef struct toml_array_t toml_array_t;
typedef struct toml_datum_t toml_datum_t;

/* Parse a file. Return a table on success, or 0 otherwise.
 * Caller must toml_free(the-return-value) after use.
 */
TOML_EXTERN toml_table_t *toml_parse_file(FILE *fp, char *errbuf, int errbufsz);

/* Parse a string containing the full config.
 * Return a table on success, or 0 otherwise.
 * Caller must toml_free(the-return-value) after use.
 */
TOML_EXTERN toml_table_t *toml_parse(char *conf, /* NUL terminated, please. */
                                     char *errbuf, int errbufsz);

/* Free the table returned by toml_parse() or toml_parse_file(). Once
 * this function is called, any handles accessed through this tab
 * directly or indirectly are no longer valid.
 */
TOML_EXTERN void toml_free(toml_table_t *tab);

/* Timestamp types. The year, month, day, hour, minute, second, z
 * fields may be NULL if they are not relevant. e.g. In a DATE
 * type, the hour, minute, second and z fields will be NULLs.
 */
struct toml_timestamp_t {
  struct { /* internal. do not use. */
    int year, month, day;
    int hour, minute, second, millisec;
    char z[10];
  } __buffer;
  int *year, *month, *day;
  int *hour, *minute, *second, *millisec;
  char *z;
};

/*-----------------------------------------------------------------
 *  Enhanced access methods
 */
struct toml_datum_t {
  int ok;
  union {
    toml_timestamp_t *ts; /* ts must be freed after use */
    char *s;              /* string value. s must be freed after use */
    int b;                /* bool value */
    int64_t i;            /* int value */
    double d;             /* double value */
  } u;
};

/* on arrays: */
/* ... retrieve size of array. */
TOML_EXTERN int toml_array_nelem(const toml_array_t *arr);
/* ... retrieve values using index. */
TOML_EXTERN toml_datum_t toml_string_at(const toml_array_t *arr, int idx);
TOML_EXTERN toml_datum_t toml_bool_at(const toml_array_t *arr, int idx);
TOML_EXTERN toml_datum_t toml_int_at(const toml_array_t *arr, int idx);
TOML_EXTERN toml_datum_t toml_double_at(const toml_array_t *arr, int idx);
TOML_EXTERN toml_datum_t toml_timestamp_at(const toml_array_t *arr, int idx);
/* ... retrieve array or table using index. */
TOML_EXTERN toml_array_t *toml_array_at(const toml_array_t *arr, int idx);
TOML_EXTERN toml_table_t *toml_table_at(const toml_array_t *arr, int idx);

/* on tables: */
/* ... retrieve the key in table at keyidx. Return 0 if out of range. */
TOML_EXTERN const char *toml_key_in(const toml_table_t *tab, int keyidx);
/* ... returns 1 if key exists in tab, 0 otherwise */
TOML_EXTERN int toml_key_exists(const toml_table_t *tab, const char *key);
/* ... retrieve values using key. */
TOML_EXTERN toml_datum_t toml_string_in(const toml_table_t *arr,
                                        const char *key);
TOML_EXTERN toml_datum_t toml_bool_in(const toml_table_t *arr, const char *key);
TOML_EXTERN toml_datum_t toml_int_in(const toml_table_t *arr, const char *key);
TOML_EXTERN toml_datum_t toml_double_in(const toml_table_t *arr,
                                        const char *key);
TOML_EXTERN toml_datum_t toml_timestamp_in(const toml_table_t *arr,
                                           const char *key);
/* .. retrieve array or table using key. */
TOML_EXTERN toml_array_t *toml_array_in(const toml_table_t *tab,
                                        const char *key);
TOML_EXTERN toml_table_t *toml_table_in(const toml_table_t *tab,
                                        const char *key);

/*-----------------------------------------------------------------
 * lesser used
 */
/* Return the array kind: 't'able, 'a'rray, 'v'alue, 'm'ixed */
TOML_EXTERN char toml_array_kind(const toml_array_t *arr);

/* For array kind 'v'alue, return the type of values
   i:int, d:double, b:bool, s:string, t:time, D:date, T:timestamp, 'm'ixed
   0 if unknown
*/
TOML_EXTERN char toml_array_type(const toml_array_t *arr);

/* Return the key of an array */
TOML_EXTERN const char *toml_array_key(const toml_array_t *arr);

/* Return the number of key-values in a table */
TOML_EXTERN int toml_table_nkval(const toml_table_t *tab);

/* Return the number of arrays in a table */
TOML_EXTERN int toml_table_narr(const toml_table_t *tab);

/* Return the number of sub-tables in a table */
TOML_EXTERN int toml_table_ntab(const toml_table_t *tab);

/* Return the key of a table*/
TOML_EXTERN const char *toml_table_key(const toml_table_t *tab);

/*--------------------------------------------------------------
 * misc
 */
TOML_EXTERN int toml_utf8_to_ucs(const char *orig, int len, int64_t *ret);
TOML_EXTERN int toml_ucs_to_utf8(int64_t code, char buf[6]);
TOML_EXTERN void toml_set_memutil(void *(*xxmalloc)(size_t),
                                  void (*xxfree)(void *));

/*--------------------------------------------------------------
 *  deprecated
 */
/* A raw value, must be processed by toml_rto* before using. */
typedef const char *toml_raw_t;
TOML_EXTERN toml_raw_t toml_raw_in(const toml_table_t *tab, const char *key);
TOML_EXTERN toml_raw_t toml_raw_at(const toml_array_t *arr, int idx);
TOML_EXTERN int toml_rtos(toml_raw_t s, char **ret);
TOML_EXTERN int toml_rtob(toml_raw_t s, int *ret);
TOML_EXTERN int toml_rtoi(toml_raw_t s, int64_t *ret);
TOML_EXTERN int toml_rtod(toml_raw_t s, double *ret);
TOML_EXTERN int toml_rtod_ex(toml_raw_t s, double *ret, char *buf, int buflen);
TOML_EXTERN int toml_rtots(toml_raw_t s, toml_timestamp_t *ret);

#endif /* TOML_H */
