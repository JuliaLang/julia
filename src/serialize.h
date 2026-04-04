// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_SERIALIZE_H
#define JL_SERIALIZE_H

#ifdef __cplusplus
extern "C" {
#endif

#define write_uint8(s, n) ios_putc((n), (s))
#define read_uint8(s) ((uint8_t)ios_getc((s)))
#define write_int8(s, n) write_uint8((s), (n))
#define read_int8(s) read_uint8((s))

/* read and write in host byte order */

static inline void write_int32(ios_t *s, int32_t i) JL_NOTSAFEPOINT
{
    ios_write(s, (char*)&i, 4);
}

static inline int32_t read_int32(ios_t *s) JL_NOTSAFEPOINT
{
    int32_t x = 0;
    ios_read(s, (char*)&x, 4);
    return x;
}

static inline uint64_t read_uint64(ios_t *s) JL_NOTSAFEPOINT
{
    uint64_t x = 0;
    ios_read(s, (char*)&x, 8);
    return x;
}

static inline void write_uint64(ios_t *s, uint64_t i) JL_NOTSAFEPOINT
{
    ios_write(s, (char*)&i, 8);
}

static inline void write_uint16(ios_t *s, uint16_t i) JL_NOTSAFEPOINT
{
    ios_write(s, (char*)&i, 2);
}

static inline uint16_t read_uint16(ios_t *s) JL_NOTSAFEPOINT
{
    int16_t x = 0;
    ios_read(s, (char*)&x, 2);
    return x;
}

static inline void write_uint32(ios_t *s, uint32_t i) JL_NOTSAFEPOINT
{
    ios_write(s, (char*)&i, 4);
}

static inline uint32_t read_uint32(ios_t *s) JL_NOTSAFEPOINT
{
    uint32_t x = 0;
    ios_read(s, (char*)&x, 4);
    return x;
}

#ifdef _P64
#define write_uint(s, i) write_uint64(s, i)
#else
#define write_uint(s, i) write_uint32(s, i)
#endif

#ifdef _P64
#define read_uint(s) read_uint64(s)
#else
#define read_uint(s) read_uint32(s)
#endif

#ifdef __cplusplus
}
#endif

#endif /* JL_SERIALIZE_H */
