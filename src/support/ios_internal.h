#ifndef IOS_PRIVATE_H
#define IOS_PRIVATE_H

#include "uv.h"
#include "ios.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef enum { bm_none=UV_HANDLE_TYPE_MAX+1, bm_line, bm_block, bm_mem } bufmode_t;

struct ios_t {
    // the state only indicates where the underlying file position is relative
    // to the buffer. reading: at the end. writing: at the beginning.
    // in general, you can do any operation in any state.
    char *buf;        // start of buffer

    int errcode;

#ifdef _P64
    int _pad_bm;      // put bm at same offset as type field of uv_stream_s
#endif
    bufmode_t bm;     //
    bufstate_t state;

    off_t maxsize;    // space allocated to buffer
    off_t size;       // length of valid data in buf, >=ndirty
    off_t bpos;       // current position in buffer
    off_t ndirty;     // # bytes at &buf[0] that need to be written

    off_t fpos;       // cached file pos
    size_t lineno;    // current line number

    // pointer-size integer to support platforms where it might have
    // to be a pointer
    long fd;

    unsigned char readable:1;
    unsigned char writable:1;
    unsigned char ownbuf:1;
    unsigned char ownfd:1;
    unsigned char _eof:1;

    // this means you can read, seek back, then read the same data
    // again any number of times. usually only true for files and strings.
    unsigned char rereadable:1;

    // this enables "stenciled writes". you can alternately write and
    // seek without flushing in between. this performs read-before-write
    // to populate the buffer, so "rereadable" capability is required.
    // this is off by default.
    //unsigned char stenciled:1;

    // request durable writes (fsync)
    // unsigned char durable:1;

    int64_t userdata;
    char local[IOS_INLSIZE];
};

DLLEXPORT int ios_bufmode(ios_t *s, bufmode_t mode);

#ifdef __cplusplus
}
#endif

#endif
