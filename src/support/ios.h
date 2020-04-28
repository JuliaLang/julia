// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_IOS_H
#define JL_IOS_H

#include <stdarg.h>
#include "uv.h"
#include "analyzer_annotations.h"

#ifdef __cplusplus
extern "C" {
#endif

// this flag controls when data actually moves out to the underlying I/O
// channel. memory streams are a special case of this where the data
// never moves out.

//make it compatible with UV Handles
typedef enum { bm_none=UV_HANDLE_TYPE_MAX+1, bm_line, bm_block, bm_mem } bufmode_t;
typedef enum { bst_none, bst_rd, bst_wr } bufstate_t;

#define IOS_INLSIZE 54
#define IOS_BUFSIZE 32768

#ifdef _P64
#define ON_P64(x) x
#else
#define ON_P64(x)
#endif

// We allow ios_t as a cvalue in flisp, which only guarantees pointer
// alignment. Make sure the compiler knows.
JL_ATTRIBUTE_ALIGN_PTRSIZE(typedef struct {
    // the state only indicates where the underlying file position is relative
    // to the buffer. reading: at the end. writing: at the beginning.
    // in general, you can do any operation in any state.
    char *buf;        // start of buffer

    int errcode;

    ON_P64(int _pad_bm;)      // put bm at same offset as type field of uv_stream_s
    bufmode_t bm;     //
    bufstate_t state;

    int64_t maxsize;    // space allocated to buffer
    int64_t size;       // length of valid data in buf, >=ndirty
    int64_t bpos;       // current position in buffer
    int64_t ndirty;     // # bytes at &buf[0] that need to be written

    int64_t fpos;       // cached file pos
    size_t lineno;    // current line number
    size_t u_colno;     // current column number (in Unicode charwidths)

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
} ios_t);

#undef ON_P64

extern void (*ios_set_io_wait_func)(int);
/* low-level interface functions */
JL_DLLEXPORT size_t ios_read(ios_t *s, char *dest, size_t n) JL_NOTSAFEPOINT;
JL_DLLEXPORT size_t ios_readall(ios_t *s, char *dest, size_t n) JL_NOTSAFEPOINT;
JL_DLLEXPORT size_t ios_write(ios_t *s, const char *data, size_t n) JL_NOTSAFEPOINT;
JL_DLLEXPORT int64_t ios_seek(ios_t *s, int64_t pos) JL_NOTSAFEPOINT; // absolute seek
JL_DLLEXPORT int64_t ios_seek_end(ios_t *s) JL_NOTSAFEPOINT;
JL_DLLEXPORT int64_t ios_skip(ios_t *s, int64_t offs);  // relative seek
JL_DLLEXPORT int64_t ios_pos(ios_t *s) JL_NOTSAFEPOINT;  // get current position
JL_DLLEXPORT int ios_trunc(ios_t *s, size_t size) JL_NOTSAFEPOINT;
JL_DLLEXPORT int ios_eof(ios_t *s);
JL_DLLEXPORT int ios_eof_blocking(ios_t *s);
JL_DLLEXPORT int ios_flush(ios_t *s);
JL_DLLEXPORT int ios_close(ios_t *s) JL_NOTSAFEPOINT;
JL_DLLEXPORT int ios_isopen(ios_t *s);
JL_DLLEXPORT char *ios_take_buffer(ios_t *s, size_t *psize);  // nul terminate and release buffer to caller
// set buffer space to use
JL_DLLEXPORT int ios_setbuf(ios_t *s, char *buf, size_t size, int own) JL_NOTSAFEPOINT;
JL_DLLEXPORT int ios_bufmode(ios_t *s, bufmode_t mode) JL_NOTSAFEPOINT;
JL_DLLEXPORT int ios_get_readable(ios_t *s);
JL_DLLEXPORT int ios_get_writable(ios_t *s);
JL_DLLEXPORT void ios_set_readonly(ios_t *s);
JL_DLLEXPORT size_t ios_copy(ios_t *to, ios_t *from, size_t nbytes);
JL_DLLEXPORT size_t ios_copyall(ios_t *to, ios_t *from);
JL_DLLEXPORT size_t ios_copyuntil(ios_t *to, ios_t *from, char delim) JL_NOTSAFEPOINT;
JL_DLLEXPORT size_t ios_nchomp(ios_t *from, size_t ntowrite);
// ensure at least n bytes are buffered if possible. returns # available.
JL_DLLEXPORT size_t ios_readprep(ios_t *from, size_t n);

/* stream creation */
JL_DLLEXPORT
ios_t *ios_file(ios_t *s, const char *fname, int rd, int wr, int create, int trunc) JL_NOTSAFEPOINT;
JL_DLLEXPORT ios_t *ios_mkstemp(ios_t *f, char *fname);
JL_DLLEXPORT ios_t *ios_mem(ios_t *s, size_t initsize) JL_NOTSAFEPOINT;
ios_t *ios_str(ios_t *s, char *str);
ios_t *ios_static_buffer(ios_t *s, char *buf, size_t sz);
JL_DLLEXPORT ios_t *ios_fd(ios_t *s, long fd, int isfile, int own);
// todo: ios_socket
extern JL_DLLEXPORT ios_t *ios_stdin;
extern JL_DLLEXPORT ios_t *ios_stdout;
extern JL_DLLEXPORT ios_t *ios_stderr;
void ios_init_stdstreams(void);

/* high-level functions - output */
JL_DLLEXPORT int ios_pututf8(ios_t *s, uint32_t wc);
JL_DLLEXPORT int ios_printf(ios_t *s, const char *format, ...);
JL_DLLEXPORT int ios_vprintf(ios_t *s, const char *format, va_list args);

/* high-level stream functions - input */
JL_DLLEXPORT int ios_getutf8(ios_t *s, uint32_t *pwc);
JL_DLLEXPORT int ios_peekutf8(ios_t *s, uint32_t *pwc);
JL_DLLEXPORT char *ios_readline(ios_t *s) JL_NOTSAFEPOINT;

// discard data buffered for reading
JL_DLLEXPORT void ios_purge(ios_t *s);

/* stdio-style functions */
#define IOS_EOF (-1)
JL_DLLEXPORT int ios_putc(int c, ios_t *s);
//wint_t ios_putwc(ios_t *s, wchar_t wc);
JL_DLLEXPORT int ios_getc(ios_t *s);
JL_DLLEXPORT int ios_peekc(ios_t *s);
//wint_t ios_getwc(ios_t *s);
int ios_ungetc(int c, ios_t *s);
//wint_t ios_ungetwc(ios_t *s, wint_t wc);
#define ios_puts(str, s) ios_write(s, str, strlen(str))

/*
  With memory streams, mixed reads and writes are equivalent to performing
  sequences of *p++, as either an lvalue or rvalue. File streams behave
  similarly, but other streams might not support this. Using unbuffered
  mode makes this more predictable.

  Note on "unget" functions:
  There are two kinds of functions here: those that operate on sized
  blocks of bytes and those that operate on logical units like "character"
  or "integer". The "unget" functions only work on logical units. There
  is no "unget n bytes". You can only do an unget after a matching get.
  However, data pushed back by an unget is available to all read operations.
  The reason for this is that unget is defined in terms of its effect on
  the underlying buffer (namely, it rebuffers data as if it had been
  buffered but not read yet). IOS reserves the right to perform large block
  operations directly, bypassing the buffer. In such a case data was
  never buffered, so "rebuffering" has no meaning (i.e. there is no
  correspondence between the buffer and the physical stream).

  Single-bit I/O is able to write partial bytes ONLY IF the stream supports
  seeking. Also, line buffering is not well-defined in the context of
  single-bit I/O, so it might not do what you expect.

  implementation notes:
  in order to know where we are in a file, we must ensure the buffer
  is only populated from the underlying stream starting with p==buf.

  to switch from writing to reading: flush, set p=buf, cnt=0
  to switch from reading to writing: seek backwards cnt bytes, p=buf, cnt=0

  when writing: buf starts at curr. physical stream pos, p - buf is how
  many bytes we've written logically. cnt==0

  dirty == (bitpos>0 && state==iost_wr), EXCEPT right after switching from
  reading to writing, where we might be in the middle of a byte without
  having changed it.

  to write a bit: if !dirty, read up to maxsize-(p-buf) into buffer, then
  seek back by the same amount (undo it). write onto those bits. now set
  the dirty bit. in this state, we can bit-read up to the end of the byte,
  then formally switch to the read state using flush.

  design points:
  - data-source independence, including memory streams
  - expose buffer to user, allow user-owned buffers
  - allow direct I/O, don't always go through buffer
  - buffer-internal seeking. makes seeking back 1-2 bytes very fast,
    and makes it possible for sockets where it otherwise wouldn't be
  - tries to allow switching between reading and writing
  - support 64-bit and large files
  - efficient, low-latency buffering
  - special support for utf8
  - type-aware functions with byte-order swapping service
  - position counter for meaningful data offsets with sockets

  theory of operation:

  the buffer is a view of part of a file/stream. you can seek, read, and
  write around in it as much as you like, as if it were just a string.

  we keep track of the part of the buffer that's invalid (written to).
  we remember whether the position of the underlying stream is aligned
  with the end of the buffer (reading mode) or the beginning (writing mode).

  based on this info, we might have to seek back before doing a flush.

  as optimizations, we do no writing if the buffer isn't "dirty", and we
  do no reading if the data will only be overwritten.
*/

#ifdef __cplusplus
}
#endif

#endif
