// This file is a part of Julia. License is MIT: https://julialang.org/license
//

#ifdef __cplusplus
extern "C" {
#endif

#include "julia.h"
#include "julia_internal.h"

#ifdef ENABLE_TRACING

#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include "threading.h"

typedef struct _jl_tracepkt_t {
    int16_t tid;
    uint32_t event;
    uint64_t timestamp;

} jl_tracepkt_t;

static struct sockaddr_in send_addr;
static int send_socket;
static jl_mutex_t send_socket_lock;

static jl_tracepkt_t **tracepkts;
static int **num_tracepkts;

void jl_init_tracing(void)
{
    if (tracepkts != NULL) {
        jl_error("jl_init_tracing() already called!()");
        return;
    }

    // use libuv to get the sockaddr_in
    uv_ip4_addr(TRACE_ADDR, TRACE_PORT, &send_addr);

    // open the socket
    if ((send_socket = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        jl_error("jl_init_tracing() cannot create socket");
        return;
    }

    // synchronize use of the socket across threads
    JL_MUTEX_INIT(&send_socket_lock);

    tracepkts = (jl_tracepkt_t **)calloc(jl_n_threads, sizeof (jl_tracepkt_t *));
    num_tracepkts = (int **)calloc(jl_n_threads, sizeof (int *));
    for (int i = 0; i < jl_n_threads; i++) {
        tracepkts[i] = (jl_tracepkt_t *)calloc(TRACEPKTS_BUFSIZE, sizeof (jl_tracepkt_t));
        num_tracepkts[i] = (int *)calloc(1, sizeof (int));
    }
}

// racy when called from `jl_flush_all_traces()` despite the atomics; drop them?
static void flush_thread_traces(int16_t tid)
{
    int num = jl_atomic_load(num_tracepkts[tid]);
    if (num == 0)
        return;
    size_t len = num * sizeof (jl_tracepkt_t);
    JL_LOCK(&send_socket_lock);
    ssize_t sent = sendto(
        send_socket,
        tracepkts[tid],
        len,
        0,
        (const struct sockaddr *)&send_addr,
        sizeof (struct sockaddr_in));
    JL_UNLOCK(&send_socket_lock);
    if (sent != len)
        jl_printf(
            JL_STDERR,
            "WARN: flush_thread_traces(%d) sent %ld of %ld bytes\n",
            tid,
            sent,
            len);
    if (!jl_atomic_bool_compare_exchange(num_tracepkts[tid], num, 0)) {
        int prev = jl_atomic_exchange(num_tracepkts[tid], 0);
        jl_printf(
            JL_STDERR,
            "WARN: flush_thread_traces(%d) race with jl_trace(); %d packets lost\n",
            tid,
            prev - num);
    }
}

void jl_flush_traces(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    flush_thread_traces(ptls->tid);
}

void jl_flush_all_traces(void)
{
    for (int i = 0; i < jl_n_threads; i++)
        flush_thread_traces(i);
}

void jl_trace(uint32_t ev)
{
    jl_ptls_t ptls = jl_get_ptls_states();

    int num = jl_atomic_load(num_tracepkts[ptls->tid]);
    assert (num != TRACEPKTS_BUFSIZE);
    jl_tracepkt_t *pkt = &tracepkts[ptls->tid][num];

    pkt->tid = ptls->tid;
    pkt->event = ev;
    pkt->timestamp = jl_hrtime();

    jl_atomic_store(num_tracepkts[ptls->tid], ++num);
    if (num == TRACEPKTS_BUFSIZE)
        jl_flush_traces();
}

#else // !ENABLE_TRACING

void jl_trace(uint32_t ev)
{
}

void jl_flush_traces(void)
{
}

void jl_flush_all_traces(void)
{
}

#endif // ENABLE_TRACING

#ifdef __cplusplus
}
#endif
