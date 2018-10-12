// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "uv.h"
#define XX(uc,lc) :UV_##uc,
#define YY(uc,lc) (:UV_##uc,UV__##uc),
const uv_handle_types = [UV_HANDLE_TYPE_MAP(XX) :UV_FILE]
const uv_req_types = [UV_REQ_TYPE_MAP(XX)]
const uv_err_vals = [UV_ERRNO_MAP(YY)]
let
    handles = [:UV_UNKNOWN_HANDLE, uv_handle_types..., :UV_HANDLE_TYPE_MAX]
    reqs = [:UV_UNKNOWN_REQ, uv_req_types..., :UV_REQ_TYPE_PRIVATE, :UV_REQ_TYPE_MAX]
    for i in 1:length(handles)
        @eval const $(handles[i]) = $(i - 1)
    end
    for i in 1:length(reqs)
        @eval const $(reqs[i]) = $(i - 1)
    end
    for (v, val) in uv_err_vals
        @eval const $v = $val
    end
end
