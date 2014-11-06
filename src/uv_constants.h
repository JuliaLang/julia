#include "uv.h"
#define XX(uc,lc) :UV_##uc,
#define YY(uc,lc) (:UV_##uc,UV__##uc),
const uv_handle_types = [UV_HANDLE_TYPE_MAP(XX) :UV_FILE]
const uv_req_types = [UV_REQ_TYPE_MAP(XX)]
const uv_err_vals = [UV_ERRNO_MAP(YY)]
let
    handles = [:UV_UNKNOWN_HANDLE, uv_handle_types, :UV_HANDLE_TYPE_MAX, :UV_RAW_FD, :UV_RAW_HANDLE]
    reqs = [:UV_UNKNOWN_REQ, uv_req_types, :UV_REQ_TYPE_PRIVATE,:UV_REQ_TYPE_MAX]
    for i=0:(length(handles)-1)
    @eval const $(handles[i+1]) = $i
    end
    for i=0:(length(reqs)-1)
    @eval const $(reqs[i+1]) = $i
    end
    for (v,val) in uv_err_vals
    @eval const $v = $val
    end
end
