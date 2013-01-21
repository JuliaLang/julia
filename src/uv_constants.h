#include "uv.h"
#define XX(uc,lc) :UV_##uc,
const UV_UNKNOWN_HANDLE = 0
handles = [UV_HANDLE_TYPE_MAP(XX) :UV_FILE, :UV_HANDLE_TYPE_MAX, :UV_RAW_FD, :UV_RAW_HANDLE]
for i=1:(length(handles))
@eval const $(handles[i]) = $i
end
