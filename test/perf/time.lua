
local ffi = require 'ffi'

ffi.cdef[[
	 struct timeval {
	    long tv_sec;
	    long tv_usec;
	 };

	 int gettimeofday(struct timeval * tp, void *tzp);
   ]]

local tv = ffi.new('struct timeval[1]')

local function gettime()
  ffi.C.gettimeofday(tv, nil)
  return tv[0].tv_sec, tv[0].tv_usec
end

return {get= gettime}
