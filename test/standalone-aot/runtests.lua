
-- LD_LIBRARY_PATH=/home/tshort/jn/usr/lib:/home/tshort/jn/usr/lib/julia gdb --args luajit runtests.lua

local ffi = require("ffi")


lib = ffi.load("./libfsin.so")
ffi.cdef[[
double fsin(double s);
]]
print(lib.fsin(0.3))

lib = ffi.load("./libarraysum.so")
ffi.cdef[[
void init_lib();
long arraysum(long s);
]]
lib.init_lib()
print(lib.arraysum(100))

lib = ffi.load("./libfccall.so")
ffi.cdef[[
int fccall();
]]
print(lib.fccall())

lib = ffi.load("./libfcglobal.so")
ffi.cdef[[
int fcglobal();
]]
print(lib.fcglobal())     -- Not right; garbage number

lib = ffi.load("./libhi.so")
ffi.cdef[[
void hi();
]]
lib.hi()

lib = ffi.load("./libhello.so")
ffi.cdef[[
void init_lib();
void hello();
]]
lib.init_lib()
lib.hello()

-- lib = ffi.load("./libprintfloat.so")
-- ffi.cdef[[
-- void init_lib();
-- void printfloat();
-- ]]
-- lib.init_lib()
-- lib.printfloat()

