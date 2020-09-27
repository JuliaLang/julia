# This file is a part of Julia. License is MIT: https://julialang.org/license
module Libdl
# Just re-export Base.Libc.Libdl:
export DL_LOAD_PATH, RTLD_DEEPBIND, RTLD_FIRST, RTLD_GLOBAL, RTLD_LAZY, RTLD_LOCAL,
    RTLD_NODELETE, RTLD_NOLOAD, RTLD_NOW, dlclose, dlopen, dlopen_e, dlsym, dlsym_e,
    dlpath, find_library, dlext, dllist

import Base.Libc.Libdl: DL_LOAD_PATH, RTLD_DEEPBIND, RTLD_FIRST, RTLD_GLOBAL, RTLD_LAZY, RTLD_LOCAL,
                        RTLD_NODELETE, RTLD_NOLOAD, RTLD_NOW, dlclose, dlopen, dlopen_e, dlsym, dlsym_e,
                        dlpath, find_library, dlext, dllist

end # module
