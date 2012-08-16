
if CURRENT_OS == :OSX
    const DL_EXTENSIONS = ["", ".dylib"]
elseif CURRENT_OS == :Windows
    const DL_EXTENSIONS = ["", ".dll"]
else
    const DL_EXTENSIONS = [".so", ""]
end

const DL_LOAD_PATH = ["$JULIA_HOME/../lib", ]

function dlopen(s::String)
    handle = ccall(:malloc, Ptr{Void}, (Uint,), 
        ccall(:jl_sizeof_uv_lib_t, Uint, ()))
    statbuf = Array(Uint8, ccall(:jl_sizeof_stat, Int, ()))
    @windows_only begin
        if s[2] == ':'
            errno = jl_uv_dlopen(s, handle)
            if errno == 0
                return handle
            end
        end
    end
    @unix_only begin
        if s[1] == '/'
            errno = jl_uv_dlopen(s, handle)
            if errno == 0
                ## Can't register finalizers on BitsKinds
                #finalizer(handle, dlclose)
                return handle
            end
        end
    end
    for ext in DL_EXTENSIONS
        for dir in DL_LOAD_PATH
            path = "$(dir)/$(s)$(ext)"
            errno = jl_uv_dlopen(path, handle)
            if errno == 0
                return handle
            else
                # isfile() won't be defined yet, so we make our own
                err = ccall(:jl_stat, Int32, (Ptr{Uint8},Ptr{Uint8}), path, statbuf)
                if (err == 0 && ccall(:jl_stat_mode, Uint, (Ptr{Uint8},), statbuf) & 0xf000 == 0x8000)
                    msg = "could not load module $s: " * bytestring(ccall(:uv_dlerror, Ptr{Uint8}, (Ptr{Void},), handle))
                    dlclose(handle)
                    error(msg)
                end
            end
        end
        path = "$s$ext"
        errno = jl_uv_dlopen(path, handle)
        if errno == 0
            ## Can't register finalizers on BitsKinds
            #finalizer(handle, dlclose)
            return handle
        end
    end
    
    msg = "could not load module $s: " * bytestring(ccall(:uv_dlerror, Ptr{Uint8}, (Ptr{Void},), handle))
    dlclose(handle)
    error(msg)
end

