# A `compile=min` module with an `__init__`. Under `--trim`, the entrypoint list is the
# only thing keeping `__init__` alive, but `jl_module_run_initializer` calls it at
# startup regardless of the module's compile setting.
module MinInitDep

Base.Experimental.@compiler_options compile=min

const initialized = Ref(false)

function __init__()
    initialized[] = true
end

end
