module LibdlExt

using Libdl, GotLibdlExt

function __init__()
    @info "loaded!"
end

end
