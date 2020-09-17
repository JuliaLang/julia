using Artifacts

function get_artifact_compiletime()
    return artifact"arty"
end

function get_artifact_runtime()
    name = "arty"
    return @artifact_str(name)
end