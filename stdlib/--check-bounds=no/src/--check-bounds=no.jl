__precompile__(:mandatory)
module var"--check-bounds=no"

@Base.recompile_invalidations begin
    Core.should_check_bounds(boundscheck::Bool) = false
end

end
