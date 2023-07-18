__precompile__(:mandatory)
module var"--check-bounds=yes"

@Base.recompile_invalidations begin
    Core.should_check_bounds(boundscheck::Bool) = true
end

end
