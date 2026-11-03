module TwoImagesA
# One of two trimmed libraries loaded into a single process; `TwoImagesB` is the
# other. Each is built as its own system image against the same libjulia.

Base.@ccallable function twoimages_a_answer()::Int32
    return Int32(1)
end

end
