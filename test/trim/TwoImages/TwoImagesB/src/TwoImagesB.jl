module TwoImagesB
# One of two trimmed libraries loaded into a single process; `TwoImagesA` is the
# other. Each is built as its own system image against the same libjulia.

Base.@ccallable function twoimages_b_answer()::Int32
    return Int32(2)
end

end
