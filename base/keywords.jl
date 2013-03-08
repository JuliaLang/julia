# type to specify that a call is suffixed with n symbol-value keyword pairs
immutable NKeywords
    n::Int
end

# singleton used to select a version of a method with keywords pre-sorted
type SortedKeywords
end
