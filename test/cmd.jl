# issue #28188
@test `$(@__FILE__)` == let file = @__FILE__; `$file` end
