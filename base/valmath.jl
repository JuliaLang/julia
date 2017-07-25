@generated (+)(::Type{Val{X}}, ::Type{Val{Y}}) where {X, Y} = :(Val{$(X + Y)})
@generated (-)(::Type{Val{X}}, ::Type{Val{Y}}) where {X, Y} = :(Val{$(X - Y)})
@generated (*)(::Type{Val{X}}, ::Type{Val{Y}}) where {X, Y} = :(Val{$(X * Y)})
