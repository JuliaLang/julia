function mul_hi(x::T, y::T) where T <: Base.BitInteger
    xy = widemul(x, y)
    (xy >> (8 * sizeof(T))) % T
end
