type Quaternion[`T] < Scalar
    q0::T
    q1::T
    q2::T
    q3::T
end

function print(z::Quaternion)
    print(z.q0)
    i = z.q1
    if sign(i) == -1
        i = -i
        print(" - ")
    else
        print(" + ")
    end
    print(i)
    print("i")
    j = z.q2
    if sign(j) == -1
        j = -j
        print(" - ")
    else
        print(" + ")
    end
    print(j)
    print("j")
    k = z.q3
    if sign(k) == -1
        k = -k
        print(" - ")
    else
        print(" + ")
    end
    print(k)
    print("k")
end

function quaternion(q0::`T, q1::`T, q2::`T, q3::`T)
    return new(Quaternion[T], q0, q1, q2, q3)
end

function re(z::Quaternion)
    return z.q0
end

function im(z::Quaternion)
    return z.q1
end

function scalar(z::Quaternion)
    return z.q0
end

function vector(z::Quaternion)
    return vector(z.q1,z.q2,z.q3)
end

function (+)(z::Quaternion, w::Quaternion)
    return quaternion(z.q0 + w.q0, z.q1 + w.q1, z.q2 + w.q2, z.q3 + w.q3)
end

function (-)(z::Quaternion, w::Quaternion)
    return quaternion(z.q0 - w.q0, z.q1 - w.q1, z.q2 - w.q2, z.q3 - w.q3)
end

function -(z::Quaternion)
    return quaternion(-z.q0, -z.q1, -z.q2, -z.q3)
end

function *(z::Quaternion, w::Quaternion)
    return quaternion(z.q0*w.q0 - z.q1*w.q1 - z.q2*w.q2 - z.q3*w.q3,
                      z.q0*w.q1 + z.q1*w.q0 + z.q2*w.q3 - z.q3*w.q2,
                      z.q0*w.q2 - z.q1*w.q3 + z.q2*w.q0 + z.q3*w.q1,
                      z.q0*w.q3 + z.q1*w.q2 - z.q2*w.q1 + z.q3*w.q0)
end

function /(z::Quaternion, x::Real)
    return quaternion(z.q0/x, z.q1/x, z.q2/x, z.q3/x)
end

function conj(z::Quaternion)
    return quaternion(z.q0, -z.q1, -z.q2, -z.q3)
end

function norm(z::Quaternion)
    return z.q0*z.q0 + z.q1*z.q1 + z.q2*z.q2 + z.q3*z.q3
end

function inv(z::Quaternion)
    return conj(z)/norm(z)
end

function /(z::Quaternion, w::Quaternion)
    return z*inv(w)
end

conversion z::Complex-->Quaternion
    return quaternion(re(z),im(z),0,0)
end
