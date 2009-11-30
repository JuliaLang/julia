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

quaternion(q0::`T, q1::`T, q2::`T, q3::`T) = new(Quaternion[T], q0, q1, q2, q3)

re(z::Quaternion) = z.q0
im(z::Quaternion) = z.q1

scalar(z::Quaternion) = z.q0
vector(z::Quaternion) = vector(z.q1,z.q2,z.q3)

conj(z::Quaternion) = quaternion(z.q0, -z.q1, -z.q2, -z.q3)
norm(z::Quaternion) = z.q0*z.q0 + z.q1*z.q1 + z.q2*z.q2 + z.q3*z.q3
inv(z::Quaternion) = conj(z)/norm(z)

(-)(z::Quaternion) = quaternion(-z.q0, -z.q1, -z.q2, -z.q3)

(/)(z::Quaternion, x::Real) = quaternion(z.q0/x, z.q1/x, z.q2/x, z.q3/x)

(+)(z::Quaternion, w::Quaternion) = quaternion(z.q0 + w.q0, z.q1 + w.q1,
                                               z.q2 + w.q2, z.q3 + w.q3)
(-)(z::Quaternion, w::Quaternion) = quaternion(z.q0 - w.q0, z.q1 - w.q1,
                                               z.q2 - w.q2, z.q3 - w.q3)
(*)(z::Quaternion, w::Quaternion) = quaternion(z.q0*w.q0 - z.q1*w.q1 - z.q2*w.q2 - z.q3*w.q3,
                                               z.q0*w.q1 + z.q1*w.q0 + z.q2*w.q3 - z.q3*w.q2,
                                               z.q0*w.q2 - z.q1*w.q3 + z.q2*w.q0 + z.q3*w.q1,
                                               z.q0*w.q3 + z.q1*w.q2 - z.q2*w.q1 + z.q3*w.q0)
(/)(z::Quaternion, w::Quaternion) = z*inv(w)

conversion z::Complex-->Quaternion
    return quaternion(re(z),im(z),0,0)
end
