@unix_only if WORD_SIZE == 64
# TODO: complex return only on 64-bit for now
for f in (:erf, :erfc, :erfcx, :erfi, :Dawson)
    fname = (f === :Dawson) ? :dawson : f
    @eval begin
        ($fname)(z::Complex128) = complex128(ccall(($(string("Faddeeva_",f)),openspecfun), Complex{Float64}, (Complex{Float64}, Float64), z, zero(Float64)))
        ($fname)(z::Complex64) = complex64(ccall(($(string("Faddeeva_",f)),openspecfun), Complex{Float64}, (Complex{Float64}, Float64), complex128(z), float64(eps(Float32))))
        ($fname)(z::Complex) = ($fname)(complex128(z))
    end
end
end
for f in (:erfcx, :erfi, :Dawson)
    fname = (f === :Dawson) ? :dawson : f
    @eval begin
        ($fname)(x::Float64) = ccall(($(string("Faddeeva_",f,"_re")),openspecfun), Float64, (Float64,), x)
        ($fname)(x::Float32) = float32(ccall(($(string("Faddeeva_",f,"_re")),openspecfun), Float64, (Float64,), float64(x)))
        ($fname)(x::Integer) = ($fname)(float(x))
        @vectorize_1arg Number $fname
    end
end

# Compute the inverse of the error function: erf(erfinv(x)) == x,
# using the rational approximants tabulated in:
#     J. M. Blair, C. A. Edwards, and J. H. Johnson, "Rational Chebyshev
#     approximations for the inverse of the error function," Math. Comp. 30,
#     pp. 827--830 (1976).
#         http://dx.doi.org/10.1090/S0025-5718-1976-0421040-7
#         http://www.jstor.org/stable/2005402
function erfinv(x::Float64)
    a = abs(x)
    if a >= 1.0
        if x == 1.0
            return Inf
        elseif x == -1.0
            return -Inf
        end
        throw(DomainError())
    elseif a <= 0.75 # Table 17 in Blair et al.
        t = x*x - 0.5625
        return x * @horner(t, 0.16030_49558_44066_229311e2,
                             -0.90784_95926_29603_26650e2,
                              0.18644_91486_16209_87391e3,
                             -0.16900_14273_46423_82420e3,
                              0.65454_66284_79448_7048e2,
                             -0.86421_30115_87247_794e1,
                              0.17605_87821_39059_0) /
                   @horner(t, 0.14780_64707_15138_316110e2,
                             -0.91374_16702_42603_13936e2,
                              0.21015_79048_62053_17714e3,
                             -0.22210_25412_18551_32366e3,
                              0.10760_45391_60551_23830e3,
                             -0.20601_07303_28265_443e2,
                              0.1e1)
    elseif a <= 0.9375 # Table 37 in Blair et al.
        t = x*x - 0.87890625
        return x * @horner(t, -0.15238_92634_40726_128e-1,
                               0.34445_56924_13612_5216,
                              -0.29344_39867_25424_78687e1,
                               0.11763_50570_52178_27302e2,
                              -0.22655_29282_31011_04193e2,
                               0.19121_33439_65803_30163e2,
                              -0.54789_27619_59831_8769e1,
                               0.23751_66890_24448) /
                   @horner(t, -0.10846_51696_02059_954e-1,
                               0.26106_28885_84307_8511,
                              -0.24068_31810_43937_57995e1,
                               0.10695_12997_33870_14469e2,
                              -0.23716_71552_15965_81025e2,
                               0.24640_15894_39172_84883e2,
                              -0.10014_37634_97830_70835e2,
                               0.1e1)
    else # Table 57 in Blair et al.
        t = 1.0 / sqrt(-log(1.0 - a))
        return @horner(t, 0.10501_31152_37334_38116e-3,
                          0.10532_61131_42333_38164_25e-1,
                          0.26987_80273_62432_83544_516,
                          0.23268_69578_89196_90806_414e1,
                          0.71678_54794_91079_96810_001e1,
                          0.85475_61182_21678_27825_185e1,
                          0.68738_08807_35438_39802_913e1,
                          0.36270_02483_09587_08930_02e1,
                          0.88606_27392_96515_46814_9) /
              (copysign(t, x) *
               @horner(t, 0.10501_26668_70303_37690e-3,
                          0.10532_86230_09333_27531_11e-1,
                          0.27019_86237_37515_54845_553,
                          0.23501_43639_79702_53259_123e1,
                          0.76078_02878_58012_77064_351e1,
                          0.11181_58610_40569_07827_3451e2,
                          0.11948_78791_84353_96667_8438e2,
                          0.81922_40974_72699_07893_913e1,
                          0.40993_87907_63680_15361_45e1,
                          0.1e1))
    end
end

function erfinv(x::Float32)
    a = abs(x)
    if a >= 1.0f0
        if x == 1.0f0
            return Inf32
        elseif x == -1.0f0
            return -Inf32
        end
        throw(DomainError())
    elseif a <= 0.75f0 # Table 10 in Blair et al.
        t = x*x - 0.5625f0
        return x * @horner(t, -0.13095_99674_22f2,
                               0.26785_22576_0f2,
                              -0.92890_57365f1) /
                   @horner(t, -0.12074_94262_97f2,
                               0.30960_61452_9f2,
                              -0.17149_97799_1f2,
                               0.1f1)
    elseif a <= 0.9375f0 # Table 29 in Blair et al.
        t = x*x - 0.87890625f0
        return x * @horner(t, -0.12402_56522_1f0,
                               0.10688_05957_4f1,
                              -0.19594_55607_8f1,
                               0.42305_81357f0) /
                   @horner(t, -0.88276_97997f-1,
                               0.89007_43359f0,
                              -0.21757_03119_6f1,
                               0.1f1)
    else # Table 50 in Blair et al.
        t = 1.0f0 / sqrt(-log(1.0f0 - a))
        return @horner(t, 0.15504_70003_116f0,
                          0.13827_19649_631f1,
                          0.69096_93488_87f0,
                         -0.11280_81391_617f1,
                          0.68054_42468_25f0,
                         -0.16444_15679_1f0) /
              (copysign(t, x) *
               @horner(t, 0.15502_48498_22f0,
                          0.13852_28141_995f1,
                          0.1f1))
    end
end

erfinv(x::Integer) = erfinv(float(x))
@vectorize_1arg Real erfinv

# Inverse complementary error function: use Blair tables for y = 1-x,
# exploiting the greater accuracy of y (vs. x) when y is small.
function erfcinv(y::Float64)
    if y > 0.0625
        return erfinv(1.0 - y)
    elseif y <= 0.0
        if y == 0.0
            return Inf
        end
        throw(DomainError())
    elseif y >= 1e-100 # Table 57 in Blair et al.
        t = 1.0 / sqrt(-log(y))
        return @horner(t, 0.10501_31152_37334_38116e-3,
                          0.10532_61131_42333_38164_25e-1,
                          0.26987_80273_62432_83544_516,
                          0.23268_69578_89196_90806_414e1,
                          0.71678_54794_91079_96810_001e1,
                          0.85475_61182_21678_27825_185e1,
                          0.68738_08807_35438_39802_913e1,
                          0.36270_02483_09587_08930_02e1,
                          0.88606_27392_96515_46814_9) /
              (t *
               @horner(t, 0.10501_26668_70303_37690e-3,
                          0.10532_86230_09333_27531_11e-1,
                          0.27019_86237_37515_54845_553,
                          0.23501_43639_79702_53259_123e1,
                          0.76078_02878_58012_77064_351e1,
                          0.11181_58610_40569_07827_3451e2,
                          0.11948_78791_84353_96667_8438e2,
                          0.81922_40974_72699_07893_913e1,
                          0.40993_87907_63680_15361_45e1,
                          0.1e1))
    else # Table 80 in Blair et al.
        t = 1.0 / sqrt(-log(y))
        return @horner(t, 0.34654_29858_80863_50177e-9,
                          0.25084_67920_24075_70520_55e-6,
                          0.47378_13196_37286_02986_534e-4,
                          0.31312_60375_97786_96408_3388e-2,
                          0.77948_76454_41435_36994_854e-1,
                          0.70045_68123_35816_43868_271e0,
                          0.18710_42034_21679_31668_683e1,
                          0.71452_54774_31351_45428_3e0) /
          (t * @horner(t, 0.34654_29567_31595_11156e-9,
                          0.25084_69079_75880_27114_87e-6,
                          0.47379_53129_59749_13536_339e-4,
                          0.31320_63536_46177_68848_0813e-2,
                          0.78073_48906_27648_97214_733e-1,
                          0.70715_04479_95337_58619_993e0,
                          0.19998_51543_49112_15105_214e1,
                          0.15072_90269_27316_80008_56e1,
                          0.1e1))
    end
end

function erfcinv(y::Float32)
    if y > 0.0625f0
        return erfinv(1.0f0 - y)
    elseif y <= 0.0f0
        if y == 0.0f0
            return Inf32
        end
        throw(DomainError())
    else # Table 50 in Blair et al.
        t = 1.0f0 / sqrt(-log(y))
        return @horner(t, 0.15504_70003_116f0,
                          0.13827_19649_631f1,
                          0.69096_93488_87f0,
                         -0.11280_81391_617f1,
                          0.68054_42468_25f0,
                         -0.16444_15679_1f0) /
        (t *
         @horner(t, 0.15502_48498_22f0,
                    0.13852_28141_995f1,
                    0.1f1))
    end
end

erfcinv(x::Integer) = erfcinv(float(x))
@vectorize_1arg Real erfcinv

