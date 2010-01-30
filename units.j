UNITS = buffer("meters","seconds")

struct Units
    powers::Buffer[Double]
end

function make_unit(index::Int32)
    powers = Buffer[Double].new(length(UNITS))
    powers[index] = 1
    Units.new(powers)
end

function print(units::Units)
    first = true
    for i = 1:length(UNITS)
        if units.powers[i] != 0
            if first
                first = false
            else
                print("*")
            end
            print(UNITS[i])
            if units.powers[i] != 1
                print("^")
                print(units.powers[i])
            end
        end
    end
end

meters = make_unit(1)
seconds = make_unit(2)

==(u1::Units, u2::Units) = u1.powers == u2.powers

function (+)(u1::Units, u2::Units)
    if u1 != u2
        error("Incompatible units")
    end
    return u1
end

function (-)(u1::Units, u2::Units)
    if u1 != u2
        error("Incompatible units")
    end
    return u1
end

function (-)(u1::Units, u2::Units)
    if u1 != u2
        error("Incompatible units")
    end
    return u1
end

*(u1::Units, u2::Units) = Units.new(apply_op((+), u1.powers, u2.powers))
/(u1::Units, u2::Units) = Units.new(apply_op((-), u1.powers, u2.powers))

struct ValueWithUnits[T]
    value::T
    units::Units
end

value_with_units[T](value::T, units::Units) = ValueWithUnits[T].new(value, units)

*(value::Any, units::Units) = value_with_units(value, units)
*(x::ValueWithUnits, units::Units) = value_with_units(x.value, x.units*units)
/(x::ValueWithUnits, units::Units) = value_with_units(x.value, x.units/units)

function +(x::ValueWithUnits, y::ValueWithUnits)
    if x.units != y.units
        error("Incompatible units")
    end
    value_with_units(x.value*y.value, x.units)
end
