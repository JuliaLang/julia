.. currentmodule:: Base

.. _devdocs-promote-op:

Operator-sensitive promotion
============================

In certain cases, the :ref:`simple rules for promotion
<man-promotion-rules>` may not be sufficient. For example, consider a
type that can represent an object with physical units, here restricted
to a single unit like "meter"::

    immutable MeterUnits{T,P} <: Number
        val::T
    end
    MeterUnits{T}(val::T, pow::Int) = MeterUnits{T,pow}(val)

    m  = MeterUnits(1.0, 1)   # 1.0 meter, i.e. units of length
    m2 = MeterUnits(1.0, 2)   # 1.0 meter^2, i.e. units of area

Now let's define the operations ``+`` and ``*`` for these objects:
``m+m`` should have the type of ``m`` but ``m*m`` should have the type
of ``m2``.  When the result type depends on the operation, and not
just the input types, ``promote_rule`` will be inadequate.

Fortunately, it's possible to provide such definitions via ``promote_op``::

    Base.promote_op{R,S}(::Base.GenericNFunc{:+,2}, ::Type{MeterUnits{R,1}}, ::Type{MeterUnits{S,1}}) = MeterUnits{promote_type(R,S),1}
    Base.promote_op{R,S}(::Base.GenericNFunc{:*,2}, ::Type{MeterUnits{R,1}}, ::Type{MeterUnits{S,1}}) = MeterUnits{promote_type(R,S),2}
    Base.promote_op{R,S}(::Base.GenericNFunc{:.*,2}, ::Type{MeterUnits{R,1}}, ::Type{MeterUnits{S,1}}) = MeterUnits{promote_type(R,S),2}

The first one defines the promotion rule for ``+``, and the second one
for ``*``.  A ``GenericNFunc`` is a `functor <https://github.com/JuliaLang/julia/blob/master/base/functors.jl>`_;
the second parameter ``2`` means that it takes two arguments.
