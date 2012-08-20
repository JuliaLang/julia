// -*- Mode: C++; c-file-style: "stroustrup"; indent-tabs-mode:nil; -*-
#include "testapi.h"

void func1 (void)
{
}

double func2 (double x, double y, double z)
{
    return x + y + z;
}


Multiplier::Multiplier ()
    : m_factor (1.0) 
{
}

Multiplier::Multiplier (double factor)
    : m_factor (factor)
{
}

Multiplier::~Multiplier ()
{
}

double Multiplier::GetFactor () const
{
    return m_factor;
}

void Multiplier::SetFactor (double f)
{
    m_factor = f;
}

void Multiplier::SetFactor ()
{
    m_factor = 1.0;
}

double
Multiplier::Multiply (double value) const
{
    return value*m_factor;
}


double
call_virtual_from_cpp (Multiplier const *obj, double value)
{
    return obj->Multiply (value);
}

