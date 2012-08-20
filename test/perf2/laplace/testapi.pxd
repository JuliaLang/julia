
cdef extern from "testapi.h":
    void func1()
    double func2(double x, double y, double z)

    cdef cppclass Multiplier:
        Multiplier()
        Multiplier(double factor)
        void SetFactor()
        void SetFactor(double f)
        double GetFactor()
        double Multiply(double value)

    double call_virtual_from_cpp (Multiplier *obj, double value)
