
cimport testapi

cdef class Multiplier:
    cdef testapi.Multiplier* multiplier

    def __cinit__(self, factor=None):
        if factor is None:
            self.multiplier = new testapi.Multiplier()
        else:
            self.multiplier = new testapi.Multiplier(<double>factor)

    def __dealloc__(self):
        del self.multiplier

    def SetFactor(self, factor=None):
        if factor is None:
            self.multiplier.SetFactor()
        else:
            self.multiplier.SetFactor(<double>factor)

    def GetFactor(self):
        return self.multiplier.GetFactor()

    def Multiply(self, double value):
        return self.multiplier.Multiply(value)


def func1():
    testapi.func1()

def func2(double x, double y, double z):
    return testapi.func2(x, y, z)

def call_virtual_from_cpp(Multiplier obj, double value):
    return testapi.call_virtual_from_cpp(obj.multiplier, value)
