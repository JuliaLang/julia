enum AddressSpace {
    Generic = 0,
    Tracked = 10, Derived = 11, CalleeRooted = 12,
    FirstSpecial = Tracked,
    LastSpecial = CalleeRooted,
};

#define JLCALL_CC (CallingConv::ID)36
#define JLCALL_F_CC (CallingConv::ID)37
