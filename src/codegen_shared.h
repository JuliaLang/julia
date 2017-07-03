enum AddressSpace {
    Generic = 0,
    Tracked = 10, Derived = 11, CalleeRooted = 12,
    FirstSpecial = Tracked,
    LastSpecial = CalleeRooted,
};

static bool isSpecialAS(unsigned AS) {
    return AddressSpace::FirstSpecial <= AS && AS <= AddressSpace::LastSpecial;
}

#define JLCALL_CC (CallingConv::ID)36
#define JLCALL_F_CC (CallingConv::ID)37
