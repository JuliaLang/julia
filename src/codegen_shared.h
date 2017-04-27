enum AddressSpace {
    Generic = 0,
    Tracked = 10, Derived = 11, CalleeRooted = 12,
    FirstSpecial = Tracked,
    LastSpecial = CalleeRooted,
};
