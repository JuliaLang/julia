/* Julia DTrace provider */

provider julia {
    probe gc__begin(int collection)
    probe gc__stop_the_world()
    probe gc__mark__begin()
    probe gc__mark__end(int64 scanned_bytes, int64 perm_scanned_bytes)
    probe gc__sweep__begin(int full)
    probe gc__sweep__end()
    probe gc__end()
    probe gc__finalizer()
}

#pragma D attributes Evolving/Evolving/Common provider julia provider
#pragma D attributes Evolving/Evolving/Common provider julia module
#pragma D attributes Evolving/Evolving/Common provider julia function
#pragma D attributes Evolving/Evolving/Common provider julia name
#pragma D attributes Evolving/Evolving/Common provider julia argst
