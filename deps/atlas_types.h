// This file missing from <ATLAS_DIR>/include/

#ifndef ATLAS_TYPE_H
#define ATLAS_TYPE_H
#define ATL_isize 4
#define ATL_ssize 4
#define ATL_dsize 8
#define ATL_csize 8
#define ATL_zsize 16
#define ATL_iMulBySize(N_) ((((N_)) << 2))
#define ATL_sMulBySize(N_) ((((N_)) << 2))
#define ATL_dMulBySize(N_) ((((N_)) << 3))
#define ATL_cMulBySize(N_) ((((N_)) << 3))
#define ATL_zMulBySize(N_) ((((N_)) << 4))
#define ATL_iDivBySize(N_) ((N_) >> 2)
#define ATL_sDivBySize(N_) ((N_) >> 2)
#define ATL_cDivBySize(N_) ((N_) >> 3)
#define ATL_dDivBySize(N_) ((N_) >> 3)
#define ATL_zDivBySize(N_) ((N_) >> 4)
#endif
