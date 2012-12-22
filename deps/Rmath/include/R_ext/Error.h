/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2005   Robert Gentleman, Ross Ihaka 
 *                            and the R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#ifndef R_ERROR_H_
#define R_ERROR_H_

#ifdef  __cplusplus
extern "C" {
#endif

void	Rf_error(const char *, ...);
void	Rf_warning(const char *, ...);
void	WrongArgCount(const char *);
void	UNIMPLEMENTED(const char *);
void 	R_ShowMessage(const char *s);
    

#ifdef  __cplusplus
}
#endif

#ifndef R_NO_REMAP
#define error Rf_error
#define warning Rf_warning
#endif


#endif /* R_ERROR_H_ */
