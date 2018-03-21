/* -----------------------------------------------------------------------------
  Copyright (c) 2005 L. D. Marks

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  ----------------------------------------------------------------------------- */

#include <stdio.h>
#include <string.h>
#include "ftypes.h"

/* Find Defined variables in Makefile.am 
Use
        INTEGER SEMPBUILD
        CHARACTER*300 VSTRING
        I=SEMPBUILD(N,VSTRING)
Input:
        N       Integer, what to return (see below)
Output:
        I       Length of VSTRING that has information
        VSTRING Text with relevant information */

/* N=1 Returns BIGENDE if Bigendian CPU (e.g. HP, SGI) or LITENDE (e.g. Intel) */
char *Package     = PACKAGE_STRING ;    /* N=2  Package information */
char *User        = BUSER ;             /* N=3  Who compiled the code */
char *Host	  = BHOST ;             /* N=4  Short name of the compilation host */
char *Triplet	  = HOST_TRIPLET ;      /* N=5  Computer type information */
char *Fortran	  = F77 ;               /* N=6  What Fortran compiler was used */
char *Cversion	  = CC  ;               /* N=7  What C compiler was used (not version) */
char *Patches     = PATCHES ;           /* N=8  What patch files were used */
char *BrowserName = BROWSER ;           /* N=9  Browser in compilation   */
char *SEMPER_PATH = PREFIX ;            /* N=10 Where code was installed */
/* char *BTime       = BTIME ;             N=11 Time when code was compiled */
char *BTime       = "Unknown" ;         /* N=11 Time when code was compiled */

integerfunction sempbuild_ (N, buildout, buildvar_len)
/* Returns various build parameters
 	Input:	N, the code
	Output:	N, the length of the buildvar
		buildvar -- the variable/parameter itself
*/
char *buildout;
long  buildvar_len;
F_integer *N;
{
	int opcode, M ;
	char *buildvar ;

	opcode = *N ;
/*	fprintf(stderr, "\n Case : %d ",opcode) ; */
	switch ( opcode )
	{
		case 1:
		{
#ifdef WORDS_BIGENDIAN
			M	= 8;
			buildvar ="WORDBIGE";
#else
			M	= 8;
			buildvar ="WORDLITE";
#endif
		}
		break ;
                case 2:
                {
                        M=strlen(Package);
                        buildvar = Package;
                }
                break ;
		case 3:
		{
			M=strlen(User);
			buildvar=User;
		}
		break ;
		case 4:
		{
			M=strlen(Host);
			buildvar=Host;
		}
		break ;
		case 5:
		{
			M=strlen(Triplet);
			buildvar=Triplet;
		}
		break ;
		case 6:
		{
			M=strlen(Fortran);
			buildvar=Fortran;
		}
		break ;
		case 7:
		{
			M=strlen(Cversion);
			buildvar=Cversion;
		}
		break ;
		case 8:
		{
			M=strlen(Patches);
			buildvar=Patches;
		}
		break ;
		case 9:
		{
			M=strlen(BrowserName);
			buildvar=BrowserName;
		}
		break ;
                case 10:
                {
                        M=strlen(SEMPER_PATH);
                        buildvar=SEMPER_PATH;
                }
                break ;
                case 11:
                {
                        M=strlen(BTime);
                        buildvar=BTime;
                }
                break ;

	}
/*	fprintf(stderr,"\n Output '%s' ",buildvar); */
	sprintf(buildout,"%s",buildvar);
	return M ;
}

integerfunction sempbuild (N, buildvar, buildvar_len)
char *buildvar;
long  buildvar_len;
F_integer *N ;
{
	return sempbuild_ (N, buildvar, buildvar_len) ;
}

