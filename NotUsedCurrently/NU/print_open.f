C Semper 6 subsidiary module Print_open
C
      LOGICAL FUNCTION Print_open(PATHNM,DFEXT,icol,irow,ibit,
     $ group_header)
      CHARACTER*(*) PATHNM,DFEXT
C
      integer*2 curs_raster_op(8),size(2)
      integer*2 version(2),group_header(8),izoom,name_size,groups
      integer*4 ibit, block_des,st
      INTEGER NBLANK
      LOGICAL CONOPT,FILMAK,FILSEA,OPT,SEMIOE,SEMKTX
%include '../primitives/display_common.ins'
C
%INCLUDE '../primitives/common.ins'
C
      INTEGER NF
      integer*4 ios 
c      {changed from integer}
      INTEGER FILMAX
      PARAMETER (FILMAX=80)
C
      INTEGER A1FILE(FILMAX)
      LOGICAL EXISTS,LNEW,LOLD,opened
      CHARACTER*(FILMAX) FILE,FILENM
      CHARACTER*3 STAT
C
      EQUIVALENCE (A1FILE,RB1)
C
C Packed names
C
      INTEGER NNAME, NNEW, NOLD
      PARAMETER (NNAME=22453, NNEW=22623, NOLD=24484)
C
c	Not implimented yet
	print_open=.true.
	write(6,*)'Print open not implimented yet'
	return
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
