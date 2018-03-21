C Semper 6 primitive module FILEXI
C
      LOGICAL FUNCTION FILEXI(PATHNM,EXIPAR)
C
C Checks if a named file exists
C
      CHARACTER*(*) PATHNM
      LOGICAL EXIPAR
C
      INTEGER PATLEN
      LOGICAL EXISTS
C
      INTEGER LNBLNK
      INTEGER ACCESS
C
      INCLUDE 'COMMON'
C
      FILEXI = .TRUE.
C
      PATLEN = LNBLNK(PATHNM)
C     Ensure correct termination at least for g77 (others?)
      PATLEN=PATLEN+1
      PATHNM(PATLEN:PATLEN)=CHAR(0)
C        ILAST=ICHAR(PATHNM(PATLEN:PATLEN))
C        write(6,*)'Terminal character ',PATHNM(PATLEN:PATLEN)
C        write(6,*)'Integer ',ILAST
C        write(6,*)'File: ',pathnm(1:pathlen)
C
C Now see if file exists in a readable form
C
      EXISTS = ACCESS(PATHNM(1:PATLEN),'r') .EQ. 0
      GOTO 20
   20 FILEXI = .FALSE.
C
      EXIPAR = EXISTS
      RETURN
C
C Copyright (C) 1989-1996: Synoptics Ltd,  All Rights Reserved
C
      END
