C Semper 6 processing module RADX50
C---------------------------------------------------------------------
C
C      SUBROUTINE RADX50
C      -----------------
C
C      PARAMETERS:
C
C      None.
C
C      Handles verbs PACK and UNPACK.
C
C---------------------------------------------------------------------
C
      SUBROUTINE RADX50
C     =================
C
      INTEGER IVAL
      LOGICAL SEMCON,SEMXA1
C     Change: LDM, July 2005
C     Ensure that gfortran does not think of range, unpack as intrinsics
      EXTERNAL RANGE, UNPACKF
      CHARACTER*3 UNPACKF
C
      INCLUDE 'COMMON'
C
      INTEGER NDOLLR,NPACK
      PARAMETER (NDOLLR=-11201, NPACK=25643)
C
      REAL X
      INTEGER I,IPTR,CH
C
      CHARACTER*3 CNAME
C
C Return if no arguments present on command line
C
      IPTR = IVAL(NDOLLR)
      IF (IPTR .EQ. 0) GOTO 40
C
C Fetch next item from command line
C
   10 IF (VERB .EQ. NPACK) THEN
         IF (SEMXA1(1,LINBUF,COMLIM,IPTR,X,I)) CONTINUE
      ELSE
         IF (SEMXA1(2,LINBUF,COMLIM,IPTR,X,I)) CONTINUE
         I = X
      ENDIF
      IF (ERROR .NE. 0) GOTO 40
C
C Fault bad name/number
C
      IF (I .EQ. 0) GOTO 50
C
C Fault bad terminator character (only space or comma allowed)
C
      IF (IPTR.LE.COMLIM) THEN
         CH = LINBUF(IPTR)
         IF (.NOT. (CH .EQ. KSPACE .OR. CH .EQ. KCOMMA)) GOTO 50
      ENDIF
C
C Output results to console output stream
C
      CNAME = UNPACKF(I)
C
      IF (VERB .EQ. NPACK) THEN
         WRITE (RECORD,20) CNAME,I
   20    FORMAT ('''',A,''' packs to ',I6)
      ELSE
         WRITE (RECORD,30) I,CNAME
   30    FORMAT (I6,' unpacks to ''',A,'''')
      ENDIF
C
      IF (SEMCON(RECORD)) GOTO 40
C
      IPTR = IPTR + 1
      IF (IPTR .LE. COMLIM) GOTO 10
C
   40 RETURN
C
C Bad syntax
C
   50 ERROR = 17
      GOTO 40
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
