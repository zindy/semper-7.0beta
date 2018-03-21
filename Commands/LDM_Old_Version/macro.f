C Semper 6 processing module MACRO
C---------------------------------------------------------------------
C
C      SUBROUTINE MACRO
C      ----------------
C
C      PARAMETERS:
C
C      None.
C
C      Handles command MACRO
C
C---------------------------------------------------------------------
C
      SUBROUTINE MACRO
C     ================
C
      INTEGER IVAL
      LOGICAL SEMMAC,SEMXPL
C
      INCLUDE 'COMMON'
C
      INTEGER NDOLLR
      PARAMETER (NDOLLR=-11201)
C
      REAL X(2)
      INTEGER IPTR,N,VD(4)
C
      IPTR = IVAL(NDOLLR)
      IF (IPTR .EQ. 0) GOTO 10
C
C MACRO processing - read macro number
C
      N = 1
      IF (SEMXPL(LINBUF,COMLIM,IPTR,X,N)) THEN
         ERROR = 17
      ELSE
         N = NINT(X(1))
         IF (SEMMAC(2,N,NEXTSC,VD,LINBUF,LINLEN,LNLINB)) GOTO 10
      ENDIF
C
   10 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
