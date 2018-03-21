C Semper 6 system module SEMFRMF
C
      INTEGER FUNCTION SEMFRMF(INFORM)
      INTEGER INFORM
C
C Returns INFORM if none of BYTE/INTEGER/FP/COMPLEX set; otherwise
C returns the corresponding form number
C
      LOGICAL VARSET
C
      INCLUDE 'PARAMS'
      INTEGER I,NNAMES(4)
C
C NNAMES contains BYTE, INTEGER, FP and COMPLEX
C
      DATA NNAMES/4220,14980,10240,5413/
C     Additional possibilities, i2 & i4
      DATA NINT2/15680/,NINT4/15760/
C
      SEMFRMF = INFORM
C
      DO 10 I = 1,4
C
C Note that no check is made for conflicting options!
C
         IF (VARSET(NNAMES(I))) SEMFRMF = I - 1
   10 CONTINUE
C Look for i2, i4 options
      IF(VARSET(NINT2))THEN
C       Is it an integer*2 system ?
                IF(LNINT.EQ.2)THEN
                        SEMFRMF=1
                ELSE
                        SEMFRMF=-2
                ENDIF
                RETURN
      ENDIF
C What about i4 option
      IF(VARSET(NINT4))THEN
C       Is it an integer*4 system ?
                IF(LNINT.EQ.4)THEN
                        SEMFRMF=1
                ELSE
                        SEMFRMF=-4
                ENDIF
                RETURN
      ENDIF
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
