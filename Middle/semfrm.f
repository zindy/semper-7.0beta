C Semper 6 system module SEMFRM
C
      INTEGER FUNCTION SEMFRM(INFORM)
      INTEGER INFORM
C
C Returns INFORM if none of BYTE/INTEGER/FP/COMPLEX set; otherwise
C returns the corresponding form number
C
      LOGICAL VARSET
C
      INTEGER I,NNAMES(4)
C
C NNAMES contains BYTE, INTEGER, FP and COMPLEX
C
      DATA NNAMES/4220,14980,10240,5413/
C
      SEMFRM = INFORM
C
      DO 10 I = 1,4
C
C Note that no check is made for conflicting options!
C
         IF (VARSET(NNAMES(I))) SEMFRM = I - 1
   10 CONTINUE
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
