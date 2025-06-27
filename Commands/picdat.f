C Semper 6 processing module PICDAT
C
      SUBROUTINE PICDAT
C
C Sets or returns 10 floating-point values stored in the unused
C part of the picture label.  The values are passed via Semper
C variables V0, V1, V2, V3, V4, V5, V6, V7, V8 and V9.  Each
C floating-point value is stored as the four constituent byte
C values, so the representation is not necessarily portable
C between different systems where the floating-point format or
C the byte ordering is not the same.
C
C Syntax: Data :PICDAT get set $1=sel picture=$1 open(lp1,old)=pic
C
      LOGICAL CONOPT,SEMLAB,OPT,SETVAR
      INTEGER IPACK
      REAL    VAL
C
      INTEGER I
      REAL    VALUE
C
      INTEGER*4 I44
C
      PARAMETER ( I44 = 4 )
C
      INTEGER NAME(10)
C
      INCLUDE 'COMMON'
C
      INTEGER LABEL(LNLAB)
C
      EQUIVALENCE (LABEL,RB1)
C
C Packed names - using IPACK this way only works if the Synoptics
C                preprocessor is invoked. Otherwise will need to
C                individually code the values.
C
      DATA NAME /
     +    -4401, -4441, -4481,
     +    -4521, -4561, -4601,
     +    -4641, -4681, -4721,
     +    -4761
     +          /
C
C Fault conflicting options GET and SET
C
      IF (CONOPT(11420,30620)) GOTO 30
C
C Fetch old contents of picture label
C
      IF (SEMLAB(1,LABEL,LP1)) GOTO 30
C
C See if option GET is set
C
      IF (OPT(11420)) THEN
C
C Copy 10 values from picture label into Semper variables V0 to V9
C
         DO 10 I=1,10
            CALL CFORM(LABEL(LBNCTT-I*4),VALUE,NFMINT,NFMBYT,I44)
            IF (SETVAR(NAME(I),VALUE)) GOTO 30
   10    CONTINUE
C
C Otherwise, see if option SET is set
C
      ELSE IF (OPT(30620)) THEN
C
C Copy 10 values from Semper variables V0 to V9 into picture label
C
         DO 20 I=1,10
            VALUE=VAL(NAME(I))
            CALL CFORMI(VALUE,LABEL(LBNCTT-I*4),NFMBYT,NFMINT,I44)
   20    CONTINUE
C
C Store new contents of picture label
C
         IF (SEMLAB(2,LABEL,LP1)) GOTO 30
      ENDIF
C
   30 RETURN
C
C Copyright (C) 1996:  Synoptics Ltd,  All Rights Reserved
C
      END
