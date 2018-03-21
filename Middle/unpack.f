C Semper 6 system routine UNPACKF
C
      CHARACTER*3 FUNCTION UNPACKF(IPACK)
C
C This is a utility function which unpacks a name in radix-50 format
C and returns it as a 3-character string.  UNPACK does the opposite
C to function IPACK.
C
      INTEGER IPACK
C
      INTEGER I,ICODE(3)
      CHARACTER*39 VALID
C
      VALID = 'abcdefghijklmnopqrstuvwxyz$  0123456789'
C
C Extract 3 integer character codes from name in radix-50 format
C
      IF (IPACK.LT.0) THEN
         I = -(IPACK+1)
      ELSE
         I = IPACK
      ENDIF
C
      ICODE(1)=I/(40*40)
      ICODE(2)=I/40-40*ICODE(1)
      ICODE(3)=I-40*(40*ICODE(1)+ICODE(2))
C
      IF (IPACK.LT.0) ICODE(1)=ICODE(1)+20
C
C Convert each integer code into CHARACTER form:
C    1 to 26 -> a to z
C    30 to 39 -> 0 to 9
C    27 -> $, and any other codes default to blank
C
      DO 10 I=1,3
C
C Check for valid integer code, otherwise convert to blank
C
         IF (ICODE(I).LT.1 .OR. ICODE(I).GT.39) THEN
            UNPACKF(I:I)=' '
         ELSE
            UNPACKF(I:I)=VALID(ICODE(I):ICODE(I))
         ENDIF
   10 CONTINUE
C
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
