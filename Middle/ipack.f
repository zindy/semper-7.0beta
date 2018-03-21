C Semper 6 system routine IPACK
C
      INTEGER FUNCTION IPACK(NAME)
C
C This a utility function that provides an easy though less
C compact way to encode 3-character names in radix-50 format.
C IPACK could be used in the first instance and then replaced
C later with the corresponding PARAMETER constant.
C
      CHARACTER*(*) NAME
      CHARACTER*3   NAM3
      CHARACTER*39  VALID
      INTEGER I,J,ICODE(3)
C
      VALID = 'abcdefghijklmnopqrstuvwxyz$$$0123456789'
C
C Look for first non-blank character in NAME
C
      DO 20 J=1,LEN(NAME)
C
C If non-blank, encode NAME in radix-50 format
C
         IF (NAME(J:J).NE.' ') THEN
C
C Extract the first 3 non-blank characters (blank fill if less than 3)
C
            NAM3=NAME(J:)
C
C Convert each character to its corresponding integer code:
C     A to Z -> 1 to 26
C     a to z -> 1 to 26
C     0 to 9 -> 30 to 39
C     $ -> 27, and the rest -> 0
C
            DO 10 I=1,3
C
C See if character is A to Z
C
               ICODE(I)=INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',NAM3(I:I))
C
C If not, then see if character is a to z, 0 to 9 or $
C
               IF (ICODE(I).EQ.0) ICODE(I)=INDEX(VALID,NAM3(I:I))
   10       CONTINUE
C
C Combine integer character codes into radix-50 format
C
            IF (ICODE(1).GE.20) THEN
               I=ICODE(1)-20
            ELSE
               I=ICODE(1)
            ENDIF
C
            IPACK=40*(40*I+ICODE(2))+ICODE(3)
C
            IF (ICODE(1).GE.20) IPACK=-(IPACK+1)
C
            GOTO 30
         ENDIF
   20 CONTINUE
C
C Return zero for blank NAME
C
      IPACK = 0
C
   30 RETURN
C
C Copyright (C) 1987,1989,1990:  Synoptics Ltd,  All Rights Reserved
C
      END
