C Semper 6 subsidiary module TEXTU3
C
      LOGICAL FUNCTION TEXTU3(TEXT,LENGTH,START,M,ML,PTR)
      INTEGER TEXT(*),LENGTH,START,M(*),ML,PTR
C
C Searches TEXT(START - LENGTH) for M(ML); iff found, returns TRUE with
C PTR pointing to start of matched string;
C matching is case-independent.
C
      INTEGER I,IC,IT,J,K,LM,PTRM
C
      TEXTU3 = .FALSE.
C
C Trap empty match string (matches at START)
C
      PTR = START
      IF (ML .NE. 0) THEN
C
C Trap empty operand string
C
         IF (START .LE. LENGTH) THEN
C
C Look for single - char match initially
C
            IT = 0
            IC = M(1)
            IF (IC .GE. 97 .AND. IC .LE. 122) IC = IC - 32
            LM = LENGTH - ML + 1
            IF (LM .GE. START) THEN
               DO 20 PTR = START,LM
                  K = TEXT(PTR)
                  IF (K .GE. 97 .AND. K .LE. 122) K = K - 32
                  IF (K .EQ. IC) THEN
C
C First char matched  -  now test remainder
C
                     IF (ML .NE. 1) THEN
                        PTRM = PTR - 1
                        DO 10 I = 2,ML
                           IT = I + PTRM
                           K = TEXT(IT)
                           IF (K .GE. 97 .AND. K .LE. 122) K = K - 32
                           J = M(I)
                           IF (J .GE. 97 .AND. J .LE. 122) J = J - 32
                           IF (K .NE. J) GOTO 20
   10                   CONTINUE
                     ENDIF
                     GOTO 30
                  ENDIF
   20          CONTINUE
            ENDIF
         ENDIF
         GOTO 40
      ENDIF
C
C Match found at PTR
C
   30 TEXTU3 = .TRUE.
   40 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
