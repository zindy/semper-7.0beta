C Semper 6 subsidiary module SHOW5
C
      SUBROUTINE SHOW5(NAMES,N)
C
C Sorts list of packed names
C
      INTEGER NAMES(*),N
C
      INTEGER I,J,K,M,JNAME,KNAME
C
      M=0
C
   10 IF (M.LT.N) THEN
         M=2*M+1
         GOTO 10
      ENDIF
C
   20 M=M/2
C
      IF (M.NE.0) THEN
         DO 40 I=1,N-M
            J=I
C
   30       IF (J.GT.0) THEN
               K=J+M
C
C Swap names J and K if necessary
C
               JNAME=NAMES(J)
               KNAME=NAMES(K)
C
C Alphabetic order among packed names is upwards positively, then
C downwards negatively
C
               IF (JNAME.LE.0) THEN
                  IF (KNAME.LE.0) THEN
                     IF (JNAME.GT.KNAME) GOTO 40
                  ENDIF
               ELSE
                  IF (KNAME.LT.0) GOTO 40
                  IF (JNAME.LT.KNAME) GOTO 40
               ENDIF
C
               NAMES(J)=KNAME
               NAMES(K)=JNAME
C
               J=J-M
               GOTO 30
            ENDIF
   40    CONTINUE
         GOTO 20
      ENDIF
C
      RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
