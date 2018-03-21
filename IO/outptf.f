C Semper 6 processing module OUTPTF
C
      SUBROUTINE OUTPTF
C
      LOGICAL OPT
C
      INCLUDE 'COMMON'
C
      INTEGER I
C
      LOGICAL LNEW
C
      INTEGER CNTYPE
      PARAMETER (CNTYPE=4)
      INTEGER NTYPES(CNTYPE)
      LOGICAL LTYPES(CNTYPE)
C
      LOGICAL LSEEN,LTIFF
      EQUIVALENCE (LTIFF,LTYPES(1))
C
      LOGICAL LRAST,LRAW
      EQUIVALENCE (LRAST,LTYPES(2))
      EQUIVALENCE (LRAW,LTYPES(3))
      LOGICAL LBMP
      EQUIVALENCE (LBMP,LTYPES(4))
C
C     Developers please note:
C        This DATA statement depends on the preprocessor to
C        substitute values for the IPACK function calls. If
C        you need to omit the preprocessor you will have to
C        manually code the values.
C
      DATA NTYPES / -367,
     +              28859,
     +              28863,
     +              3736
     +            /
C
C Manually check for conflicting options
C
      LSEEN = .FALSE.
      DO 10 I = 1,CNTYPE
         LTYPES(I) = OPT(NTYPES(I))
         IF (LTYPES(I)) THEN
C
C Option set - any other already seen ?
C
            IF (LSEEN) THEN
               IDERR2 = NTYPES(I)
               ERROR = 60
               GOTO 20
            ELSE
C
C Remember this option
C
               IDERR = NTYPES(I)
               LSEEN = .TRUE.
            ENDIF
         ENDIF
   10 CONTINUE
C
      LNEW = OPT(22623)
C
C Branch according to type
C
      IF (LTIFF) THEN
         CALL OUTTIF(LNEW)
      ELSE IF (LBMP) THEN
         CALL OUTBMP(LNEW)
      ELSE IF (LRAW) THEN
         CALL OUTRAW(LNEW)
      ELSE IF (LRAST) THEN
         CALL OUTRAS(LNEW)
      ELSE
         CALL OUTUNF(LNEW)
      ENDIF
C
   20 RETURN
C
C Copyright (C) 1987-1995 Synoptics Ltd,  All Rights Reserved
C
      END
