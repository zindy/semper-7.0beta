C Semper 6 processing module INPUTF
C
      SUBROUTINE INPUTF
C
      LOGICAL OPT
C
      INCLUDE 'COMMON'
C
      INTEGER I
C
      INTEGER CNBASE,CNTYPE
C
      PARAMETER (CNBASE=7)
      PARAMETER (CNTYPE=CNBASE)
      INTEGER NTYPES(CNTYPE)
      LOGICAL LTYPES(CNTYPE)
C
      LOGICAL LSEEN,LRAW,LRAST,LBMP,LTIFF, LOPT, LNUHP, LMS
C
      EQUIVALENCE (LRAW,LTYPES(1))
      EQUIVALENCE (LRAST,LTYPES(2))
      EQUIVALENCE (LBMP,LTYPES(3))
      EQUIVALENCE (LTIFF,LTYPES(4))
      EQUIVALENCE (LOPT,LTYPES(5))
      EQUIVALENCE (LNUHP, LTYPES(6))
      EQUIVALENCE (LMS, LTYPES(7))
C
C     Developers please note:
C        This DATA statement depends on the preprocessor to
C        substitute values for the IPACK function calls. If
C        you need to omit the preprocessor you will have to
C        manually code the values.
C
C     Types currently in NTYPES are raw, ras, bmp, tif, opt, nuhp, ms
      DATA NTYPES /  28863
     +              ,28859
     +              ,3736
     +              ,-367
     +              ,24660
     +              ,23248 , 21560
     +            /
C
C     Explicit check for conflicting options
C
      LSEEN = .FALSE.
      DO 10 I = 1,CNTYPE
         LTYPES(I) = OPT(NTYPES(I))
         IF (LTYPES(I)) THEN
C
C     Option set - any other already seen ?
C
            IF (LSEEN) THEN
               IDERR2 = NTYPES(I)
               ERROR = 60
               GOTO 20
            ELSE
C
C     Remember this option
C
               IDERR = NTYPES(I)
               LSEEN = .TRUE.
            ENDIF
         ENDIF
   10 CONTINUE
C
C     Branch according to type
C
      IF (LRAW) THEN
         CALL INPRAW
      ELSE IF (LRAST) THEN
         CALL INPRAS
      ELSE IF (LBMP) THEN
         CALL INPBMP
      ELSE IF (LTIFF) THEN
         CALL INPTIF
      ELSE IF (LOPT) THEN
         CALL MICRO
      ELSE IF (LNUHP) THEN
         CALL PICGET
      ELSE IF (LMS) THEN
         CALL PICGET2
      ELSE
         CALL INPUNF
      ENDIF
C
   20 RETURN
C
C Copyright (C) 1987-1996 Synoptics Ltd,  All Rights Reserved
C
      END
