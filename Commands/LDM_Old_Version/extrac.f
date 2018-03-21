C Semper 6 processing module EXTRAC
C
      SUBROUTINE EXTRAC
C
C Performs general extraction, with bi-linear interpolation if
C necessary.  If key WITH is set, pixel values are obtained at the
C positions indicated by the specified position list.  The extraction
C process is repeated for each layer of a multi-layer source picture.
C
      LOGICAL VARSET,SEMOPN,TSTSRG,MRKREG
      INTEGER IVALPN,SEMFRM
C
      INCLUDE 'COMMON'
C
      INTEGER CLASS,FORM,NCOL,NROW,NLAY
      INTEGER SIZE(3)
      LOGICAL PIXREG
C
      EQUIVALENCE (SIZE,SMGI7),(PIXREG,SMGL3)
C
C Packed names
C
      INTEGER NWITH,NTO
      PARAMETER (NWITH=-5181,NTO=-601)
C
C If key WITH is set, extract at positions contained in position list
C
      IF (VARSET(NWITH)) THEN
         CALL EXTRA2
C
C Otherwise, perform normal extraction on regular grid of points
C
      ELSE
C
C Determine region to extract
C
         IF (TSTSRG(4,LP1)) GOTO 10
C
C Determine output picture size, class and form
C
         NCOL = SIZE(1)
         NROW = SIZE(2)
         NLAY = NLAYS(LP1)
         CLASS= CLASSN(LP1)
         FORM = SEMFRM(FORMN(LP1))
C
C Open new output picture
C
         LP2 = LP1
         IF (SEMOPN(2,IVALPN(NTO),NCOL,NROW,NLAY,
     +                CLASS,FORM,LP2)) GOTO 10
C
C Fault re-use of source picture disc space for output
C
         IF (LP2.EQ.LP1) THEN
            ERROR = 59
         ELSE
C
C Draw extraction frame
C
            IF (.NOT.MRKREG(0)) THEN
               IF (PIXREG) THEN
C
C Extract region by sampling pixels if region contains only whole pixels
C
                  CALL EXTRA3
               ELSE
C
C Otherwise, extract region using bi-linear interpolation
C
                  CALL EXTRA4
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
   10 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
