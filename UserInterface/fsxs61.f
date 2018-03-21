C Semper 6 subroutine fsxs61 - set xwires style
C
      SUBROUTINE FSXS61(ISTYLE)
      INTEGER ISTYLE
C
      INTEGER KSTYLE,KXTYPE
      COMMON /XSTYLE/ KSTYLE,KXTYPE
C
C Permitted values are:
C   0 - standard cursor
C   1 - rubber band
C   2 - rubber box
C
      IF (ISTYLE .LT. 0 .OR. ISTYLE .GT. 2) THEN
         KSTYLE = 0
      ELSE
         KSTYLE = ISTYLE
      ENDIF
      RETURN
C
C Copyright (C) 1988,1989,1990:  Synoptics Ltd.
C
      END
