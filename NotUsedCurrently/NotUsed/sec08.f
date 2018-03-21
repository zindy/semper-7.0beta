      SUBROUTINE ANOT2
C----------------------------------------------------------------------
C
C   Performs the operation
C
C   XWIres FRAme POSition 0,0; MARk FRAme POSition X,Y TEXt 'nnnn' LJ
C
C   If a value of TEXt is not supplied, it is prompted for
C
C----------------------------------------------------------------------
C
C  Semper syntax:
C
C  An2   :ANOT2   text='
C
C----------------------------------------------------------------------
C
      INCLUDE 'COMMON'
C
C  Semper functions
C
      LOGICAL SEMKTX,FSINIT,FSTEXT,FSXWIR
      INTEGER IPACK
C
      INTEGER MAXLEN
      PARAMETER (MAXLEN=100)
C
      INTEGER NTEXT,ITEXT(MAXLEN),LENGTH
      REAL XPOS,YPOS
C
C  Initialise graphics  (frame 1, frame coordinates)
C
      IF(FSINIT(1,1)) RETURN
C
C  XWIRES at position 0,0
C
      IF(FSXWIR(0.0,0.0,XPOS,YPOS)) RETURN
C
C  NTEXT = Semper internal form for the key TEXT
C
      NTEXT=IPACK('TEXT')
C
C  Input value of text string to internal form
C
      LENGTH=MAXLEN
      IF(SEMKTX(NTEXT,'Text please ! ',ITEXT,LENGTH,.FALSE.)) RETURN
C
C  Write text at XPOS,YPOS to display, left justified
C  (FSTEXT will clip the text if it exceeds the frame boundaries)
C
      IF(LENGTH .NE. 0) THEN
         IF(FSTEXT(ITEXT,LENGTH,XPOS,YPOS,-1,0)) RETURN
      ENDIF
C
C  Done !
C
      RETURN
      END
