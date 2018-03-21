      SUBROUTINE ANOT1
C----------------------------------------------------------------------
C
C   Performs the operation MARk FRAme TEXt 'nnnn' POSition 0,0
C   If a value of TEXt is not supplied, it is prompted for
C
C----------------------------------------------------------------------
C
C  Semper syntax:
C
C  An1   :ANOT1   text='
C
C----------------------------------------------------------------------
C
      INCLUDE 'COMMON'
C
C  Semper functions
C
      LOGICAL SEMKTX,FSINIT,FSTEXT
      INTEGER IPACK
C
      INTEGER MAXLEN
      PARAMETER (MAXLEN=100)
C
      INTEGER NTEXT,ITEXT(MAXLEN),LENGTH
C
C  Initialise graphics  (frame 1, frame coordinates)
C
      IF(FSINIT(1,1)) RETURN
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
C  Write text to centre of display, centre justified
C  (FSTEXT will clip the text if it exceeds the frame boundaries)
C
      IF(LENGTH .NE. 0) THEN
         IF(FSTEXT(ITEXT,LENGTH,0.0,0.0,0,0)) RETURN
      ENDIF
C
C  Done !
C
      RETURN
      END
