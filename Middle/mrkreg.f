C Semper system module MRKREG
C
      LOGICAL FUNCTION MRKREG(IDUMMY)
      INTEGER IDUMMY
C
C Marks a sub-region on the display picture specified by key MARK.
C The position and shape of the sub-region is defined by values set up
C in COMMON by a preceeding call to TSTSRG.
C
      LOGICAL MARSET,FSINIT,FSLINE,FSFLUS
C
      INCLUDE 'COMMON'
C
      REAL CORNER(2),U(2),V(2),X1,X2,X3,X4,Y1,Y2,Y3,Y4
      INTEGER MARK,SIZE(2)
      LOGICAL ANNOT
C
      EQUIVALENCE (SIZE,SMGI7),(CORNER,SMGR1),(U,SMGR3),(V,SMGR5)
C
      MRKREG=.TRUE.
C
C Fetch and check value of key MARK
C
      IF (MARSET(ANNOT,MARK)) GOTO 10
C
C If key MARK appropriately set, annotate specified display picture
C
      IF (ANNOT) THEN
C
C Initialise display graphics
C
         IF (FSINIT(3,MARK)) GOTO 10
C
C Annotate display picture only if 2-D image
C
         IF (FSPTYP.EQ.1) THEN
C
C Determine position of four corner points of sub-region
C
            X1=CORNER(1)
            Y1=CORNER(2)
            X2=CORNER(1)+REAL(SIZE(1)-1)*U(1)
            Y2=CORNER(2)+REAL(SIZE(1)-1)*U(2)
            X3=CORNER(1)+REAL(SIZE(2)-1)*V(1)
            Y3=CORNER(2)+REAL(SIZE(2)-1)*V(2)
            X4=X2+X3-X1
            Y4=Y2+Y3-Y1
C
C Draw sub-region border
C
            IF (FSLINE(X1,Y1,X2,Y2)) GOTO 10
            IF (FSLINE(X2,Y2,X4,Y4)) GOTO 10
            IF (FSLINE(X4,Y4,X3,Y3)) GOTO 10
            IF (FSLINE(X3,Y3,X1,Y1)) GOTO 10
C
C Flush contents of graphics buffer
C
            IF (FSFLUS()) GOTO 10
         ENDIF
      ENDIF
C
      MRKREG=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      IJUNK=IDUMMY
      END
