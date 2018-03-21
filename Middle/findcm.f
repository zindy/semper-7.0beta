C Semper 6 subsidiary module FINDCM
C
      LOGICAL FUNCTION FINDCM(LPN,CM,SUBREG,XCEN,YCEN,R,VALUE,XPOS,YPOS)
      REAL XCEN,YCEN,R,VALUE,XPOS,YPOS
      INTEGER LPN
      LOGICAL CM,SUBREG
C
C Finds centre of mass (CM = .TRUE.) or highest or lowest point
C (CM = .FALSE.) of source picture LPN.  If SUBREG = .TRUE., a circular
C sub-region (centre (XCEN,YCEN), radius R) is scanned instead of the
C entire source picture.  The position of the centre of mass or peak
C is returned in (XPOS,YPOS).  The mass or peak value is returned in
C VALUE.  If the option SQUARED is set, squared pixel values are used
C to find the centre of mass or peak.  If option NEGATIVE is set to NO,
C only pos values are used to find the centre of mass.  A search for
C the highest pixel value is made, unless option LOWEST is set, in which
C case, the lowest value is searched for.  When determining a centre of
C mass with a circular sub-region, a second pass is made if option
C ITERATED is set, with the centre of the sub-region set to the centre
C of mass determined in the first pass.
C
      LOGICAL OPT,OPTNO,SEMROW
C
      INCLUDE 'COMMON'
C
      REAL RR,X,X0,X1,X2,Y,Y0,Y1,Y2,SX,SY,T,TS,PC,DX,DY
      INTEGER CCOL,CROW,NCOL,NROW,I,I1,I2,J,J1,J2,IPASS,NPASS
      LOGICAL DISREG,FIRST,SQUARE,POSITV,LOWEST
C
C Packed names
C
      INTEGER NSQUAR,NITERA,NNEGAT,NLOWES
      PARAMETER (NSQUAR=31101, NITERA=15205, NNEGAT=22607, NLOWES=19823)
C
      FINDCM=.TRUE.
C
C Fetch source picture size and centre position
C
      NCOL = NCOLS(LPN)
      NROW = NROWS(LPN)
      CCOL = CCOLN(LPN)
      CROW = CROWN(LPN)
C
C See if option SQUARED is set
C
      SQUARE = OPT(NSQUAR)
C
C See if circular sub-region is required
C
      IF (SUBREG) THEN
         RR=R*R
         XPOS=XCEN
         YPOS=YCEN
      ELSE
         I1=1
         I2=NCOL
         J1=1
         J2=NROW
      ENDIF
C
C Single pass through source picture normally required
C
      NPASS = 1
C
C Check remaining options
C
      IF (CM) THEN
C
C Two passes required if sub-region required and option ITERATED is set
C
         IF (SUBREG.AND.OPT(NITERA)) NPASS = 2
C
C See if option NEGATIVE is set to NO
C
         POSITV=OPTNO(NNEGAT)
      ELSE
C
C Otherwise, see if option LOWEST is set
C
         LOWEST=OPT(NLOWES)
      ENDIF
C
C Process source picture with one or two passes
C
      DO 40 IPASS=1,NPASS
C
C If circular sub-region, determine range of source rows to process
         IF (SUBREG) THEN
C
C Determine sub-region centre position in pixel coordinates
C
            X0 = REAL(CCOL)+XPOS
            Y0 = REAL(CROW)-YPOS
C
C Determine Y limits of sub-region
C
            Y1 = Y0-R
            Y2 = Y0+R
C
C Fault sub-region outside source picture
C
            IF (Y1.GT.REAL(NROW).OR.Y2.LT.1.0) THEN
               ERROR = 9
               GOTO 50
            ENDIF
C
C Determine start and end row numbers (rounding inwards)
C
            IF (Y1.GT.1.0) THEN
               J1=INT(Y1)
               IF (REAL(J1).LT.Y1) J1=J1+1
            ELSE
               J1=1
            ENDIF
C
            IF (Y2.LT.REAL(NROW)) THEN
               J2=INT(Y2)
               IF (REAL(J2).GT.Y2) J2=J2-1
            ELSE
               J2=NROW
            ENDIF
         ENDIF
C
C Initialise variables and flags
C
         IF (CM) THEN
            VALUE=0.0
            SX=0.0
            SY=0.0
            TS=0.0
            PC=0.0
         ELSE
            FIRST=.TRUE.
         ENDIF
C
         DISREG=.TRUE.
C
C Process relevant source picture rows
C
         DO 30 J=J1,J2
C
C Determine Y picture coordinate for row
C
            Y=REAL(CROW-J)
C
C If circular sub-region required, determine range of pixels to process
C
            IF (SUBREG) THEN
C
C Determine X limits of row
C
               DY=Y0-REAL(J)
               DX=SQRT(RR-DY*DY)
               X1=X0-DX
               X2=X0+DX
C
C Skip this row if limits outside source picture limits
C
               IF (X1.GT.REAL(NCOL).OR.X2.LT.1.0) GOTO 30
C
C Determine start and end pixel numbers (rounding inwards)
C
               IF (X1.GT.1.0) THEN
                  I1=INT(X1)
                  IF (REAL(I1).LT.X1) I1=I1+1
               ELSE
                  I1=1
               ENDIF
C
               IF (X2.LT.REAL(NCOL)) THEN
                  I2=INT(X2)
                  IF (REAL(I2).GT.X2) I2=I2-1
               ELSE
                  I2=NCOL
               ENDIF
            ENDIF
C
C Skip this row if start and end pixel numbers reversed
C
            IF (I1.GT.I2) GOTO 30
C
C Read source row from LPN
C
            IF (SEMROW(1,RB1,NFMFP,J,1,LPN)) GOTO 50
C
C Set flag for sub-region overlapping source picture
C
            DISREG=.FALSE.
C
C Process source row
C
            IF (CM) THEN
C
C Process pixels to determine centre of mass
C
               DO 10 I=I1,I2
C
C Fetch pixel value
C
                  T=RB1(I)
C
C Skip this pixel if option NEGATIVE is set to NO and the pixel value
C is zero or negative
C
                  IF (POSITV.AND.T.LE.0.0) GOTO 10
C
C Determine X picture coordinate for pixel
C
                  X=REAL(I-CCOL)
C
C Process pixel value
C
                  IF (SQUARE) T=T*T
C
                  VALUE=VALUE+T
                  SX=SX+X*T
                  SY=SY+Y*T
                  TS=TS+T*T
                  PC=PC+1.0
   10          CONTINUE
C
C Otherwise, process pixels to determine peak value
C
            ELSE
C
C Set up initial peak value and position if first row to be processed
C
               IF (FIRST) THEN
                  VALUE=RB1(I1)
                  IF (SQUARE) VALUE=VALUE*VALUE
                  SX=REAL(I1-CCOL)
                  SY=Y
                  FIRST=.FALSE.
               ENDIF
C
C Process pixels
C
               DO 20 I=I1,I2
C
C Fetch pixel value
C
                  T=RB1(I)
                  IF (SQUARE) T=T*T
C
C Skip this pixel if its value does not exceed the current peak value
C
                  IF (LOWEST) THEN
                     IF (T.GE.VALUE) GOTO 20
                  ELSE
                     IF (T.LE.VALUE) GOTO 20
                  ENDIF
C
C Determine X picture coordinate for pixel
C
                  X=REAL(I-CCOL)
C
C Update peak value and position
C
                  VALUE=T
                  SX=X
                  SY=Y
   20          CONTINUE
            ENDIF
   30    CONTINUE
C
C Fault a sub-region that does not overlap the source picture
C
         IF (DISREG) THEN
            ERROR=9
            GOTO 50
         ENDIF
C
C Determine resulting position for centre of mass or peak
C
         IF (CM) THEN
C
C Fault source picture with near zero mass
C
            IF (VALUE*VALUE.LT.PC*TS/1E8) THEN
               ERROR=77
               IDMESS = 'Total mass near zero'
               GOTO 50
            ENDIF
C
C Divide moment by mass for centre of mass
C
            XPOS=SX/VALUE
            YPOS=SY/VALUE
C
C Otherwise, just return peak position
C
         ELSE
            XPOS=SX
            YPOS=SY
         ENDIF
   40 CONTINUE
C
      FINDCM=.FALSE.
C
   50 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
