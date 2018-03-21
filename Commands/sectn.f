C Semper 6 processing module SECTN
C
      SUBROUTINE SECTN
C
C Calculates rotationally averaged radial section of LP1, which may be
C offset from the source picture origin by means of the keys POSITION
C and PO2.  The direction of the section's centre line is specified
C by the ANGLE key.  A section along the centre line is extracted using
C bi-linear interpolation.  If the key WIDTH is set, any pixels falling
C inside the angular limits ANGLE-WIDTH/2 to ANGLE+WIDTH/2 are added
C  to the section and finally averaged.  If key MARK is set, the centre
C line and angular limits of the section are annotated.
C
      LOGICAL SEMOPN,SEMCEN,SEMROW,EXTRCT,MARSET,FSINIT,FSLINE
      LOGICAL FSARRO,FSFLUS
      INTEGER SEMFRM,IVALPN
      REAL VAL
C
      INCLUDE 'COMMON'
C
      INTEGER CLASS,FORM,CCOL,CROW,NCOL,NROW,IRCEN,I,IRMAX,NPIC,MARK
      INTEGER IR,J,J1,J2,I1,I2
      REAL CR(2),SR(2),XR(2),YR(2),RR(2),XC(4),YC(4),WIDTH
      REAL DX,DY,X0,Y0,XMIN,XMAX,YMIN,YMAX,R,ANGLE,C,S,XCEN,YCEN,RMAX
      REAL RCEN,Y1,Y2,Y,YY,X1,X2,X
      LOGICAL WIDE,GAP,ANNOT
C
      REAL B3(0:LNBUF/LNREAL),B4(0:LNBUF/LNREAL),B5(0:LNBUF/LNREAL)
      EQUIVALENCE (B3,RB3),(B4,RB4),(B5,RB5)
C
C Packed names
      INTEGER NANGLE,NWIDTH,NTO,NPOS,NPO2
      PARAMETER (NANGLE=2167, NWIDTH=-5165, NTO=-601)
      PARAMETER (NPOS=26219, NPO2=26232)
C
      REAL DUMMY(2)
      DATA DUMMY / 2*0.0 /
C
C Fault multi-layer source picture
      IF (NLAYS(LP1).NE.1) THEN
         ERROR=62
         IDERR=VERB
         RETURN
      ENDIF
C
C Fault bad value for WIDTH key
      WIDTH=VAL(NWIDTH)
      IF (WIDTH.LT.0.0.OR.WIDTH.GT.TWOPI) THEN
         ERROR=3
         IDERR=NWIDTH
         RETURN
      ENDIF
C
C Fetch source picture size and centre position
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      CCOL=CCOLN(LP1)
      CROW=CROWN(LP1)
C
C Fetch sector centre position offset
      DX=VAL(NPOS)
      DY=VAL(NPO2)
C
C Determine centre position (including offset) in pixel coordinates
      X0=REAL(CCOL)+DX
      Y0=REAL(CROW)-DY
C
C Set up source picture limits (in sector coordinates)
      XMIN=1.0-X0
      XMAX=REAL(NCOL)-X0
      YMIN=Y0-REAL(NROW)
      YMAX=Y0-1.0
C
C Fault offset which put centre position outside source picture
      IF (XMIN.GT.0.0.OR.XMAX.LT.0.0.OR.YMIN.GT.0.0.OR.YMAX.LT.0.0) THEN
         ERROR=3
         IDERR=NPOS
         RETURN
      ENDIF
C
C Determine radius that encompasses entire source picture
      R=REAL(NCOL+NROW)
C
C Determine direction of centre line
      ANGLE=VAL(NANGLE)
      C=COS(ANGLE)
      S=SIN(ANGLE)
C
C Determine length of centre line
      CALL SECTN2(XCEN,YCEN,RCEN,R,C,S,XMIN,XMAX,YMIN,YMAX)
C
C Determine length of centre line in pixel units
      IRCEN=INT(RCEN)
C
C Determine whether section has finite area
      WIDE=WIDTH.NE.0.0
C
C Determine size of section according to type of section
      IF (WIDE) THEN
C
C Determine directions of sector limits
         WIDTH=WIDTH/2.0
C
C See if sector limits are distinct
         GAP=SIN(WIDTH).GT.0.0
C
C If so, determine separate sector limits
         IF (GAP) THEN
            CR(1)=COS(ANGLE-WIDTH)
            SR(1)=SIN(ANGLE-WIDTH)
            CR(2)=COS(ANGLE+WIDTH)
            SR(2)=SIN(ANGLE+WIDTH)
C
C Otherwise, set sector limits the same
         ELSE
            CR(1)=-C
            SR(1)=-S
            CR(2)=-C
            SR(2)=-S
         ENDIF
C
C Determine lengths of sector limits
         DO 10 I=1,2
            CALL SECTN2(XR(I),YR(I),RR(I),R,CR(I),SR(I),
     +                  XMIN,XMAX,YMIN,YMAX)
   10    CONTINUE
C
C Set up sector coordinates of four corner points of source picture
         XC(1)=XMIN
         YC(1)=YMIN
         XC(2)=XMIN
         YC(2)=YMAX
         XC(3)=XMAX
         YC(3)=YMIN
         XC(4)=XMAX
         YC(4)=YMAX
C
C Determine extreme Y values and maximum radius
         YMIN=MIN(0.0,YR(1),YR(2))
         YMAX=MAX(0.0,YR(1),YR(2))
         RMAX=MAX(RR(1),RR(2))
C
         DO 20 I=1,4
            IF (XC(I).NE.0.0.OR.YC(I).NE.0.0) THEN
               IF (ABS(ATAN2(S*XC(I)-C*YC(I),C*XC(I)+S*YC(I))).LT.WIDTH)
     +               THEN
                  YMIN=MIN(YMIN,YC(I))
                  YMAX=MAX(YMAX,YC(I))
                  RMAX=MAX(RMAX,SQRT(XC(I)*XC(I)+YC(I)*YC(I)))
               ENDIF
            ENDIF
   20    CONTINUE
C
C Determine length of section in pixel units (rounding up)
         IRMAX=INT(RMAX)
         IF (REAL(IRMAX).LT.RMAX) IRMAX=IRMAX+1
C
C Otherwise, section consists of just centre line
      ELSE
         IRMAX=IRCEN
      ENDIF
C
C Open new output picture
      NPIC=IVALPN(NTO)
      CLASS=CLASSN(LP1)
      FORM=SEMFRM(FORMN(LP1))
      LP2=LP1
      IF (SEMOPN(2,NPIC,IRMAX+1,1,1,CLASS,FORM,LP2)) RETURN
C
C Reset section centre position
      IF (SEMCEN(LP2,1,1,1)) RETURN
C
C Fetch and check value of key MARK
      IF (MARSET(ANNOT,MARK)) RETURN
C
C If key MARK appropriately set, annotate specified display picture
      IF (ANNOT) THEN
C
C Initialise display graphics
         IF (FSINIT(3,MARK)) RETURN
C
C Annotate display picture only if 2-D image
         IF (FSPTYP.EQ.1) THEN
C
C Draw centre line
            IF (FSARRO(DX,DY,DX+XCEN,DY+YCEN)) RETURN
C
C Draw sector limits if relevant
            IF (WIDE) THEN
               DO 30 I=1,2
                  IF (FSLINE(DX,DY,DX+XR(I),DY+YR(I))) RETURN
   30          CONTINUE
            ENDIF
C
C Flush graphics buffer
            IF (FSFLUS()) RETURN
         ENDIF
      ENDIF
C
C Calculate all pixel positions along centre line
      DO 40 IR=0,IRCEN
         R=REAL(IR)
         B3(IR)=X0+C*R
         B4(IR)=Y0-S*R
   40 CONTINUE
C
C Extract pixel values from source picture using bi-linear interpolation
      IF (EXTRCT(LP1,IRCEN+1,NFMCOM,1,.FALSE.,DUMMY)) RETURN
C
C If section has finite area, sum all pixels that lie within sector
      IF (WIDE) THEN
C
C Copy real and imaginary interpolated section values into separate
C row buffers
         I=1
         DO 50 IR=0,IRCEN
            B3(IR)=RB2(I)
            B4(IR)=RB2(I+1)
            B5(IR)=1.0
            I=I+2
   50    CONTINUE
C
C Zero rest of section
         DO 60 IR=IRCEN+1,IRMAX
            B3(IR)=0.0
            B4(IR)=0.0
            B5(IR)=0.0
   60    CONTINUE
C
C Determine range of source rows to process (rounding inwards)
         Y1=Y0-YMAX
         J1=INT(Y1)
         IF (REAL(J1).LT.Y1) J1=J1+1
C
         Y2=Y0-YMIN
         J2=INT(Y2)
         IF (REAL(J2).GT.Y2) J2=J2-1
C
C Process source picture
         DO 80 J=J1,J2
C
C Read source row from LP1
            IF (SEMROW(1,RB1,NFMCOM,J,1,LP1)) RETURN
C
C Determine Y sector coordinate
            Y=Y0-REAL(J)
            YY=Y*Y
C
C Initialise X limits
            X1=1.0
            I1=1
            X2=REAL(NCOL)
            I2=NCOL
C
C If sector limits are distinct, determine proper X limits
            IF (GAP) THEN
C
C If centre row, truncate X limits to centre if limit outside sector
               IF (Y.EQ.0.0) THEN
C
C Check lower X limit
                  IF (XMIN.NE.0.0) THEN
                     IF (ABS(ATAN2(S*XMIN,C*XMIN)).GT.WIDTH) THEN
                        I1=INT(X0)
                        IF (REAL(I1).LT.X0) I1=I1+1
                     ENDIF
                  ENDIF
C
C Check upper X limit
                  IF (XMAX.NE.0.0) THEN
                     IF (ABS(ATAN2(S*XMAX,C*XMAX)).GT.WIDTH) THEN
                        I2=INT(X0)
                        IF (REAL(I2).GT.X0) I2=I2-1
                     ENDIF
                  ENDIF
C
C Otherwise, look for any intersection points between sector limits
C and row and determine X limits accordingly
               ELSE
C
C Look at each sector limit in turn
                  DO 70 I=1,2
C
C See if row crosses sector limit
                     IF ((Y.GT.0.0.AND.Y.LE.YR(I)).OR.
     +                   (Y.LT.0.0.AND.Y.GE.YR(I))) THEN
C
C Determine X pixel coordinate of crossing point
                        X=X0+Y*XR(I)/YR(I)
C
C Determine whether this is an upper or lower X limit and round inwards
                        IF ((I.EQ.1.AND.YR(I).LT.0.0).OR.
     +                      (I.EQ.2.AND.YR(I).GT.0.0)) THEN
C
C Lower X limit (round upwards)
                           X1=X
                           I1=INT(X1)
                           IF (REAL(I1).LT.X1) I1=I1+1
C
C Otherwise, upper X limit (round downwards)
                        ELSE
                           X2=X
                           I2=INT(X2)
                           IF (REAL(I2).GT.X2) I2=I2-1
                        ENDIF
                     ENDIF
   70             CONTINUE
               ENDIF
            ENDIF
C
C If X limits reversed, row overlaps sector in two distinct places
            IF (X2.LT.X1) THEN
C
C Sum pixels in two passes
               CALL SECTN3(1,I2,X0,YY,IRMAX)
               CALL SECTN3(I1,NCOL,X0,YY,IRMAX)
C
C Otherwise, sum pixels in one pass
            ELSE
               CALL SECTN3(I1,I2,X0,YY,IRMAX)
            ENDIF
   80    CONTINUE
C
C Normalize sum and recombine into output row buffer
         I=1
         DO 90 IR=0,IRMAX
C
C Divide sum by number of contributing pixels (if any)
            IF (B5(IR).NE.0.0) THEN
               B3(IR)=B3(IR)/B5(IR)
               B4(IR)=B4(IR)/B5(IR)
            ENDIF
C
C Copy real and imaginary parts into output row buffer
            RB2(I)=B3(IR)
            RB2(I+1)=B4(IR)
            I=I+2
   90    CONTINUE
      ENDIF
C
C Store result in LP2
      IF (SEMROW(2,RB2,NFMCOM,1,1,LP2)) RETURN
C
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module SECTN2
C
      SUBROUTINE SECTN2(XR,YR,RR,R,C,S,XMIN,XMAX,YMIN,YMAX)
C
C Returns clipped end position (XR,YR) and length RR of radius R, in
C direction (C,S).  Clipping limits in XMIN,XMAX,YMIN,YMAX.
C
      REAL XR,YR,RR,R,C,S,XMIN,XMAX,YMIN,YMAX
C
C Calculate unclipped end position
      XR=R*C
      YR=R*S
C
C Clip radius in X direction
      IF (XR.LT.XMIN) THEN
         XR=XMIN
         YR=XMIN*S/C
      ELSE IF (XR.GT.XMAX) THEN
         XR=XMAX
         YR=XMAX*S/C
      ENDIF
C
C Clip radius in Y direction
      IF (YR.LT.YMIN) THEN
         XR=YMIN*C/S
         YR=YMIN
      ELSE IF (YR.GT.YMAX) THEN
         XR=YMAX*C/S
         YR=YMAX
      ENDIF
C
C Determine length of clipped radius
      RR=SQRT(XR*XR+YR*YR)
C
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module SECTN3
C
      SUBROUTINE SECTN3(I1,I2,X0,YY,IRMAX)
C
C Adds real and imaginary contributions of pixels in row buffer RB1
C to summation buffers RB3 and RB4.  Pixels I1 to I2 are processed.
C Contributions are distributed according to each pixel's distance
C from the section's centre position.  The pixel values are divided
C in proportion to the pixels' proximity to adjacent output pixels.
C A count of the number of pixels contributing to each output pixel
C is kept in RB5 for later averaging.
C
      INTEGER I1,I2,IRMAX
      REAL X0,YY
C
      INCLUDE 'COMMON'
C
      REAL B3(0:255),B4(0:255),B5(0:255)
      EQUIVALENCE (B3,RB3),(B4,RB4),(B5,RB5)
      REAL X,XX,R,DR,CR
      INTEGER IR,I
C
C Initialise X sector coordinate
      X=REAL(I1-1)-X0
C
C Process source pixels
      DO 10 I=2*I1-1,2*I2-1,2
C
C Increment X sector coordinate
         X=X+1.0
         XX=X*X
C
C Determine distance (radius) from sector centre position
         R=SQRT(XX+YY)
C
C Add pixels contribution to section
         IR=INT(R)
         IF (IR.GE.IRMAX) THEN
            B3(IRMAX)=B3(IRMAX)+RB1(I)
            B4(IRMAX)=B4(IRMAX)+RB1(I+1)
            B5(IRMAX)=B5(IRMAX)+1.0
         ELSE
            DR=R-REAL(IR)
            CR=1.0-DR
            B3(IR)=B3(IR)+CR*RB1(I)
            B4(IR)=B4(IR)+CR*RB1(I+1)
            B5(IR)=B5(IR)+CR
            IR=IR+1
            B3(IR)=B3(IR)+DR*RB1(I)
            B4(IR)=B4(IR)+DR*RB1(I+1)
            B5(IR)=B5(IR)+DR
         ENDIF
   10 CONTINUE
C
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
