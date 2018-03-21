C Semper 6 processing module PDRAW
C
      SUBROUTINE PDRAW
C
C Inputs curve directly from the cursor and resets pixels, in the
C specified source picture, that lie along the curve.  The new pixel
C value is specified by the key VALUE.  Each section of the curve is
C verified on the display, unless option NOVERIFY is set.  The keys
C POSITION and PO2 specify a start position for the cursor.
C
C Cursor input is terminated by inputting the same point twice
C (within a tolerance determined by parameter TOLSQD).
C
C Slightly more pixels are reset than is normally required for straight
C line segments in order to avoid any diagonal steps between pixels.
C This will force ANALYSE to differentiate between particles on either
C side of the curve.
C
      REAL VAL
      INTEGER IPACK,IVAL,IVALPN
      LOGICAL FSXWIR
      LOGICAL FSINIT,FSLINE,FSFLUS,FSCLIP
      LOGICAL VARSET,SEMOPN,SEMROW,OPTNO
C
      INCLUDE 'COMMON'
C
      REAL TOLSQD
      PARAMETER (TOLSQD=2.5)
C
      REAL DX,DY,SLOPE,VALUE,X(2),X0,XCLIP(2),Y(2),Y0,Y2,YCLIP(2)
      INTEGER CLASS,CCOL,CROW,FORM,I,I1,I2,IMAX,IMIN,J,J1,J2,J3
      INTEGER NCOL,NLAY,NPIC,NROW
      LOGICAL LVERIF,LSTEEP
C
C If key IMAGE is set to zero, picture number for source picture
C is obtained from key PIMAGE
C
      IF (IVAL(14921).EQ.0) THEN
C
C If key PIMAGE is set, use it to determine picture number
C
         IF (VARSET(25973)) THEN
            NPIC=IVALPN(25973)
C
C Otherwise, fault its being unset
C
         ELSE
            ERROR=25
            IDERR=25973
            GOTO 50
         ENDIF
C
C Otherwise, use key value to determine picture number
C
      ELSE
         NPIC=IVALPN(14921)
      ENDIF
C
C Open source picture
C
      IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP1)) GOTO 50
C
C Initialise display graphics
C
      IF (FSINIT(3,IVALPN(25963))) GOTO 50
C
C Fault display picture which is not a 2-D image
C
      IF (FSPTYP.NE.1) THEN
         ERROR=157
         GOTO 50
      ENDIF
C
C Fetch source centre position
C
      CCOL=CCOLN(LP1)
      CROW=CROWN(LP1)
C
C Fetch value for key VALUE
C
      VALUE=VAL(-3253)
C
C See if verification of cursor input required (option NOVERIFY absent)
C
      LVERIF=.NOT.OPTNO(-3419)
C
C Fetch initial cursor start position from keys POSITION and PO2
C
      X0=VAL(26219)
      Y0=VAL(26232)
C
C Obtain first cursor position
C
      CALL FSXS61(0)
      IF (FSXWIR(X0,Y0,X(1),Y(1))) GOTO 50
   10 CONTINUE
C
C
C Set line style
C
      CALL FSXS61(1)
C
C Obtain next cursor position
C
      IF (FSXWIR(X(1),Y(1),X(2),Y(2))) GOTO 50
C
C Determine X and Y pixel increments between two cursor positions
C
      DX=FSXSCA*(X(2)-X(1))
      DY=FSYSCA*(Y(2)-Y(1))
C
C Return if distance between two cursor positions is small enough
C
      IF (DX*DX+DY*DY.LT.TOLSQD) GOTO 50
C
C Verify cursor input if necessary
C
      IF (LVERIF) THEN
C
C Draw line between two cursor positions
C
         IF (FSLINE(X(1),Y(1),X(2),Y(2))) GOTO 50
C
C Flush contents of graphics buffer
C
         IF (FSFLUS()) GOTO 50
      ENDIF
C
C Set up picture X and Y limits
C
      XCLIP(1)=REAL(1-CCOL)
      XCLIP(2)=REAL(NCOL-CCOL)
C
      YCLIP(1)=REAL(CROW-NROW)
      YCLIP(2)=REAL(CROW-1)
C
C Clip vector in X and Y directions
C
      IF (FSCLIP(X,Y,XCLIP)) GOTO 40
      IF (FSCLIP(Y,X,YCLIP)) GOTO 40
C
C Convert X start and end positions to nearest column positions
C
      I1=CCOL+NINT(X(1))
      I2=CCOL+NINT(X(2))
C
C Set up limits for column numbers
C
      IMIN=MIN(I1,I2)
      IMAX=MAX(I1,I2)
C
C Convert Y start and end positions to nearest row positions
C
      J1=CROW-NINT(Y(1))
      J2=CROW-NINT(Y(2))
C
C Set up pixel increment in row direction
C
      IF (J1.LE.J2) THEN
         J3=1
      ELSE
         J3=-1
      ENDIF
C
C Set up vector increments
C
      DX=X(2)-X(1)
      DY=Y(2)-Y(1)
C
C Set up flag for absolute slope greater than 1
C
      LSTEEP=ABS(DY).GT.ABS(DX)
C
C Set up vector slope value
C
      IF (J1.NE.J2) THEN
         SLOPE=DX/DY
      ELSE
         SLOPE=0.0
      ENDIF
C
C Scan through source picture rows
C
      DO 30 J=J1,J2,J3
C
C If large absolute slope, intersection is between picture row
C and vector
C
         IF (LSTEEP) THEN
            Y2=REAL(CROW-J)
C
C Otherwise, intersection is between vector and line half pixel
C above/below current row
C
         ELSE
            Y2=REAL(CROW)-REAL(J+J+J3)/2.0
         ENDIF
C
C Determine corresponding closest column number
C
         I2=CCOL+NINT(X(2)+SLOPE*(Y2-Y(2)))
C
C Read row from source picture
C
         IF (SEMROW(1,RB1,NFMFP,J,1,LP1)) GOTO 50
C
C Reset pixel values
C
         DO 20 I=MAX(MIN(I1,I2),IMIN),MIN(MAX(I1,I2),IMAX)
            RB1(I)=VALUE
   20    CONTINUE
C
C Write row back into source picture
C
         IF (SEMROW(2,RB1,NFMFP,J,1,LP1)) GOTO 50
C
C Last pixel position becomes first pixel position for next row
C
         I1=I2
   30 CONTINUE
C
C Last cursor position becomes previous cursor position
C
   40 X(1)=X(2)
      Y(1)=Y(2)
C
C Go back for more
C
      GOTO 10
C
   50 RETURN
C
C Copyright (C) 1987-1994:  Synoptics Ltd,  All Rights Reserved
C
      END
