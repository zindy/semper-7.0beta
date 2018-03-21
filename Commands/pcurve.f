C Semper 6 subsidiary module PCURVE
C
      SUBROUTINE PCURVE
C
C Given a closed curve suplied in the form of a position list,
C calculates equivalent particle parameters, taking area enclosed
C by the curve as the area representing a particle.  If a source
C picture is specified by means of the IMAGE key, the minimum,
C maximum, integrated and mean intensity and the associated standard
C deviation, centres of mass, moments of mass and orientation are also
C calculated.  The results are printed on the terminal (unless option
C NOVERIFY is set) and returned in the variables XR,YR,X1,X2,Y1,Y2,HF,
C VF,AF,BF,HP,VP,P,A,C,NP,XC,YC,M1,M2,THETA and, if the IMAGE key is
C set in variables I1,I2,SI,MI,SDI,XCM,YCM, MM1,MM2,PHI,I1,I2 and SDI.
C
C Some parameters are calculated directly from the positions defining
C the vertices of the closed curve.  The remainder are calculated by
C summation over a grid of points which coincide with pixel positions
C in the source picture (if any) and which lie inside the closed curve.
C For compatibility with the MASK command, the pixel coordinates have
C the X axis from left to right and the Y axis running from top to
C bottom with the top left source pixel at (1,1).  The final results
C are evaluated with respect to normal picture coordinates.
C
      LOGICAL VARSET,OPTNO,SEMOPN,SEMROW,SEMLU,SEMCON,SEMDIA
      INTEGER IVALPN
C
      REAL MEANI,SDI,MINI,MAXI,MAMIN,MAMAX,MMMIN,MMMAX
      REAL X,Y,XREF,YREF,XMIN,XMAX,YMIN,YMAX,AMIN,AMAX,BMIN,BMAX
      REAL HPROJ,VPROJ,PERIM,AREA,XN,YN,XLOW,YLOW,XHIG,YHIG,HFERET
      REAL VFERET,AFERET,BFERET,CIRCUL,SUM,SUMX,SUMY,SUMXX,SUMYY,SUMXY
      REAL SUMI,SUMII,SUMIX,SUMIY,SUMIXX,SUMIYY,SUMIXY,RSUMX,RSUMXX
      REAL SUM1,SUM2,RSUM,RSMI,RSMII,RSMIX,RSMIXX,PIXEL,XCEN,YCEN,SUMRR
      REAL PX,PY,P,THETA,XCM,YCM,SUMIRR,PHI,X1,X2,IMIN,IMAX,JMIN,JMAX
      INTEGER I,I1,I2,J,J1,J2,NPIC,NCOL,NROW,NLAY,N,ISUM
      INTEGER ICHORD,NCHORD,NV,IX1,IX2,CLASS,FORM,CCOL,CROW,IREF,JREF
      LOGICAL LIMAGE,LNZERO,LVERIF
C
      INTEGER NPPAR
      PARAMETER (NPPAR=31)
C
      INTEGER NVAR(NPPAR)
      REAL    VALU(NPPAR)
C
      INCLUDE 'COMMON'
C
      INTEGER ICBUFF((LNBUF/LNINT)/2,2)
C
      EQUIVALENCE (ICBUFF,RB2)
C
      REAL CMIN,CMAX
      PARAMETER (CMIN=-32000.0, CMAX=32000.0)
C
C Packed names
C
      INTEGER NWITH,NIMAGE,NVERIF,NXR,NYR,NX1,NX2,NY1,NY2,NHF,NVF,NAF
      INTEGER NBF,NHP,NVP,NP,NA,NC,NNP,NXC,NYC,NM1,NM2,NTHETA,NSI,NMI
      INTEGER NXCM,NYCM,NMM1,NMM2,NPHI,NI1,NI2,NSDI
      PARAMETER (NWITH=-5181, NIMAGE=14921, NVERIF=-3419)
      PARAMETER (NXR=-7121, NYR=-8721, NX1=-7641, NX2=-7681)
      PARAMETER (NY1=-9241, NY2=-9281, NHF=13040, NVF=-3441)
      PARAMETER (NAF=1840, NBF=3440, NHP=13440, NVP=-3841)
      PARAMETER (NP=25600, NA=1600, NC=4800, NNP=23040, NXC=-6521)
      PARAMETER (NYC=-8121, NM1=22040, NM2=22080, NTHETA=-326)
      PARAMETER (NSI=30760, NMI=21160, NXCM=-6534, NYCM=-8134)
      PARAMETER (NMM1=21351, NMM2=21352, NPHI=25929)
      PARAMETER (NI1=15640, NI2=15680, NSDI=30569)
C
      DATA NVAR / NXR,NYR,NX1,NX2,NY1,NY2,NHF,NVF,NAF,NBF,NHP,NVP,
     +            NP,NA,NC,NNP,NXC,NYC,NM1,NM2,NTHETA,NSI,NMI,
     +            NXCM,NYCM,NMM1,NMM2,NPHI,NI1,NI2,NSDI/
C
C See if key IMAGE is set
C
      LIMAGE=VARSET(NIMAGE)
C
C If so, open source picture
C
      IF (LIMAGE) THEN
C
C Open source picture
C
         NPIC=IVALPN(NIMAGE)
         IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP1)) GOTO 90
C
C Fetch source picture centre position
C
         CCOL=CCOLN(LP1)
         CROW=CROWN(LP1)
C
C Otherwise, centre position defaults to origin
C
      ELSE
         CCOL=0
         CROW=0
      ENDIF
C
C Open picture containing curve data
C
      NPIC=IVALPN(NWITH)
      IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP2)) GOTO 90
C
C Fault non-plist picture class for picture containing curve data
C
      IF (CLASSN(LP2).NE.NCLPLI) THEN
         ERROR=6
         IDERR=NPIC
         GOTO 90
      ENDIF
C
C Read X and Y data from position list
C
      IF (SEMROW(1,RB1,NFMFP,1,1,LP2)) GOTO 90
      IF (SEMROW(1,RB2,NFMFP,1,2,LP2)) GOTO 90
C
C Fetch first point of curve
C
      X=RB1(1)
      Y=RB2(1)
C
C Initialise particle reference point
C
      XREF=X
      YREF=Y
C
C Initialise curve limits
C
      XMIN=X
      XMAX=X
      YMIN=Y
      YMAX=Y
C
C Initialise feret limits
C
      AMIN=X+Y
      AMAX=X+Y
      BMIN=X-Y
      BMAX=X-Y
C
C Initialise horizontal and vertical projection sums
C
      HPROJ=0.0
      VPROJ=0.0
C
C Initialise perimeter and area sums
C
      PERIM=0.0
      AREA=0.0
C
C Repeat first point at the end of the position list
C
      RB1(NCOL+1)=X
      RB2(NCOL+1)=Y
C
C Process curve data
C
      DO 10 I=1,NCOL
C
C Fetch position of current point on curve
C
         X=RB1(I)
         Y=RB2(I)
C
C Update particle reference point if necessary
C
         IF (Y.LT.YREF) THEN
            XREF=X
            YREF=Y
         ELSE IF (Y.EQ.YREF) THEN
            IF (X.GT.XREF) THEN
               XREF=X
               YREF=Y
            ENDIF
         ENDIF
C
C Update curve limits
C
         XMIN=MIN(XMIN,X)
         XMAX=MAX(XMAX,X)
         YMIN=MIN(YMIN,Y)
         YMAX=MAX(YMAX,Y)
C
C Update feret limits
C
         AMIN=MIN(AMIN,X+Y)
         AMAX=MAX(AMAX,X+Y)
         BMIN=MIN(BMIN,X-Y)
         BMAX=MAX(BMAX,X-Y)
C
C Fetch position of next point on curve
C
         XN=RB1(I+1)
         YN=RB2(I+1)
C
C Increment horizontal and vertical projection sums
C
         HPROJ=HPROJ+DIM(Y,YN)
         VPROJ=VPROJ+DIM(X,XN)
C
C Increment perimeter sum
C
         PERIM=PERIM+SQRT((XN-X)*(XN-X)+(YN-Y)*(YN-Y))
C
C Increment area sum
C
         AREA=AREA+(X*YN-XN*Y)
C
C Convert end points to pixel coordinates
C
         X=REAL(CCOL)+X
         Y=REAL(CROW)-Y
C
         XN=REAL(CCOL)+XN
         YN=REAL(CROW)-YN
C
C Decide which is top and bottom end point (in Y pixel direction)
C
         IF (Y.LT.YN) THEN
            XLOW=X
            YLOW=Y
            XHIG=XN
            YHIG=YN
         ELSE
            XLOW=XN
            YLOW=YN
            XHIG=X
            YHIG=Y
         ENDIF
C
C Store parameters for scan conversion
C
         RB3(I)=YLOW
         RB4(I)=YHIG
         RB5(I)=XHIG-XLOW
         RB6(I)=XLOW*YHIG-XHIG*YLOW
   10 CONTINUE
C
C Determine final values for feret diameters
C
      HFERET=XMAX-XMIN
      VFERET=YMAX-YMIN
      AFERET=(AMAX-AMIN)/SQRT(2.0)
      BFERET=(BMAX-BMIN)/SQRT(2.0)
C
C Determine final value for curve area (note that this value can only
C be correct for a curve that does not intersect itself)
C
      AREA=ABS(AREA)/2.0
C
C Determine circularity
C
      CIRCUL=4.0*PI*AREA/(PERIM*PERIM)
C
C See if results are to be printed, according to the option NOVERIFY
C
      LVERIF=.NOT.OPTNO(NVERIF)
C
C Print curve-based results (unless option NOVERIFY is set)
C
      IF (LVERIF) THEN
         IF (SEMCON(' ')) GOTO 90
         WRITE (RECORD,100) XREF,YREF
         IF (SEMCON(RECORD)) GOTO 90
         WRITE (RECORD,110) XMIN,XMAX,YMIN,YMAX
         IF (SEMCON(RECORD)) GOTO 90
         WRITE (RECORD,120) HFERET,VFERET,AFERET,BFERET
         IF (SEMCON(RECORD)) GOTO 90
         WRITE (RECORD,130) HPROJ,VPROJ
         IF (SEMCON(RECORD)) GOTO 90
         WRITE (RECORD,140) PERIM
         IF (SEMCON(RECORD)) GOTO 90
         WRITE (RECORD,150) AREA
         IF (SEMCON(RECORD)) GOTO 90
         WRITE (RECORD,160) CIRCUL
         IF (SEMCON(RECORD)) GOTO 90
      ENDIF
C
C Set up array of return values
C
      VALU(1)=XREF
      VALU(2)=YREF
      VALU(3)=XMIN
      VALU(4)=XMAX
      VALU(5)=YMIN
      VALU(6)=YMAX
      VALU(7)=HFERET
      VALU(8)=VFERET
      VALU(9)=AFERET
      VALU(10)=BFERET
      VALU(11)=HPROJ
      VALU(12)=VPROJ
      VALU(13)=PERIM
      VALU(14)=AREA
      VALU(15)=CIRCUL
      NV=15
C
C Initialise overall sums
C
      SUM=0.0
      SUMX=0.0
      SUMY=0.0
      SUMXX=0.0
      SUMYY=0.0
      SUMXY=0.0
C
C Convert curve limits to pixel coordinates
C
      IMIN=REAL(CCOL)+XMIN
      IMAX=REAL(CCOL)+XMAX
C
      JMIN=REAL(CROW)-YMAX
      JMAX=REAL(CROW)-YMIN
C
C See if IMAGE key is set
C
      IF (LIMAGE) THEN
C
C Fetch source picture size
C
         NCOL=NCOLS(LP1)
         NROW=NROWS(LP1)
C
C Fault limits of position list if they exceed the picture limits
C
         IF (IMIN.LT.1.0.OR.IMAX.GT.REAL(NCOL).OR.
     +       JMIN.LT.1.0.OR.JMAX.GT.REAL(NROW)) THEN
            ERROR=9
            GOTO 90
         ENDIF
C
C Convert curve reference point position to nearest pixel position
C
         IREF=CCOL+NINT(XREF)
         JREF=CROW-NINT(YREF)
C
C Read source row containing curve reference point
C
         IF (SEMROW(1,RB1,NFMFP,JREF,1,LP1)) GOTO 90
C
C Use pixel value at reference point to initialise intensity range
C
         MINI=RB1(IREF)
         MAXI=RB1(IREF)
C
C Initialise extra overall sums
C
         SUMI=0.0
         SUMII=0.0
         SUMIX=0.0
         SUMIY=0.0
         SUMIXX=0.0
         SUMIYY=0.0
         SUMIXY=0.0
C
C Otherwise, just fault excessively large limits for position list
C
      ELSE
         IF (IMIN.LT.CMIN.OR.IMAX.GT.CMAX.OR.
     +       JMIN.LT.CMIN.OR.JMAX.GT.CMAX) THEN
            ERROR=156
            GOTO 90
         ENDIF
      ENDIF
C
C Initialise non-zero area flag
C
      LNZERO=.FALSE.
C
C Determine integer Y range spanned by position list (rounding inwards)
C
      J1=INT(JMIN)
      IF (REAL(J1).LT.JMIN) J1=J1+1
C
      J2=INT(JMAX)
      IF (REAL(J2).GT.JMAX) J2=J2-1
C
C Process scan lines
C
      DO 70 J=J1,J2
C
         Y=REAL(CROW-J)
C
C Find all intersection points between current scan line and curve
C
         N=0
         DO 20 I=1,NCOLS(LP2)
C
C Add intersection X value to list if curve crosses scan line
C
            IF (RB3(I).LT.REAL(J).AND.RB4(I).GE.REAL(J)) THEN
               N=N+1
               RB1(N)=(RB5(I)*REAL(J)+RB6(I))/(RB4(I)-RB3(I))
            ENDIF
   20    CONTINUE
C
C Sort X intersection values
C
         CALL PCURV2(RB1,N)
C
C Initialise chord count
C
         NCHORD=0
C
C Determine integer limits of each chord
C
         DO 30 I=1,N-1,2
C
C Fetch chord limits
C
            X1=RB1(I)
            X2=RB1(I+1)
C
C Round inwards to next pixel
C
            I1=INT(X1)
            IF (REAL(I1).LT.X1) I1=I1+1
C
            I2=INT(X2)
            IF (REAL(I2).GT.X2) I2=I2-1
C
C If chord overlaps or touches previous chord, extend previous chord
C to end of this chord
C
            IF (NCHORD.GE.1) THEN
               IF (I1.LE.ICBUFF(NCHORD,2)+1) THEN
                  ICBUFF(NCHORD,2)=I2
                  GOTO 30
               ENDIF
            ENDIF
C
C Otherwise, store chord limits
C
            NCHORD=NCHORD+1
            ICBUFF(NCHORD,1)=I1
            ICBUFF(NCHORD,2)=I2
   30    CONTINUE
C
C Initialise row sums
C
         ISUM=0
         RSUMX=0.0
         RSUMXX=0.0
C
C Process chords
C
         DO 40 ICHORD=1,NCHORD
C
C Fetch start and end X values for chord, converting from pixel to
C picture coordinates
C
            IX1=ICBUFF(ICHORD,1)-CCOL
            IX2=ICBUFF(ICHORD,2)-CCOL
C
C Evaluate row sums
C
            ISUM=ISUM+IX2-IX1+1
            SUM1=REAL(IX1)*REAL(IX1-1)
            SUM2=REAL(IX2)*REAL(IX2+1)
            RSUMX=RSUMX+SUM2-SUM1
            RSUMXX=RSUMXX+SUM2*REAL(IX2+IX2+1)-SUM1*REAL(IX1+IX1-1)
   40    CONTINUE
C
         RSUM=REAL(ISUM)
C
C Update overall sums
C
         SUM=SUM+RSUM
         SUMX=SUMX+RSUMX
         SUMY=SUMY+RSUM*Y
         SUMXX=SUMXX+RSUMXX
         SUMYY=SUMYY+RSUM*Y*Y
         SUMXY=SUMXY+RSUMX*Y
C
C If non-zero row sum, set flag
C
         IF (ISUM.NE.0) LNZERO=.TRUE.
C
C See if source picture specified
C
         IF (LIMAGE) THEN
C
C Read corresponding row from source picture
C
            IF (SEMROW(1,RB1,NFMFP,J,1,LP1)) GOTO 90
C
C Initialise extra row sums
C
            RSMI=0.0
            RSMII=0.0
            RSMIX=0.0
            RSMIXX=0.0
C
C Process chords
C
            DO 60 ICHORD=1,NCHORD
C
C Evaluate extra row sums
C
               DO 50 I=ICBUFF(ICHORD,1),ICBUFF(ICHORD,2)
C
                  X=REAL(I-CCOL)
C
                  PIXEL=RB1(I)
C
                  MINI=MIN(PIXEL,MINI)
                  MAXI=MAX(PIXEL,MAXI)
C
                  RSMI=RSMI+PIXEL
                  RSMII=RSMII+PIXEL*PIXEL
                  RSMIX=RSMIX+PIXEL*X
                  RSMIXX=RSMIXX+PIXEL*X*X
   50          CONTINUE
   60       CONTINUE
C
C Update overall sums
C
            SUMI=SUMI+RSMI
            SUMII=SUMII+RSMII
            SUMIX=SUMIX+RSMIX
            SUMIY=SUMIY+RSMI*Y
            SUMIXX=SUMIXX+RSMIXX
            SUMIYY=SUMIYY+RSMI*Y*Y
            SUMIXY=SUMIXY+RSMIX*Y
         ENDIF
   70 CONTINUE
C
C If non-zero area flag is set, particle sums are defined
C
      IF (LNZERO) THEN
C
C Make final adjustment to overall sums
C
         SUMX=SUMX/2.0
         SUMXX=SUMXX/6.0
         SUMXY=SUMXY/2.0
C
C Determine centre of area
C
         XCEN=SUMX/SUM
         YCEN=SUMY/SUM
C
C Determine second moments about centre of area
C
         SUMXX=SUMXX/SUM-XCEN*XCEN
         SUMYY=SUMYY/SUM-YCEN*YCEN
         SUMXY=SUMXY/SUM-XCEN*YCEN
C
C Determine principal second moments of area
C
         SUMRR=SUMXX+SUMYY
         PX=(SUMXX-SUMYY)/2.0
         PY=SUMXY
         P=SQRT(PX*PX+PY*PY)
         MAMIN=SUMRR/2.0-P
         MAMAX=SUMRR/2.0+P
C
C Determine orientation of long axis (axis with least moment)
C
         IF (P.EQ.0.0) THEN
            THETA=0.0
         ELSE
            THETA=ATAN2(PY,PX)/2.0
         ENDIF
C
C Print results (unless option NOVERIFY is set)
C
         IF (LVERIF) THEN
            IF (SEMCON(' ')) GOTO 90
            WRITE (RECORD,170) SUM
            IF (SEMCON(RECORD)) GOTO 90
            WRITE (RECORD,180) XCEN,YCEN
            IF (SEMCON(RECORD)) GOTO 90
            WRITE (RECORD,190) MAMIN,MAMAX
            IF (SEMCON(RECORD)) GOTO 90
            WRITE (RECORD,200) (180.0/PI)*THETA
            IF (SEMCON(RECORD)) GOTO 90
         ENDIF
C
C Set up array of return values
C
         VALU(16)=SUM
         VALU(17)=XCEN
         VALU(18)=YCEN
         VALU(19)=MAMIN
         VALU(20)=MAMAX
         VALU(21)=THETA
         NV=21
C
C If source picture specified, determine centre of mass, principal
C moments of mass and correspoding orientation angle
C
         IF (LIMAGE) THEN
C
C Determine mean intensity
C
            MEANI=SUMI/SUM
C
C Determine standard deviation for intensity
C
            SDI=SQRT(MAX(SUMII/SUM-MEANI*MEANI,0.0))
C
C Determine centre of mass
C
            XCM=SUMIX/SUMI
            YCM=SUMIY/SUMI
C
C Determine second moments about centre of mass
C
            SUMIXX=SUMIXX/SUMI-XCM*XCM
            SUMIYY=SUMIYY/SUMI-YCM*YCM
            SUMIXY=SUMIXY/SUMI-XCM*YCM
C
C Determine principal second moments of mass
C
            SUMIRR=SUMIXX+SUMIYY
            PX=(SUMIXX-SUMIYY)/2.0
            PY=SUMIXY
            P=SQRT(PX*PX+PY*PY)
            MMMIN=SUMIRR/2.0-P
            MMMAX=SUMIRR/2.0+P
C
C Determine orientation of long axis (axis with least moment)
C
            IF (P.EQ.0.0) THEN
               PHI=0.0
            ELSE
               PHI=ATAN2(PY,PX)/2.0
            ENDIF
C
C Print results (unless option NOVERIFY is set)
C
            IF (LVERIF) THEN
               IF (SEMCON(' ')) GOTO 90
               WRITE (RECORD,210) MINI,MAXI
               IF (SEMCON(RECORD)) GOTO 90
               WRITE (RECORD,220) SUMI
               IF (SEMCON(RECORD)) GOTO 90
               WRITE (RECORD,230) MEANI
               IF (SEMCON(RECORD)) GOTO 90
               WRITE (RECORD,240) SDI
               IF (SEMCON(RECORD)) GOTO 90
               WRITE (RECORD,250) XCM,YCM
               IF (SEMCON(RECORD)) GOTO 90
               WRITE (RECORD,260) MMMIN,MMMAX
               IF (SEMCON(RECORD)) GOTO 90
               WRITE (RECORD,270) (180.0/PI)*PHI
               IF (SEMCON(RECORD)) GOTO 90
            ENDIF
C
C Set up array of return values
C
            VALU(22)=SUMI
            VALU(23)=MEANI
            VALU(24)=XCM
            VALU(25)=YCM
            VALU(26)=MMMIN
            VALU(27)=MMMAX
            VALU(28)=PHI
            VALU(29)=MINI
            VALU(30)=MAXI
            VALU(31)=SDI
            NV=31
         ENDIF
C
C Set return variables XR,YR,X1,X2,Y1,Y2,HF,VF,AF,BF,HP,VP,P,A,C,
C NP,XC,YC,M1,M2,THETA,I1,I2,SI,MI,SDI,XCM,YCM,MM1,MM2 and PHI if
C return value is defined
C
         DO 80 I=1,NV
            IF (SEMLU(1,NVAR(I),VALU(I))) GOTO 90
   80    CONTINUE
C
C If curve and pixel areas do not agree, print warning (symptom of
C self-intersecting curve)
C
         IF (ABS(AREA-SUM)/SUM.GT.0.1) THEN
            IF (SEMDIA(
     + 'Curve and pixel areas do not agree (curve intersects itself?)',
     +         NDIWAR)) GOTO 90
         ENDIF
C
C Otherwise, fault undefined particle sums
C
      ELSE
         ERROR=159
      ENDIF
C
   90 RETURN
C
  100 FORMAT ('Reference point   ',2F15.2)
  110 FORMAT ('Particle limits   ',4F15.2)
  120 FORMAT ('Feret diameters   ',4F15.2)
  130 FORMAT ('H,V projections   ',2F15.2)
  140 FORMAT ('Perimeter         ',F15.2)
  150 FORMAT ('Curve area        ',F15.2)
  160 FORMAT ('Circularity       ',F15.5)
  170 FORMAT ('Pixel area        ',F15.2)
  180 FORMAT ('Centre of area    ',2F15.2)
  190 FORMAT ('Min,max 2nd moment',2E15.6)
  200 FORMAT ('Angle (degrees)   ',F15.2)
  210 FORMAT ('Intensity range   ',2F15.2)
  220 FORMAT ('Total intensity   ',E15.6)
  230 FORMAT ('Mean intensity    ',F15.2)
  240 FORMAT ('Standard deviation',F15.2)
  250 FORMAT ('Centre of mass    ',2F15.2)
  260 FORMAT ('Min,max 2nd moment',2E15.6)
  270 FORMAT ('Angle (degrees)   ',F15.2)
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module PCURV2
C
      SUBROUTINE PCURV2(VALUE,N)
C
C Sorts N floating-point values in array VALUE.  Sort is done in place
C using straightforward insertion sort which is very efficient for
C sorting a small number of values.
C
      REAL VALUE(*)
      INTEGER N
C
      INTEGER I,J
      REAL V
C
      DO 20 I=2,N
C
         J=I-1
         V=VALUE(I)
C
   10    IF (V.LT.VALUE(J)) THEN
C
            VALUE(J+1)=VALUE(J)
            J=J-1
C
            IF (J.GT.0) GOTO 10
         ENDIF
C
         VALUE(J+1)=V
C
   20 CONTINUE
C
      RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
