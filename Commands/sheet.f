C Semper 6 processing module SHEET
C
      SUBROUTINE SHEET
C
C Generates shaded image of surface defined by 2-D array of Z values
C contained in the source picture.
C
C     Change: LDM, July 2005
C     Ensure that gfortran does not think of range, unpack as intrinsics
      EXTERNAL RANGE, UNPACKF
      LOGICAL RANGE,VARSET,SEMOPN,SEMCEN,SEMROW,SHEET2,SHEET3,SHEET4
      REAL VAL
      INTEGER IVALPN
C
      INCLUDE 'COMMON'
C
      LOGICAL LBORDE,LRANGE,EVEN
      INTEGER CCOL,CROW,CCOLT,CROWT
      REAL LTHETA,LPHI,XLIM(2),YLIM(2),ZLIM(2),MAINL
      REAL L1,L2,L3,M1,M2,M3,N1,N2,N3,NE,NL
C
      REAL RB1EXT(1-LNEDGE:LNBUF/LNREAL+LNEDGE)
      REAL RB2EXT(1-LNEDGE:LNBUF/LNREAL+LNEDGE)
C
      REAL T(9),T11,T12,T13,T21,T22,T23,T31,T32,T33
      EQUIVALENCE (RB1EXT(1),RB1),(RB2EXT(1),RB2)
      EQUIVALENCE (T11,T(1)),(T12,T(2)),(T13,T(3))
      EQUIVALENCE (T21,T(4)),(T22,T(5)),(T23,T(6))
      EQUIVALENCE (T31,T(7)),(T32,T(8)),(T33,T(9))
      REAL BORDER,R1,R2,TIMES,SDR,THETA,PSI,E1,E2,E3,XT,YT,ZT,D
      REAL Z1,Z2,ZMIN,ZMAX,Z0,ZM,ZC,XMIN,XMAX,YMIN,YMAX,AMBIEN,DCONT
      REAL FORWL,SPECUL,DIFFUS,VALUE,YS,XS,Z,COSNE,B,COSNL,COSNM,ZS,BR
      INTEGER NCOL,NROW,I1,I2,I3,J1,J2,J3,K3,I,K,J,N,IZ1,IZ2,K1,K2
      INTEGER IXMIN,IXMAX,IYMIN,IYMAX,NCOLT,NROWT,LPTEMP,JB1,JB2
C
C Packed names
C
      INTEGER NTHETA,NPSI,NLTHET,NLPHI,NFROM,NTIMES,NVALUE,NAMBIE
      INTEGER NMAIN,NFORWA,NSDR,NDCONT,NRANGE,NRA2,NBORDE,NZORIG
      PARAMETER (NTHETA=-326, NPSI=26369, NLTHET=20008, NLPHI=19848)
      PARAMETER (NFROM=10335, NTIMES=-374, NVALUE=-3253, NAMBIE=2122)
      PARAMETER (NMAIN=20849, NFORWA=10218, NSDR=30578, NDCONT=6535)
      PARAMETER (NRANGE=28854, NRA2=28872, NBORDE=3818, NZORIG=-10219)
C
C Fault multi-layer source picture
C
      IF (NLAYS(LP1).NE.1) THEN
         ERROR=5
         IDERR=IVALPN(NFROM)
         RETURN
      ENDIF
C
C See if key BORDER is set
C
      LBORDE=VARSET(NBORDE)
      BORDER=VAL(NBORDE)
C
C See if Z coordinate range specified with keys RANGE and RA2
C
      LRANGE=VARSET(NRANGE)
      R1=VAL(NRANGE)
      R2=VAL(NRA2)
C
C Determine final magnification factor using key TIMES
C Note: Extra factor of 1.5 required to reverse effect of
C de-magnification
C
      TIMES=1.5*VAL(NTIMES)
C
C Fault zero or negative magnification factor
C
      IF (TIMES.LE.0.0) THEN
         ERROR=3
         IDERR=NTIMES
         RETURN
      ENDIF
C
C Fetch ratio of specular to diffuse reflection
C
      SDR=VAL(NSDR)
C
C Fault negative ratio
C
      IF (SDR.LT.0.0) THEN
         ERROR=3
         IDERR=NSDR
         RETURN
      ENDIF
C
C Determine transformation from source picture coordinates to projected
C image coordinates
C
      THETA=VAL(NTHETA)
      PSI=VAL(NPSI)
C
      T11=COS(THETA)
      T12=SIN(THETA)
      T13=0.0
      T21=-COS(PSI)*SIN(THETA)
      T22=COS(PSI)*COS(THETA)
      T23=SIN(PSI)
      T31=SIN(PSI)*SIN(THETA)
      T32=-SIN(PSI)*COS(THETA)
      T33=COS(PSI)
C
C Determine view direction = inv(T).(0,0,1)
C Note: inv(T) = transpose(T)
C
      E1=T31
      E2=T32
      E3=T33
C
C Determine lighting direction (with respect to projected coordinates)
C
      LTHETA=VAL(NLTHET)
      LPHI=VAL(NLPHI)
C
      XT=SIN(LTHETA)*COS(LPHI)
      YT=SIN(LTHETA)*SIN(LPHI)
      ZT=COS(LTHETA)
C
C Determine lighting direction in source picture coordinates
C = inv(T).(XT,YT,ZT)
C
      L1=T11*XT+T21*YT+T31*ZT
      L2=T12*XT+T22*YT+T32*ZT
      L3=T13*XT+T23*YT+T33*ZT
C
C Determine direction of mirror normal = e + l
C
      M1=E1+L1
      M2=E2+L2
      M3=E3+L3
C
C Make mirror normal a unit vector if not zero vector
C
      D=SQRT(M1*M1+M2*M2+M3*M3)
C
      IF (D.NE.0.0) THEN
         M1=M1/D
         M2=M2/D
         M3=M3/D
      ENDIF
C
C Fetch source picture size and centre position
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
C
      CCOL=CCOLN(LP1)
      CROW=CROWN(LP1)
C
C Set loop indices so that the source scanning direction in X, Y and Z
C is from back to front with respect to the view direction
C
      IF (E1.GE.0.0) THEN
         I1=1
         I2=NCOL
         I3=1
      ELSE
         I1=NCOL
         I2=1
         I3=-1
      ENDIF
C
      IF (E2.LE.0.0) THEN
         J1=1
         J2=NROW
         J3=1
      ELSE
         J1=NROW
         J2=1
         J3=-1
      ENDIF
C
      IF (E3.GE.0.0) THEN
         K3=1
      ELSE
         K3=-1
      ENDIF
C
C Add de-magnification factor of 1.5 to transformation, to ensure that
C the source projects onto every output pixel
C
      DO 10 I=1,9
         T(I)=T(I)/1.5
   10 CONTINUE
C
C Set up source picture X and Y limits in picture coordinates
C
      XLIM(1)=REAL(1-CCOL)
      XLIM(2)=REAL(NCOL-CCOL)
      YLIM(1)=REAL(CROW-NROW)
      YLIM(2)=REAL(CROW-1)
C
C Determine min and max Z values (preserving VMIN,VMAX)
C
      SMGL1=.FALSE.
      Z1=VMIN
      Z2=VMAX
      IF (RANGE(1,LP1)) RETURN
      IF (ERROR.NE.0) RETURN
      ZMIN=VMIN
      ZMAX=VMAX
      VMIN=Z1
      VMAX=Z2
C
C Determine Z data range
C
      IF (LRANGE) THEN
         Z1=MIN(R1,R2)
         Z2=MAX(R1,R2)
      ELSE
         Z1=ZMIN
         Z2=ZMAX
      ENDIF
C
C Allow for additiion of border
C
      IF (LBORDE) THEN
         Z1=MIN(Z1,BORDER)
         Z2=MAX(Z2,BORDER)
      ENDIF
C
C Set up Z source origin using key ZORIGIN if set
C
      IF (VARSET(NZORIG)) THEN
         Z0=VAL(NZORIG)
C
C Fault Z origin outside Z data range
C
         IF (Z0.LT.Z1.OR.Z0.GT.Z2) THEN
            ERROR=3
            IDERR=NZORIG
            RETURN
         ENDIF
C
C Otherwise, Z origin defaults to centre of Z data range
C
      ELSE
         Z0=(Z1+Z2)/2.0
      ENDIF
C
C Set up Z source picture limits
C
      ZLIM(1)=Z1-Z0
      ZLIM(2)=Z2-Z0
C
C Determine Z source data transformation
C
      IF (LRANGE) THEN
         ZM=(R2-R1)/(ZMAX-ZMIN)
         ZC=ZMIN+(Z0-R1)/ZM
      ELSE
         ZM=1.0
         ZC=Z0
      ENDIF
C
C Initialise minimum and maximum transformed coordinates
C
      XMIN=0.0
      XMAX=0.0
      YMIN=0.0
      YMAX=0.0
      ZMIN=0.0
      ZMAX=0.0
C
C Determine minimum and maximum transformed coordinates
C
      DO 40 K=1,2
         DO 30 J=1,2
            DO 20 I=1,2
C
C Transform source picture limits
C
               XT=T11*XLIM(I)+T12*YLIM(J)+T13*ZLIM(K)
               YT=T21*XLIM(I)+T22*YLIM(J)+T23*ZLIM(K)
               ZT=T31*XLIM(I)+T32*YLIM(J)+T33*ZLIM(K)
C
C Look for extreme values
C
               XMIN=MIN(XT,XMIN)
               XMAX=MAX(XT,XMAX)
               YMIN=MIN(YT,YMIN)
               YMAX=MAX(YT,YMAX)
               ZMIN=MIN(ZT,ZMIN)
               ZMAX=MAX(ZT,ZMAX)
   20       CONTINUE
   30    CONTINUE
   40 CONTINUE
C
C Round X and Y limits outwards
C
      IXMIN=INT(XMIN)
      IF (REAL(IXMIN).GT.XMIN) IXMIN=IXMIN-1
C
      IXMAX=INT(XMAX)
      IF (REAL(IXMAX).LT.XMAX) IXMAX=IXMAX+1
C
      IYMIN=INT(YMIN)
      IF (REAL(IYMIN).GT.YMIN) IYMIN=IYMIN-1
C
      IYMAX=INT(YMAX)
      IF (REAL(IYMAX).LT.YMAX) IYMAX=IYMAX+1
C
C Determine size and centre position of temporary output picture
C
      NCOLT=IXMAX-IXMIN+1
      NROWT=IYMAX-IYMIN+1
C
      CCOLT=1-IXMIN
      CROWT=1+IYMAX
C
C Create temporary output picture
C
      LPTEMP=0
      IF (SEMOPN(3,0,NCOLT,NROWT,1,NCLIMA,NFMFP,LPTEMP)) RETURN
C
C Set centre position
C
      IF (SEMCEN(LPTEMP,CCOLT,CROWT,1)) RETURN
C
C Determine lighting conditions
C
      AMBIEN=VAL(NAMBIE)
      DCONT=VAL(NDCONT)/(ZMAX-ZMIN)
      MAINL=VAL(NMAIN)
      FORWL=VAL(NFORWA)
      SPECUL=SDR/(1.0+SDR)
      DIFFUS=1.0/(1.0+SDR)
C
C Fetch background pixel value
C
      VALUE=VAL(NVALUE)
C
C Initialise temporary output picture
C
      DO 50 I=1,NCOLT
         RB1(I)=VALUE
   50 CONTINUE
C
      DO 60 J=1,NROWT
         IF (SEMROW(2,RB1,NFMFP,J,1,LPTEMP)) RETURN
   60 CONTINUE
C
C Initialise output data count
C
      N=0
C
C Initialise flag for switching of row buffers RB1 and RB2
C
      EVEN=.FALSE.
C
C Initialise contents of RB1
C
      IF (SHEET2(RB1EXT,RB2EXT,NCOL,NROW,J1,LP1,ZM,ZC)) RETURN
C
C Process source picture
C
      DO 90 J=J1,J2,J3
C
C Determine Y source coordinate
C
         YS=REAL(CROW-J)
C
C Fetch neighbouring row in Y direction, taking into account switching
C of row buffers
C
         IF (EVEN) THEN
               IF (SHEET2(RB1EXT,RB2EXT,NCOL,NROW,J+J3,LP1,ZM,ZC))
     +            RETURN
         ELSE
            IF (SHEET2(RB2EXT,RB1EXT,NCOL,NROW,J+J3,LP1,ZM,ZC))
     +         RETURN
         ENDIF
C
C Process current source row
C
         DO 80 I=I1,I2,I3
C
C Determine X source coordinate
C
            XS=REAL(I-CCOL)
C
C Determine current Z value and neighbouring Z values
C
            IF (EVEN) THEN
               Z=RB2EXT(I)
               Z1=RB2EXT(I+I3)
               Z2=RB1EXT(I)
            ELSE
               Z=RB1EXT(I)
               Z1=RB1EXT(I+I3)
               Z2=RB2EXT(I)
            ENDIF
C
C Determine direction of surface normal
C
            IF (LBORDE.AND.(I.EQ.I2.OR.J.EQ.J2)) THEN
               IF (I.EQ.I2) THEN
                  N1=REAL(I3)
               ELSE
                  N1=0.0
               ENDIF
C
               IF (J.EQ.J2) THEN
                  N2=-REAL(J3)
               ELSE
                  N2=0.0
               ENDIF
C
               N3=0.0
            ELSE
               IF (I3.LT.0) THEN
                  N1=Z1-Z
               ELSE
                  N1=Z-Z1
               ENDIF
C
               IF (J3.LT.0) THEN
                  N2=Z-Z2
               ELSE
                  N2=Z2-Z
               ENDIF
C
               N3=1.0
            ENDIF
C
C Skip this pixel if surface normal points away from eye
C
            NE=N1*E1+N2*E2+N3*E3
            IF (NE.LE.0.0) GOTO 80
C
C Determine length of surface normal vector
C
            D=SQRT(N1*N1+N2*N2+N3*N3)
C
C Determine direction cosine of surface normal with respect to view dir.
C
            COSNE=NE/D
C
C Determine brightness value with no depth contrast or contribution from
C main light source
C
            B=AMBIEN+FORWL*(DIFFUS*COSNE+SPECUL*.03*COSNE/(1.03-COSNE))
C
C If surface points towards light source, add contribution from light
C source
C
            NL=N1*L1+N2*L2+N3*L3
            IF (NL.GT.0.0) THEN
C
C Determine direction cosine of surface normal with respect to light
C source
C
               COSNL=NL/D
C
C Determine direction cosine of surface normal with respect to mirror
C normal
C
               COSNM=(N1*M1+N2*M2+N3*M3)/D
C
C Add brightness contribution from light source
C
               B=B+MAINL*(DIFFUS*COSNL+SPECUL*.03*COSNM/(1.03-COSNM))
            ENDIF
C
C Determine Z source pixel range
C
            IF (I.EQ.I2.OR.J.EQ.J2) THEN
               IF (LBORDE) THEN
                  IZ1=MIN(NINT(Z),NINT(BORDER-Z0))
                  IZ2=MAX(NINT(Z),NINT(BORDER-Z0))
               ELSE
                  IZ1=NINT(Z)
                  IZ2=NINT(Z)
               ENDIF
            ELSE
               IZ1=MIN(NINT(Z),NINT(Z1)+1,NINT(Z2)+1)
               IZ2=MAX(NINT(Z),NINT(Z1)-1,NINT(Z2)-1)
            ENDIF
C
C Arrange Z limits to tie in with Z scanning direction
C
            IF (K3.LT.0) THEN
               K1=IZ2
               K2=IZ1
            ELSE
               K1=IZ1
               K2=IZ2
            ENDIF
C
C Generate series of source pixels
C
            DO 70 K=K1,K2,K3
C
C Determine Z source coordinate
C
               ZS=REAL(K)
C
C Transform position into temporary output picture coordinates
C
               XT=T11*XS+T12*YS+T13*ZS
               YT=T21*XS+T22*YS+T23*ZS
               ZT=T31*XS+T32*YS+T33*ZS
C
C Determine brightness value by adding depth contrast
C
               BR=B+DCONT*ZT
C
C Add results for this pixel to output data array
C
               N=N+1
               RB4(N)=XT
               RB5(N)=YT
               RB6(N)=BR
C
C Transfer contents of output data array to temporary output picture if
C data array full
C
               IF (N.EQ.LNBUF/LNREAL) THEN
C
C Add data values to temporary output picture
C
                  IF (SHEET3(N,NROWT,CCOLT,CROWT,LPTEMP)) RETURN
C
C Reset output data count
C
                  N=0
C
C Take into account switching of row buffers
C
                  IF (EVEN) THEN
                     JB1=J+J3
                     JB2=J
                  ELSE
                     JB1=J
                     JB2=J+J3
                  ENDIF
C
C Restore contents of RB1 and RB2 (SHEET3 uses them for extra workspace)
C
                  IF (SHEET2(RB1EXT,RB2EXT,NCOL,NROW,JB1,LP1,ZM,ZC))
     +               RETURN
                  IF (SHEET2(RB2EXT,RB1EXT,NCOL,NROW,JB2,LP1,ZM,ZC))
     +               RETURN
               ENDIF
   70       CONTINUE
   80    CONTINUE
C
C Switch row buffers RB1 and RB2
C
         EVEN=.NOT.EVEN
   90 CONTINUE
C
C Transfer contents of output data array to temporary output picture
C
      IF (SHEET3(N,NROWT,CCOLT,CROWT,LPTEMP)) RETURN
C
C Create output picture and transfer output results from temporary
C output picture using specified magnification factor.  Factor includes
C factor of 1.5 to reverse effect of de-magnification required by hidden
C surface algorithm
C
      IF (SHEET4(LPTEMP,TIMES,XMIN,XMAX,YMIN,YMAX,VALUE)) RETURN
C
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module SHEET2
C
      LOGICAL FUNCTION SHEET2(RBNEXT,RBMEXT,NCOL,NROW,J,LPN,ZM,ZC)
C
C Fetches row data from 2-D source picture LPN.  The data consists of Z
C values.  The data is transformed to fit the specified data range (if
C specified) and so that the central Z value is zero. Extra edge pixels
C are calculated to surround the source picture.  These pixel values
C are such as to maintain the same slope as their neighbouring pixels.
C
      INCLUDE 'PARAMS'
C
      REAL RBNEXT(1-LNEDGE:LNBUF/LNREAL+LNEDGE)
      REAL RBMEXT(1-LNEDGE:LNBUF/LNREAL+LNEDGE)
      INTEGER NCOL,NROW,J,LPN
      REAL ZM,ZC
C
      LOGICAL SEMROW
      INTEGER I
C
      SHEET2=.TRUE.
C
C Fetch row data from source picture, filling row buffer with
C calculated values instead, if row outside source picture
C
      IF (J.LT.1.OR.J.GT.NROW) THEN
C
C Row outside source picture - continue slope at edge of picture
C outwards for one more row.  Note: previous row values contained
C in RBMEXT
C
         DO 10 I=1,NCOL
            RBNEXT(I)=2.0*RBMEXT(I)-RBNEXT(I)
   10    CONTINUE
C
C Otherwise, row data is taken from source picture
C
      ELSE
C
C Read row data from source picture LPN
C
         IF (SEMROW(1,RBNEXT(1),NFMFP,J,1,LPN)) RETURN
C
C Transform Z source data
C
         DO 20 I=1,NCOL
            RBNEXT(I)=ZM*(RBNEXT(I)-ZC)
   20    CONTINUE
      ENDIF
C
C Set extra left and right-hand pixel values
C
      RBNEXT(0)=2.0*RBNEXT(1)-RBNEXT(2)
      RBNEXT(NCOL+1)=2.0*RBNEXT(NCOL)-RBNEXT(NCOL-1)
C
      SHEET2=.FALSE.
C
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module SHEET3
C
      LOGICAL FUNCTION SHEET3(N,NROW,CCOL,CROW,LPN)
C
C Output results (pixel X,Y position and brightness) are accumulated
C in row buffers RB4, RB5 and RB6.  When necessary they must be
C incorporated into the temporary output picture.  This routine does
C it by determining which output rows need to be accessed, and storing
C the results in each of these rows in turn.  The sorting of the data
C according to output rows is done very quickly by using two extra row
C buffers to store the results. IB1 points to the first result which is
C to be added to each row (zero if none) and IB2 is a linked list of
C pointers to subsequent values also to be added to the same output row
C (zero pointer value for last one in linked list).
C
      LOGICAL SEMROW
C
      INCLUDE 'COMMON'
C
      INTEGER N,NROW,CCOL,CROW,LPN
C
      INTEGER IB1(256),IB2(256)
      EQUIVALENCE (IB1,RB1),(IB2,RB2)
      INTEGER J,L
C
      SHEET3=.TRUE.
C
C Initialise row pointer array
C
      DO 10 J=1,NROW
         IB1(J)=0
   10 CONTINUE
C
C Sort out which output rows to add to using Y values in RB5
C Note: The array must be scanned backwards so that the linked list
C       traverses the output values in the correct order
C
      DO 20 L=N,1,-1
         J=CROW-NINT(RB5(L))
         IB2(L)=IB1(J)
         IB1(J)=L
   20 CONTINUE
C
C Add results to required output rows
C
      DO 40 J=1,NROW
C
C See if there is anything to add to this row
C
         L=IB1(J)
         IF (L.NE.0) THEN
C
C Fetch output row data
C
            IF (SEMROW(1,RB3,NFMFP,J,1,LPN)) RETURN
C
C Add data values to this row
C
   30       IF (L.NE.0) THEN
               RB3(CCOL+NINT(RB4(L)))=RB6(L)
               L=IB2(L)
               GOTO 30
            ENDIF
C
C Store output row data
C
            IF (SEMROW(2,RB3,NFMFP,J,1,LPN)) RETURN
         ENDIF
   40 CONTINUE
C
      SHEET3=.FALSE.
C
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module SHEET4
C
      LOGICAL FUNCTION SHEET4(LPTEMP,TIMES,XMIN,XMAX,YMIN,YMAX,VALUE)
C
C Creates final output picture by transferring contents of temporary
C output picture LPTEMP to picture specified by key TO.  Picture size is
C determined by the keys SIZE and SI2 (default is size just large enough
C to contain end result).  The keys POSITION and PO2 may be used to
C displace the output picture by a specified amount.  The output origin
C corresponds to the view centre and if this lies within the output
C picture limits, the picture origin is updated to reflect this.
C
      LOGICAL VARSET,SEMOPN,SEMCEN,SEMROW
      INTEGER SEMFRM,IVAL,IVALPN
      REAL VAL
C
      INCLUDE 'COMMON'
C
      INTEGER LPTEMP
      REAL TIMES,XMIN,XMAX,YMIN,YMAX,VALUE
C
      INTEGER CCOL,CROW,CCOLT,CROWT,CLASS,FORM
      INTEGER NCOL,NROW,IXMIN,IXMAX,IYMIN,IYMAX,NPIC,NCOLT,NROWT
      INTEGER JB1,JB2,J,I,J1,J2,I1,I2
      REAL X0,Y0,DXT,DYT,YT,DY,XT,DX,P00,P10,P01,P11
C
C Packed names
C
      INTEGER NSIZE,NSI2,NPOSIT,NPO2,NTO
      PARAMETER (NSIZE=30786, NSI2=30792, NPOSIT=26219, NPO2=26232)
      PARAMETER (NTO=-601)
C
      SHEET4=.TRUE.
C
C Determine output picture size from keys SIZE and SI2 if set
C
      IF (VARSET(NSIZE)) THEN
         NCOL=IVAL(NSIZE)
C
         IF (VARSET(NSI2)) THEN
            NROW=IVAL(NSI2)
         ELSE
            NROW=NCOL
         ENDIF
C
C Set up default centre position
C
         CCOL=1+NCOL/2
         CROW=1+NROW/2
C
C Otherwise, size defaults to size just large enough to contain output
C results
C
      ELSE
C
C Determine magnified output limits
C
         XMIN=TIMES*XMIN
         XMAX=TIMES*XMAX
         YMIN=TIMES*YMIN
         YMAX=TIMES*YMAX
C
C Round limits outwards
C
         IXMIN=INT(XMIN)
         IF (REAL(IXMIN).GT.XMIN) IXMIN=IXMIN-1
C
         IXMAX=INT(XMAX)
         IF (REAL(IXMAX).LT.XMAX) IXMAX=IXMAX+1
C
         IYMIN=INT(YMIN)
         IF (REAL(IYMIN).GT.YMIN) IYMIN=IYMIN-1
C
         IYMAX=INT(YMAX)
         IF (REAL(IYMAX).LT.YMAX) IYMAX=IYMAX+1
C
C Determine default size and centre position for output picture
C
         NCOL=IXMAX-IXMIN+1
         NROW=IYMAX-IYMIN+1
C
         CCOL=1-IXMIN
         CROW=1+IYMAX
      ENDIF
C
C Create output picture
C
      NPIC=IVALPN(NTO)
      CLASS=NCLIMA
      FORM=SEMFRM(NFMBYT)
      LP2=LP1
      IF (SEMOPN(2,NPIC,NCOL,NROW,1,CLASS,FORM,LP2)) RETURN
C
C Apply specified offset to picture origin
C
      X0=REAL(CCOL)+VAL(NPOSIT)
      Y0=REAL(CROW)-VAL(NPO2)
C
C Set output centre position if origin still within picture limits
C
      IF (X0.GE.1.0.AND.X0.LE.REAL(NCOL).AND.
     +    Y0.GE.1.0.AND.Y0.LE.REAL(NROW)) THEN
         IF (SEMCEN(LP2,NINT(X0),NINT(Y0),1)) RETURN
      ENDIF
C
C Fetch temporary output picture size and centre position
C
      NCOLT=NCOLS(LPTEMP)
      NROWT=NROWS(LPTEMP)
C
      CCOLT=CCOLN(LPTEMP)
      CROWT=CROWN(LPTEMP)
C
C Initialise input row pointers and X and Y coordinate increments
C
      JB1=0
      JB2=0
C
      DXT=1.0/TIMES
      DYT=1.0/TIMES
C
C Initialise Y coordinate position
C
      YT=REAL(CROWT)-Y0/TIMES
C
C Transfer results from temporary to final output picture
C
      DO 40 J=1,NROW
C
C Increment Y pixel coordinate
C
         YT=YT+DYT
C
C If outside Y picture limits, fill output row buffer with background
C pixel value
C
         IF (YT.LT.1.0.OR.YT.GE.REAL(NROWT)) THEN
            DO 10 I=1,NCOL
               RB3(I)=VALUE
   10       CONTINUE
C
C Otherwise, calculate output row values using interpolation scheme
C
         ELSE
C
C Determine temporary output row numbers
C
            J1=INT(YT)
            J2=J1+1
C
C Update contents of first row buffer if necessary
C
            IF (J1.NE.JB1) THEN
C
C Use contents of second row buffer if appropriate
C
               IF (J1.EQ.JB2) THEN
                  DO 20 I=1,NCOLT
                     RB1(I)=RB2(I)
   20             CONTINUE
C
C Otherwise, read data from temporary output picture
C
               ELSE
                  IF (SEMROW(1,RB1,NFMFP,J1,1,LPTEMP)) RETURN
               ENDIF
C
C Update pointer to first row buffer
C
               JB1=J1
            ENDIF
C
C Update contents of second row buffer if necessary
C
            IF (J2.NE.JB2) THEN
C
C Read data from temporary output picture
C
               IF (SEMROW(1,RB2,NFMFP,J2,1,LPTEMP)) RETURN
C
C Update pointer to second row buffer
C
               JB2=J2
            ENDIF
C
C Determine fractional part of Y pixel coordinate
C
            DY=YT-REAL(J1)
C
C Initialise X coordinate position
C
            XT=REAL(CCOLT)-X0/TIMES
C
C Process row data
C
            DO 30 I=1,NCOL
C
C Increment X pixel coordinate
C
               XT=XT+DXT
C
C If X coordinate outside picture limits, set pixel value to background
C pixel value
C
               IF (XT.LT.1.0.OR.XT.GE.REAL(NCOLT)) THEN
                  RB3(I)=VALUE
C
C Otherwise, determine pixel value by interpolation
C
               ELSE
C
C Determine pixel positions on either side of required position
C
                  I1=INT(XT)
                  I2=I1+1
C
C Determine fractional part of X pixel coordinate
C
                  DX=XT-REAL(I1)
C
C Fetch four surrounding pixel values
C
                  P00=RB1(I1)
                  P10=RB1(I2)
                  P01=RB2(I1)
                  P11=RB2(I2)
C
C Interpolate between four pixel values by first splitting square about
C diagonal with least pixel difference.  Then use three pixel values
C that lie at corners of triangle containing required pixel position.
C Pixel value is then obtained by straight linear interpolation between
C three corner values
C
                  IF (ABS(P11-P00).LT.ABS(P10-P01)) THEN
C
C Square split along leading diagonal
C
                     IF (DX.LT.DY) THEN
                        RB3(I)=P00+(P11-P01)*DX+(P01-P00)*DY
                     ELSE
                        RB3(I)=P00+(P10-P00)*DX+(P11-P10)*DY
                     ENDIF
C
C Otherwise, square split along other diagonal
C
                  ELSE
                     IF (DX+DY.LT.1.0) THEN
                        RB3(I)=P00+(P10-P00)*DX+(P01-P00)*DY
                     ELSE
                        RB3(I)=P11+(P11-P01)*(DX-1.0)+(P11-P10)*(DY-1.0)
                     ENDIF
                  ENDIF
               ENDIF
   30       CONTINUE
         ENDIF
C
C Store results in LP2
C
         IF (SEMROW(2,RB3,NFMFP,J,1,LP2)) RETURN
   40 CONTINUE
C
      SHEET4=.FALSE.
C
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
