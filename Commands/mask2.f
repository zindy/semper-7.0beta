C Semper 6 subsidiary module MASK2
C
      SUBROUTINE MASK2(INSIDE,LVALUE)
C
C Masks the source picture using the plist in the picture
C specified by the key WITH.  The plist defines a polygonal
C area outside which all pixels are set to a given value.  If
C the key INSIDE is set, all pixels inside this area are reset.
C The reset value is specified by the key VALUE if set, otherwise
C the reset value is determined by calculating the perimeter mean.
C If the key VALUE is unset, the area's perimeter must intersect
C the source picture, so that the perimeter mean can be arrived at.
C
      LOGICAL INSIDE,LVALUE
C
      REAL VAL
      INTEGER IVALPN
      LOGICAL SEMOPN,SEMROW,EXTRCT
      LOGICAL MARSET,FSINIT,FSCURV,FSFLUS
C
      INCLUDE 'COMMON'
C
      REAL DUMMY(2),DP,P,S,T,X,X1,X2,Y,Y1,Y2,XLOW,XHIG,YLOW,YHIG
      INTEGER CLASS,FORM,EXTFRM,EXTLEN,CCOL,CROW,NPIC,NCOL,NROW,NLAY
      INTEGER I,I1,I2,INFORM,IP,J,K,M,MARK,N,NP
      LOGICAL NOROW,ANNOT
C
      INTEGER IB1(LNBUF/LNINT)
      EQUIVALENCE (IB1,RB1)
C
      LOGICAL CLOSED
      PARAMETER (CLOSED=.TRUE.)
C
C Packed names
C
      INTEGER NWITH,NVALUE,NVA2
      PARAMETER (NWITH=-5181, NVALUE=-3253, NVA2=-3273)
C
      DATA DUMMY / 2*0.0 /
C
C Open picture containing plist defining mask
C
      NPIC=IVALPN(NWITH)
      IF (SEMOPN(1,NPIC,NP,NROW,NLAY,CLASS,FORM,LP3)) GOTO 140
C
C Fault non-plist picture class
C
      IF (CLASS.NE.NCLPLI) THEN
         ERROR=6
         IDERR=NPIC
         GOTO 140
      ENDIF
C
C Read contents of plist
C
      IF (SEMROW(1,RB1,NFMFP,1,1,LP3)) GOTO 140
      IF (SEMROW(1,RB2,NFMFP,1,2,LP3)) GOTO 140
C
C Fetch and check value of key MARK
C
      IF (MARSET(ANNOT,MARK)) GOTO 140
C
C If key MARK appropriately set, annotate specified display picture
C
      IF (ANNOT) THEN
C
C Initialise display graphics
C
         IF (FSINIT(3,MARK)) GOTO 140
C
C Annotate display picture only if 2-D image
C
         IF (FSPTYP.EQ.1) THEN
C
C Draw closed curve
C
            IF (FSCURV(RB1,RB2,NP,CLOSED)) GOTO 140
C
C Flush graphics buffer
C
            IF (FSFLUS()) GOTO 140
         ENDIF
      ENDIF
C
C Fetch source picture size and centre position
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      NLAY=NLAYS(LP1)
      CCOL=CCOLN(LP1)
      CROW=CROWN(LP1)
C
C Determine form for processing source picture
C
      INFORM=FORMN(LP1)
      IF (INFORM.EQ.NFMBYT) INFORM=NFMINT
C
C See if reset value defined by key VALUE
C
      IF (LVALUE) THEN
C
C Set up reset value from VALUE key
C
         S=VAL(NVALUE)
         T=VAL(NVA2)
C
C Otherwise, determine perimeter mean for reset value
C
      ELSE
C
C Determine form for determining perimeter mean (using EXTRCT)
C
         IF (INFORM.EQ.NFMCOM) THEN
            EXTFRM=NFMCOM
            EXTLEN=LNCOMP
         ELSE
            EXTFRM=NFMFP
            EXTLEN=LNREAL
         ENDIF
      ENDIF
C
C Process each source picture layer in turn
C
      DO 130 K=1,NLAY
C
C If reset value not specified, establish it by determining the
C perimeter mean
C
         IF (.NOT.LVALUE) THEN
C
C Read contents of plist
C
            IF (SEMROW(1,RB1,NFMFP,1,1,LP3)) GOTO 140
            IF (SEMROW(1,RB2,NFMFP,1,2,LP3)) GOTO 140
C
C Repeat first point at the end of the position list
C
            RB1(NP+1)=RB1(1)
            RB2(NP+1)=RB2(1)
C
C Determine and sum lengths for each perimeter segment
C
            P=0.0
            DO 10 IP=1,NP
               RB5(IP)=SQRT((RB1(IP+1)-RB1(IP))**2+
     +                      (RB2(IP+1)-RB2(IP))**2)
C
               P=P+RB5(IP)
   10       CONTINUE
C
C Determine number of perimeter sampling points
C
            M=MAX(4,INT(MIN(P,REAL(LNBUF/EXTLEN))))
C
C Determine actual distance between each sampling point
C
            DP=P/REAL(M)
C
C First sampling point = first point in plist
C
            RB3(1)=RB1(1)
            RB4(1)=RB2(1)
C
C Determine remaining sampling points by interpolation
C
            P=0.0
            IP=1
            N=0
            DO 30 I=2,M
C
C Step forward to next point
C
               P=P+DP
C
C See if next sampling point lies beyond current segment
C
   20          IF (P.GT.RB5(IP)) THEN
                  P=P-RB5(IP)
                  IP=IP+1
                  GOTO 20
               ENDIF
C
C Determine position of next sampling point in pixel coordinates
C
               X=REAL(CCOL)+(RB1(IP)+P*(RB1(IP+1)-RB1(IP))/RB5(IP))
               Y=REAL(CROW)-(RB2(IP)+P*(RB2(IP+1)-RB2(IP))/RB5(IP))
C
C Add sampling point to list if within source picture limits
C
               IF (X.GE.1.0.AND.X.LE.REAL(NCOL).AND.
     +             Y.GE.1.0.AND.Y.LE.REAL(NROW)) THEN
                  N=N+1
                  RB3(N)=X
                  RB4(N)=Y
               ENDIF
   30       CONTINUE
C
C Fault disjoint perimeter (no sampling points)
C
            IF (N.EQ.0) THEN
               ERROR = 77
               IDMESS = 'Boundary entirely outside picture'
               GOTO 140
            ENDIF
C
C Extract source picture values at each sampling point using bi-linear
C interpolation
C
            IF (EXTRCT(LP1,N,EXTFRM,K,.FALSE.,DUMMY)) GOTO 140
C
C Determine perimeter mean
C
            IF (EXTFRM.EQ.NFMFP) THEN
               S=0.0
               DO 40 I=1,N
                  S=S+RB2(I)
   40          CONTINUE
               S=S/REAL(N)
            ELSE
               S=0.0
               T=0.0
               DO 50 I=1,2*N,2
                  S=S+RB2(I)
                  T=T+RB2(I+1)
   50          CONTINUE
               S=S/REAL(N)
               T=T/REAL(N)
            ENDIF
         ENDIF
C
C Restore contents of plist (EXTRCT uses all row buffers)
C
         IF (SEMROW(1,RB1,NFMFP,1,1,LP3)) GOTO 140
         IF (SEMROW(1,RB2,NFMFP,1,2,LP3)) GOTO 140
C
C Repeat first point at the end of the position list
C
         RB1(NP+1)=RB1(1)
         RB2(NP+1)=RB2(1)
C
C Set up perimeter data in convenient form for masking
C
         DO 60 IP=1,NP
C
C Determine pixel coordinates of two ends of current perimeter segment
C
            X1=REAL(CCOL)+RB1(IP)
            Y1=REAL(CROW)-RB2(IP)
            X2=REAL(CCOL)+RB1(IP+1)
            Y2=REAL(CROW)-RB2(IP+1)
C
C Decide which is top and bottom end point (in Y direction)
C
            IF (Y1.LT.Y2) THEN
               XLOW=X1
               YLOW=Y1
               XHIG=X2
               YHIG=Y2
            ELSE
               XLOW=X2
               YLOW=Y2
               XHIG=X1
               YHIG=Y1
            ENDIF
C
C Store parameters for later use
C
            RB3(IP)=YLOW
            RB4(IP)=YHIG
            RB5(IP)=XHIG-XLOW
            RB6(IP)=XLOW*YHIG-XHIG*YLOW
   60    CONTINUE
C
C Process source picture
C
         DO 120 J=1,NROW
C
C Count picture limits as intersection points if option INSIDE not set
C
            IF (INSIDE) THEN
               N=0
            ELSE
               RB2(1)=1.0
               RB2(2)=REAL(NCOL)
               N=2
            ENDIF
C
C Find all intersection points between current row and mask perimeter
C
            DO 70 IP=1,NP
C
C Add intersection X value to list if row crosses perimeter segment
C
               IF (RB3(IP).LT.REAL(J).AND.RB4(IP).GE.REAL(J)) THEN
                  N=N+1
                  RB2(N)=(RB5(IP)*REAL(J)+RB6(IP))/(RB4(IP)-RB3(IP))
               ENDIF
   70       CONTINUE
C
C Skip this row if no intersection points and in situ processing
C
            IF (N.EQ.0.AND.LP1.EQ.LP2) GOTO 120
C
C Set flag for outstanding input of current source row
C
            NOROW=.TRUE.
C
C Sort intersection points
C
            CALL MASK3(RB2,N)
C
C Reset all masked sections of row to reset value
C
            DO 110 M=1,N,2
C
C Fetch section limits and ignore section if outside picture limits
C
               X1=RB2(M)
               X2=RB2(M+1)
               IF (X1.GE.REAL(NCOL).OR.X2.LE.1.0) GOTO 110
C
C Clip section to picture limits and round inwards to next pixel
C
               X1=MAX(X1,1.0)
               X2=MIN(X2,REAL(NCOL))
C
               I1=INT(X1)
               IF (REAL(I1).LT.X1) I1=I1+1
               I2=INT(X2)
               IF (REAL(I2).GT.X2) I2=I2-1
C
C Read row from source picture if still not input
C
               IF (NOROW) THEN
                  IF (SEMROW(1,RB1,INFORM,J,K,LP1)) GOTO 140
C
                  NOROW=.FALSE.
               ENDIF
C
C Reset section according to form for processing the source picture
C
               IF (INFORM.EQ.NFMINT) THEN
                  DO 80 I=I1,I2
                     IB1(I)=NINT(S)
   80             CONTINUE
               ELSE IF (INFORM.EQ.NFMFP) THEN
                  DO 90 I=I1,I2
                     RB1(I)=S
   90             CONTINUE
               ELSE
                  DO 100 I=2*I1-1,2*I2,2
                     RB1(I)=S
                     RB1(I+1)=T
  100             CONTINUE
               ENDIF
  110       CONTINUE
C
C Check to see if source row still not input
C
            IF (NOROW) THEN
C
C Skip this row if in situ processing
C
               IF (LP1.EQ.LP2) GOTO 120
C
C Otherwise, read row from source picture
C
               IF (SEMROW(1,RB1,INFORM,J,K,LP1)) GOTO 140
            ENDIF
C
C Store result in LP2
C
            IF (SEMROW(2,RB1,INFORM,J,K,LP2)) GOTO 140
  120    CONTINUE
  130 CONTINUE
C
  140 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
