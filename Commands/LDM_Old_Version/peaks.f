C Semper 6 processing module PEAKS
C
      SUBROUTINE PEAKS
C
C Locates local peaks in the source picture.  Results consists of list
C of peak positions and the corresponding peak values.  Only pixel
C values greater than the threshold value specified by key THRESHOLD
C are used. If key SRADIUS is set, any peaks within the given
C neighbourhood radius which are less than the maximum peak value are
C rejected.  If key RADIUS is set, the centre of mass and the
C corresponding mass value is determined.  Results are output to 3 layer
C position list and printed if option VERIFY is set.  Variable N is set
C to number of peaks found.
C
      REAL VAL
      INTEGER IVALPN
      LOGICAL OPT,VARSET,SEMCON,SEMROW,SEMLU,FINDCM,SEMOPN,SEMLAB,PEAKS3
      LOGICAL MARSET,FSINIT,FSLIST
C
      INCLUDE 'COMMON'
C
      REAL MASS,RADIUS,SRADIU,T,THRESH,X,Y
      INTEGER I,J,K,LABEL(256),MARK,N,NCOL,NROW
      LOGICAL SUPPR,CM,ANNOT
C
      EQUIVALENCE (LABEL,RB1)
C
C Packed names
C
      INTEGER NFROM,NTO,NVERIF,NSRADI,NTHRES,NRADIU,NN
      PARAMETER (NFROM=10335, NTO=-601, NVERIF=-3419)
      PARAMETER (NSRADI=31121, NTHRES=-339, NRADIU=28844, NN=22400)
C
C Fetch source picture size
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
C
C Fault bad size for source picture
C
      IF (NCOL.LT.3.OR.NROW.EQ.2) THEN
         ERROR=5
         IDERR=IVALPN(NFROM)
         GOTO 70
      ENDIF
C
C See if key SRADIUS is set
C
      SUPPR=VARSET(NSRADI)
C
C If key is set, fault zero or negative radius value
C
      IF (SUPPR) THEN
C
C Fetch value of key SRADIUS
C
         SRADIU=VAL(NSRADI)
C
C Fault zero or negative value
C
         IF (SRADIU.LE.0.0) THEN
            ERROR=3
            IDERR=NSRADI
            GOTO 70
         ENDIF
      ENDIF
C
C See if key RADIUS is set
C
      CM=VARSET(NRADIU)
C
C If key is set, fault a zero or negative value
C
      IF (CM) THEN
C
C Fetch value of key RADIUS
C
         RADIUS=VAL(NRADIU)
C
C Fault zero or negative value
C
         IF (RADIUS.LE.0.0) THEN
            ERROR=3
            IDERR=NRADIU
            GOTO 70
         ENDIF
      ENDIF
C
C Fetch value of key THRESHOLD
C
      THRESH=VAL(NTHRES)
C
C Fetch first row of source picture
C
      IF (SEMROW(1,RB1,NFMFP,1,1,LP1)) GOTO 70
C
C Initialise peaks count
C
      N=0
C
C Search source picture for local peaks (with special code for 1-D case)
C
      IF (NROW.EQ.1) THEN
C
C Scan single source row for peaks
C
         DO 10 I=2,NCOL-1
C
C Skip this point if less than treshold value
C
            T=RB1(I)
            IF (T.LT.THRESH) GOTO 10
C
C Skip this point if its value does not exceed neighbouring pixel values
C
            IF (T.LE.RB1(I-1)) GOTO 10
            IF (T.LE.RB1(I+1)) GOTO 10
C
C Add point to output position list
C
            N=N+1
            RB4(N)=REAL(I-CCOLN(LP1))
            RB5(N)=0.0
            RB6(N)=T
   10    CONTINUE
C
C Otherwise, search 2-D source picture for local peaks
C
      ELSE
C
C Fetch second row of source picture
C
         IF (SEMROW(1,RB2,NFMFP,2,1,LP1)) GOTO 70
C
C Scan all but first and last source rows
C
         DO 20 J=2,NROW-1
C
C Determine order of input row buffers
C
            K=MOD(J,3)
C
C Process source rows
C
            IF (K.EQ.0) THEN
               IF (PEAKS3(RB2,RB3,RB1,J,N,THRESH,SUPPR,SRADIU)) GOTO 70
            ELSE IF (K.EQ.1) THEN
               IF (PEAKS3(RB3,RB1,RB2,J,N,THRESH,SUPPR,SRADIU)) GOTO 70
            ELSE IF (K.EQ.2) THEN
               IF (PEAKS3(RB1,RB2,RB3,J,N,THRESH,SUPPR,SRADIU)) GOTO 70
            ENDIF
C
C Stop processing source picture if output position list is full
C
            IF (N.EQ.LNBUF/LNREAL) GOTO 30
   20    CONTINUE
      ENDIF
C
C Delete multiple peaks if required
C
   30 IF (SUPPR) CALL PEAKS2(RB4,RB5,RB6,N,SRADIU)
C
C Fault absence of any peaks
C
      IF (N.EQ.0) THEN
         ERROR=77
         IDMESS='No peaks found'
         GOTO 70
      ENDIF
C
C Replace peak positions with centre of mass, if required
C
      IF (CM) THEN
C
C Determine centre of mass for each peak
C
         DO 40 I=1,N
            IF (FINDCM(LP1,.TRUE.,.TRUE.,RB4(I),RB5(I),RADIUS,MASS,X,Y))
     +         GOTO 70
C
            RB4(I)=X
            RB5(I)=Y
            RB6(I)=MASS
   40    CONTINUE
      ENDIF
C
C Sort final contents of position list by peak value/mass
C
      CALL PEAKSR(RB4,RB5,RB6,N,.FALSE.)
C
C Fetch and check value of key MARK
C
      IF (MARSET(ANNOT,MARK)) GOTO 70
C
C If key MARK appropriately set, annotate specified display picture
C
      IF (ANNOT) THEN
C
C Initialise display graphics
C
         IF (FSINIT(3,MARK)) GOTO 70
C
C Annotate display picture only if 2-D image
C
         IF (FSPTYP.EQ.1) THEN
C
C Mark each point in position list
C
            IF (FSLIST(RB4,RB5,N,FSMMOD,FSMSIZ)) GOTO 70
         ENDIF
      ENDIF
C
C Open output picture = positon list
C
      LP2=LP1
      IF (SEMOPN(2,IVALPN(NTO),N,1,3,NCLPLI,NFMFP,LP2)) GOTO 70
C
C Add type code for position list to picture label = 1, straight
C position list
C
      IF (SEMLAB(1,LABEL,LP2)) GOTO 70
C
      LABEL(LBPLTY)=1
C
      IF (SEMLAB(2,LABEL,LP2)) GOTO 70
C
C Store contents of position list
C
      IF (SEMROW(2,RB4,NFMFP,1,1,LP2)) GOTO 70
      IF (SEMROW(2,RB5,NFMFP,1,2,LP2)) GOTO 70
      IF (SEMROW(2,RB6,NFMFP,1,3,LP2)) GOTO 70
C
C Set variable N to number of peaks
C
      IF (SEMLU(1,NN,REAL(N))) GOTO 70
C
C If option VERIFY is set, print results to console output stream
C
      IF (OPT(NVERIF)) THEN
         IF (SEMCON(' ')) GOTO 70
         IF (SEMCON('Number     position       value/mass')) GOTO 70
C
         DO 60 I=1,N
            WRITE (RECORD,50) I,RB4(I),RB5(I),RB6(I)
   50       FORMAT (I5,F9.2,',',F8.2,2X,G12.4)
            IF (SEMCON(RECORD)) GOTO 70
   60    CONTINUE
      ENDIF
C
   70 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module PEAKS2
C
      SUBROUTINE PEAKS2(X,Y,VALUE,N,RADIUS)
C
C Sorts entries in position list accordind to values in VALUE and then
C deletes all subsequent entries within distance RADIUS of each point.
C
      INTEGER N
      REAL X(*),Y(*),VALUE(*),RADIUS
C
      REAL DD,DX,DY,XPEAK,YPEAK
      INTEGER I,J
C
      IF (N .GT. 0) THEN
C
C Sort by VALUE
C
         CALL PEAKSR(X,Y,VALUE,N,.FALSE.)
C
C Examine each point (except last) each turn
C
         DD = RADIUS * RADIUS
         DO 20 I=1,N-1
C
C Skip this point if already deleted
C
            IF (X(I).GT.1E5) GOTO 20
C
C Fetch X and Y coordinate values
C
            XPEAK=X(I)
            YPEAK=Y(I)
C
C Examine all subsequent points
C
            DO 10 J=I+1,N
C
C Skip this point if already deleted
C
               IF (X(J).GT.1E5) GOTO 10
C
C Skip this point if X increment exceeds specified radius
C
               DX=X(J)-XPEAK
               IF (ABS(DX).GT.RADIUS) GOTO 10
C
C Skip this point if Y increment exceeds specified radius
C
               DY=Y(J)-YPEAK
               IF (ABS(DY).GT.RADIUS) GOTO 10
C
C Delete point if within specified radius by placing off image!
C
               IF ((DX*DX)+(DY*DY).LT.DD) X(J)=1E6
   10       CONTINUE
   20    CONTINUE
C
C Remove deleted entries from position list
C
         CALL PEAKSD(X,Y,VALUE,N,.TRUE.)
      ENDIF
C
      RETURN
C
C Copyright (C) 1987,1989,1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 sub-processing module PEAKS3
C
      LOGICAL FUNCTION PEAKS3(ROW1,ROW2,ROW3,J,N,THRESH,SUPPR,RADIUS)
C
C Locates all points along ROW2 which exceed the threshold value THRESH
C and and which exceed all neighbouring pixel values.  Row buffers are
C passed as dummy arrays to facilitate switching between row buffers
C
      REAL ROW1(*),ROW2(*),ROW3(*),THRESH,RADIUS
      INTEGER J,N
      LOGICAL SUPPR
C
      LOGICAL SEMROW,SEMDIA
C
      INCLUDE 'COMMON'
C
      REAL T
      INTEGER I,IM1,IP1,NBF
C
      PARAMETER (NBF=LNBUF/LNREAL)
C
      PEAKS3=.TRUE.
C
C Fetch contents of third row buffer from source picture
C
      IF (SEMROW(1,ROW3,NFMFP,J+1,1,LP1)) GOTO 30
C
C Scan along centre row
C
      DO 10 I=2,NCOLS(LP1)-1
C
C Skip this point if less than threshold value
C
         T=ROW2(I)
         IF (T.LT.THRESH) GOTO 10
C
C Skip this point if value does not exceed all neighbouring points
C
         IM1 = I-1
         IF (T.LE.ROW1(IM1)) GOTO 10
         IF (T.LE.ROW2(IM1)) GOTO 10
         IF (T.LE.ROW3(IM1)) GOTO 10
C
         IF (T.LE.ROW1(I)) GOTO 10
         IF (T.LE.ROW3(I)) GOTO 10
C
         IP1 = I+1
         IF (T.LE.ROW1(IP1)) GOTO 10
         IF (T.LE.ROW2(IP1)) GOTO 10
         IF (T.LE.ROW3(IP1)) GOTO 10
C
C If output buffer arrays full, try to condense data
C
         IF (N.EQ.NBF .AND. SUPPR)
     +      CALL PEAKS2(RB4,RB5,RB6,N,RADIUS)
C
C If buffer arrays still full, output message and return
C
         IF (N.EQ.LNBUF/LNREAL) THEN
            IF (SEMDIA('Position list overflow - omitting some',NDIWAR))
     +         GOTO 30
            GOTO 20
         ENDIF
C
C Add point to output data
C
         N=N+1
         RB4(N)=REAL(I-CCOLN(LP1))
         RB5(N)=REAL(CROWN(LP1)-J)
         RB6(N)=T
   10 CONTINUE
C
   20 PEAKS3=.FALSE.
C
   30 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
