C Semper 6 processing module TRAIN
C
      SUBROUTINE TRAIN
C
C Syntax:  Learn :TRAIN data= da2= da3= da4= da5= da6= da7= da8= da9= +
C                       class= cl2= cl3= cl4= cl5= cl6= cl7= cl8= cl9= +
C                       verify $1=sel from=$1 open(lp1,old)=fro to=
C
C Calculates the covariance matrix and layer statistics (mean, standard
C deviation and range) for up to 9 sets of training regions in the
C source picture and stores this in the output picture (one layer for
C each class).  The training regions are defined with position lists
C (DATA key) and each position list can be assigned to a user specified
C class (CLASS key).  All the information for the training regions is
C combined into a single temporary integer form picture.  If a pixel
C belongs to a training region, the corresponding bit of the pixel mask
C will be set.  The number of rows/columns of the covariance matrix is
C equal to the number of layers in the subregion.  Since the covariance
C matrix is symmetric, only the upper half of the matrix is calculated
C (the lower half is filled in by transposing the results).  The output
C picture contains the covariance matrix in rows 1 to N (where
C N = number of layers in the subregion) and the mean, standard
C deviation, minumum and maximum pixel values for each layer in rows
C N+1, N+2, N+3 and N+4 respectively.
C
      LOGICAL VARSET,SETVAR,SEMOPN,SEMCON,OPT,TRAIN2,TRAIN3
      INTEGER IVAL,IVALPN,IPACK
C
      INTEGER     I,N,NLIST,NCLASS,NCOL,NROW,NLAY,NPIC,LPT
      INTEGER     CLASS(9),PLIST(9),XPOS(9),YPOS(9)
      LOGICAL     LCLASS(9)
      INTEGER*4   NSUM(9)
      CHARACTER*3 NAME
      CHARACTER*9 DIGIT
C
      INCLUDE 'COMMON'
C
C Bit masks for classes 1 to 9
C
      INTEGER BITPOS(9)
C
      DATA BITPOS / 1, 2, 4, 8, 16, 32, 64, 128, 256 /
C
C See how many DATA keys set
C
      NAME(1:2)='da'
      DIGIT='t23456789'
      NLIST=0
C
      DO 10 I=1,9
         NAME(3:3)=DIGIT(I:I)
C
         IF (VARSET(IPACK(NAME))) THEN
            PLIST(I)=IVALPN(IPACK(NAME))
            NLIST=I
         ELSE
            GOTO 20
         ENDIF
   10 CONTINUE
C
C Fault DATA key not set
C
   20 IF (NLIST.EQ.0) THEN
         ERROR=25
         IDERR=6460
         GOTO 110
      ENDIF
C
C See if any CLASS keys set
C
      DO 30 I=1,NLIST
         LCLASS(I)=.FALSE.
   30 CONTINUE
C
      NAME(1:2)='cl'
      DIGIT='a23456789'
      NCLASS=0
C
      DO 40 I=1,NLIST
         NAME(3:3)=DIGIT(I:I)
C
         IF (VARSET(IPACK(NAME))) THEN
            N=IVAL(IPACK(NAME))
            IF (N.LT.1.OR.N.GT.NLIST) GOTO 120
            CLASS(I)=N
            LCLASS(N)=.TRUE.
            IF (N.GT.NCLASS) NCLASS=N
         ELSE
            GOTO 50
         ENDIF
   40 CONTINUE
C
C If CLASS key not set, assign each training region to a separate class
C
   50 IF (NCLASS.EQ.0) THEN
         DO 60 I=1,NLIST
            CLASS(I)=I
   60    CONTINUE
C
         NCLASS=NLIST
C
C Otherwise, make sure there are no gaps in class numbering
C
      ELSE
         DO 70 I=1,NCLASS
            IF (.NOT.LCLASS(I)) GOTO 120
   70    CONTINUE
      ENDIF
C
C Fetch source picture dimensions
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      NLAY=NLAYS(LP1)
C
C Open temporary picture to store training data
C
      LPT=LP1
      IF (SEMOPN(3,0,NCOL,NROW,1,NCLIMA,NFMINT,LPT)) GOTO 110
C
C Generate training data
C
      IF (TRAIN2(LPT,PLIST,CLASS,NLIST,BITPOS,XPOS,YPOS,NSUM,NCLASS))
     +   GOTO 110
C
C Return pixel counts for each class in AREA variables
C
      NAME(1:2)='ar'
      DIGIT='e23456789'
C
      DO 80 I=1,NCLASS
         NAME(3:3)=DIGIT(I:I)
         IF (SETVAR(IPACK(NAME),REAL(NSUM(I)))) GOTO 110
   80 CONTINUE
C
C If VERIFY option set, list pixel counts (and flag low counts)
C
      IF (OPT(-3419)) THEN
         DO 90 I=1,NCLASS
            WRITE (RECORD,130) I,NSUM(I)
            IF (NSUM(I).LT.10*NLAY) RECORD(10:10)='*'
            IF (SEMCON(RECORD)) GOTO 110
   90    CONTINUE
      ENDIF
C
C Fault any zero pixel counts
C
      DO 100 I=1,NCLASS
         IF (NSUM(I).EQ.0) THEN
            ERROR=77
            WRITE (RECORD,140) I
            GOTO 110
         ENDIF
  100 CONTINUE
C
C Open destination picture for covariance data and other statistics
C
      NPIC=IVALPN(-601)
      LP2=LP1
      IF (SEMOPN(2,NPIC,NLAY,NLAY+4,NCLASS,NCLUND,NFMFP,LP2)) GOTO 110
C
C Generate covariance data
C
      IF (TRAIN3(LPT,BITPOS,XPOS,YPOS,NSUM,NCLASS)) GOTO 110
C
  110 RETURN
C
C Fault bad CLASS key
C
  120 ERROR=3
      IDERR=5281
      GOTO 110
C
  130 FORMAT ('Class',I3,'   contains',I8,' pixels')
C
  140 FORMAT ('Training region for class',I3,
     +        ' contains no source pixels')
C
C Copyright (C) 1995:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module TRAIN2
C
      LOGICAL FUNCTION TRAIN2(LPT,PLIST,CLASS,NLIST,
     +                        BITPOS,XPOS,YPOS,NSUM,NCLASS)
C
      INTEGER   LPT,PLIST(*),CLASS(*),NLIST
      INTEGER   BITPOS(*),XPOS(*),YPOS(*),NCLASS
      INTEGER*4 NSUM(*)
C
C Fills picture LPT with class bits wherever pixels lie within a
C training region for a given class.  The training regions are
C defined by Plists whose picture numbers are stored in array PLIST.
C The class associated with each training region and Plist is stored
C array CLASS.  NLIST specifies the number of Plists to be processed.
C Array BITPOS contains the appropriate bit mask for each class.
C XPOS and YPOS record the position of a pixel (the bottom-most,
C right-most one) in a class and NSUM returns the number of pixels.
C The classes range from 1 to NCLASS (i.e. 1 <= CLASS(i) <= NCLASS).
C
      LOGICAL SEMOPN,SEMCLS,SEMROW
C
      LOGICAL   LREAD
      INTEGER   I,J,L,M,N,I1,I2,IP,NP,NR,NL,ICLASS,FM
      INTEGER   NCOL,NROW,CCOL,CROW,MASK,IX,IY
      INTEGER*4 ISUM
      REAL      X1,X2,Y1,Y2,XLOW,XHIG,YLOW,YHIG
C
      INCLUDE 'COMMON'
C
      INTEGER IB1(LNBUF/LNINT)
      EQUIVALENCE (IB1,RB1)
C
      TRAIN2=.TRUE.
C
C Fetch temporary picture size and centre position
C
      NCOL=NCOLS(LPT)
      NROW=NROWS(LPT)
C
      CCOL=CCOLN(LPT)
      CROW=CROWN(LPT)
C
C Fill row buffer with zeros
C
      DO 10 I=1,NCOL
         IB1(I)=0
   10 CONTINUE
C
C Fill temporary picture with zeros
C
      DO 20 J=1,NROW
         IF (SEMROW(2,RB1,NFMINT,J,1,LPT)) GOTO 130
   20 CONTINUE
C
C Process each Plist in turn
C
      DO 80 L=1,NLIST
C
C Open picture containing Plist
C
         IF (SEMOPN(1,PLIST(L),NP,NR,NL,ICLASS,FM,LP3)) GOTO 130
C
C Fault picture class if not Plist
C
         IF (ICLASS.NE.NCLPLI) THEN
            ERROR=6
            IDERR=PLIST(L)
            GOTO 130
         ENDIF
C
C Read contents of Plist
C
         IF (SEMROW(1,RB1,NFMFP,1,1,LP3)) GOTO 130
         IF (SEMROW(1,RB2,NFMFP,1,2,LP3)) GOTO 130
C
C Close Plist to protect the LP entry for LP1 and LPT
C (otherwise problem will arise if NLIST+2 > NLPS)
C
         IF (SEMCLS(LP3)) GOTO 130
C
C Repeat first point at the end of the position list
C
         RB1(NP+1)=RB1(1)
         RB2(NP+1)=RB2(1)
C
C Set up perimeter data in convenient form for masking
C
         DO 30 IP=1,NP
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
   30    CONTINUE
C
C Set up bit mask for the class associated with this training region
C
         MASK=BITPOS(CLASS(L))
C
C Process source picture
C
         DO 70 J=1,NROW
C
C Find all intersection points between current row and mask perimeter
C
            N=0
            DO 40 IP=1,NP
C
C Add intersection X value to list if row crosses perimeter segment
C
               IF (RB3(IP).LT.REAL(J).AND.RB4(IP).GE.REAL(J)) THEN
                  N=N+1
                  RB2(N)=(RB5(IP)*REAL(J)+RB6(IP))/(RB4(IP)-RB3(IP))
               ENDIF
   40       CONTINUE
C
C Skip this row if no intersection points
C
            IF (N.EQ.0) GOTO 70
C
C Sort intersection points
C
            CALL MASK3(RB2,N)
C
C Set flag to say that current row has not been read in
C
            LREAD=.FALSE.
C
C Set bit in all masked sections of row
C
            DO 60 M=1,N,2
C
C Fetch section limits and ignore section if outside picture limits
C
               X1=RB2(M)
               X2=RB2(M+1)
               IF (X1.GE.REAL(NCOL).OR.X2.LE.1.0) GOTO 60
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
C If row data not read in, read it in now
C
               IF (.NOT.LREAD) THEN
                  IF (SEMROW(1,RB1,NFMINT,J,1,LPT)) GOTO 130
                  LREAD=.TRUE.
               ENDIF
C
C Set class bit
C
               DO 50 I=I1,I2
                  IB1(I)=IOR(IB1(I),MASK)
   50          CONTINUE
   60       CONTINUE
C
C If row data read in, write it back now
C
            IF (LREAD) THEN
               IF (SEMROW(2,RB1,NFMINT,J,1,LPT)) GOTO 130
            ENDIF
   70    CONTINUE
   80 CONTINUE
C
C Zero pixel count for each class
C
      DO 90 M=1,NCLASS
         NSUM(M)=0
   90 CONTINUE
C
C Scan the results
C
      DO 120 J=1,NROW
C
C Fetch row of bits
C
         IF (SEMROW(1,RB1,NFMINT,J,1,LPT)) GOTO 130
C
C Count pixels in each class
C
         DO 110 M=1,NCLASS
C
C Set up bit mask for this class
C
            MASK=BITPOS(M)
C
C Zero pixel count for row
C
            ISUM=0
C
C Process row of bits
C
            DO 100 I=1,NCOL
C
C If pixel contained in class, count it and record its position
C
               IF (IAND(IB1(I),MASK).NE.0) THEN
                  IX=I
                  IY=J
                  ISUM=ISUM+1
               ENDIF
  100       CONTINUE
C
C If pixels counted in this row, update pixel position and count
C
            IF (ISUM.GT.0) THEN
               XPOS(M)=IX
               YPOS(M)=IY
               NSUM(M)=NSUM(M)+ISUM
            ENDIF
  110    CONTINUE
  120 CONTINUE
C
      TRAIN2=.FALSE.
C
  130 RETURN
C
C Copyright (C) 1995:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module TRAIN3
C
      LOGICAL FUNCTION TRAIN3(LPT,BITPOS,XPOS,YPOS,NSUM,NCLASS)
C
      INTEGER   LPT,BITPOS(*),XPOS(*),YPOS(*),NCLASS
      INTEGER*4 NSUM(*)
C
C Generates covariance matrix for each set of training regions defined
C in temporary picture LPT.  Each set corresponds to one or NCLASS
C classes.  A pixel belongs to a given class if the corresponding bit
C in picture LPT is set.  Array BITPOS defines the bit masks for each
C class.  The source data is contained in picture LP1 and the results
C stored in picture LP2, with one layer for each class.  Each layer
C contains the covariance matrix for that class together with the mean
C standard deviation and range for each layer of source pixels
C contained within the training region for that class.  Arrays XPOS
C and YPOS supply the position of a pixel which lies within a training
C region for a given class.  This is used to fetch a source pixel value
C to initialise the range variables PMIN and PMAX.  Array NSUM provides
C the pixel counts for each class.
C
      LOGICAL SEMROW,TRAIN4
C
      INTEGER          I,J,K,L,M,N1,N2
      INTEGER          NBUF,NCOL,NROW,NLAY,MASK
      REAL             PMIN,PMAX,PMEAN,PVAR
      DOUBLE PRECISION PSUM,PPSUM,PQSUM,RPSUM,RPPSUM,RPQSUM
C
      INCLUDE 'COMMON'
C
      INTEGER IB2(LNBUF/LNINT)
      INTEGER IB6(LNBUF/LNINT)
C
      EQUIVALENCE (IB2,RB2),(IB6,RB6)
C
C Number of floating point values that can be stored in the row
C buffer array (less one row buffer for reading source data)
C
      INTEGER*4 NREAL
      PARAMETER ( NREAL = (NNBUF-1)*(LNBUF/LNREAL) )
C
      TRAIN3=.TRUE.
C
C Fetch source picture dimensions
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      NLAY=NLAYS(LP1)
C
C Process each class in turn
C
      DO 100 M=1,NCLASS
C
C Set up bit mask for this class
C
         MASK=BITPOS(M)
C
C Process each source layer in turn
C
         DO 30 K=1,NLAY
C
C Fetch source row containing at least one pixel in training region
C
            IF (SEMROW(1,RB1,NFMFP,YPOS(M),K,LP1)) GOTO 110
C
C Initialise minimum and maximum pixel values for current layer
C
            PMIN=RB1(XPOS(M))
            PMAX=RB1(XPOS(M))
C
C Initialise layer sums
C
            PSUM=0.0
            PPSUM=0.0
C
C Process current layer
C
            DO 20 J=1,NROW
C
C Fetch source row
C
               IF (SEMROW(1,RB1,NFMFP,J,K,LP1)) GOTO 110
C
C Fetch training data
C
               IF (SEMROW(1,RB2,NFMINT,J,1,LPT)) GOTO 110
C
C Initialise row sums
C
               RPSUM=0.0
               RPPSUM=0.0
C
C Process row of data
C
               DO 10 I=1,NCOL
C
C See if pixel lies within training region
C
                  IF (IAND(IB2(I),MASK).NE.0) THEN
C
C Update minimum and maximum values
C
                     IF (RB1(I).LT.PMIN) PMIN=RB1(I)
                     IF (RB1(I).GT.PMAX) PMAX=RB1(I)
C
C Accumulate row sums
C
                     RPSUM=RPSUM+DBLE(RB1(I))
                     RPPSUM=RPPSUM+DBLE(RB1(I)*RB1(I))
                  ENDIF
   10          CONTINUE
C
C Add row sums to layer sums
C
               PSUM=PSUM+RPSUM
               PPSUM=PPSUM+RPPSUM
   20       CONTINUE
C
C Calculate mean and variance
C
            PMEAN=REAL(PSUM/DBLE(NSUM(M)))
C
            PVAR=REAL(PPSUM/DBLE(NSUM(M))-DBLE(PMEAN)*DBLE(PMEAN))
C
C Adjust variance for unbiased estimate
C
            PVAR=PVAR*REAL(NSUM(M))/(REAL(NSUM(M))-1.0)
C
C Store mean, variance and range
C
            RB3(K)=PMEAN
            RB4(K)=PVAR
            RB5(K)=PMIN
            RB6(K)=PMAX
   30    CONTINUE
C
C Calculate standard deviation
C
         DO 40 K=1,NLAY
            RB1(K)=SQRT(MAX(RB4(K),0.0))
   40    CONTINUE
C
C Write statistics to output picture
C
         IF (SEMROW(2,RB3,NFMFP,NLAY+1,M,LP2)) GOTO 110
         IF (SEMROW(2,RB1,NFMFP,NLAY+2,M,LP2)) GOTO 110
         IF (SEMROW(2,RB5,NFMFP,NLAY+3,M,LP2)) GOTO 110
         IF (SEMROW(2,RB6,NFMFP,NLAY+4,M,LP2)) GOTO 110
C
C Process source data to calculate upper half of covariance matrix
C
         DO 80 K=1,NLAY
C
C Copy variance into covariance array
C
            RB5(K)=RB4(K)
C
C Process subsequent layers to obtain covariance sums
C
            DO 70 L=K+1,NLAY
C
C Initialise layer sum
C
               PQSUM=0.0
C
C Calculate covariance sum between two layers
C
               DO 60 J=1,NROW
C
C Fetch source rows
C
                  IF (SEMROW(1,RB1,NFMFP,J,K,LP1)) GOTO 110
                  IF (SEMROW(1,RB2,NFMFP,J,L,LP1)) GOTO 110
C
C Fetch training data
C
                  IF (SEMROW(1,RB6,NFMINT,J,1,LPT)) GOTO 110
C
C Initialise row sum
C
                  RPQSUM=0.0
C
C Accumulate row sum
C
                  DO 50 I=1,NCOL
                     IF (IAND(IB6(I),MASK).NE.0) THEN
                        RPQSUM=RPQSUM+DBLE(RB1(I)*RB2(I))
                     ENDIF
   50             CONTINUE
C
C Add row sum to layer sum
C
                  PQSUM=PQSUM+RPQSUM
   60          CONTINUE
C
C Calculate covariance
C
               PVAR=REAL(PQSUM/DBLE(NSUM(M))-DBLE(RB3(K))*DBLE(RB3(L)))
C
C Adjust covariance for unbiased estimate
C
               PVAR=PVAR*REAL(NSUM(M))/(REAL(NSUM(M))-1.0)
C
C Store covariance in covariance array
C
               RB5(L)=PVAR
   70       CONTINUE
C
C Write covariance data to output picture
C
            IF (SEMROW(2,RB5,NFMFP,K,M,LP2)) GOTO 110
   80    CONTINUE
C
C Determine number of available row buffers
C
         NBUF=MIN(NREAL/NLAY,NLAY)
C
C Transpose covariance data to fill lower half of covariance matrix
C
         DO 90 N1=1,NLAY,NBUF
            N2=MIN(N1+NBUF-1,NLAY)
            IF (TRAIN4(RB1,RB2,M,NLAY,N1,N2,LP2)) GOTO 110
   90    CONTINUE
  100 CONTINUE
C
      TRAIN3=.FALSE.
C
  110 RETURN
C
C Copyright (C) 1995:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module TRAIN4
C
      LOGICAL FUNCTION TRAIN4(DATA,BUFFER,K,N,N1,N2,LPN)
C
      INTEGER K,N,N1,N2,LPN
      REAL    DATA(N),BUFFER(N,N1:N2)
C
C Transposes rows N1 to N2 of the covariance matrix stored in rows 1
C to N of layer K of picture LPN.  DATA is a row buffer for reading
C the covariance data and BUFFER is large enough to store rows N1 to
C N2 of the covariance matrix.
C
      LOGICAL SEMROW
C
      INTEGER I,J
C
      INCLUDE 'PARAMS'
C
      TRAIN4=.TRUE.
C
C Fill buffer array
C
      DO 10 J=N1,N2
         IF (SEMROW(1,BUFFER(1,J),NFMFP,J,K,LPN)) GOTO 70
   10 CONTINUE
C
C Transpose values within buffer array
C
      DO 30 J=N1,N2
         DO 20 I=J+1,N2
            BUFFER(J,I)=BUFFER(I,J)
   20    CONTINUE
   30 CONTINUE
C
C Write back data in buffer array
C
      DO 40 J=N1,N2
         IF (SEMROW(2,BUFFER(1,J),NFMFP,J,K,LPN)) GOTO 70
   40 CONTINUE
C
C Transpose buffered data into subsequent rows of covariance matrix
C
      DO 60 J=N2+1,N
         IF (SEMROW(1,DATA,NFMFP,J,K,LPN)) GOTO 70
         DO 50 I=N1,N2
            DATA(I)=BUFFER(J,I)
   50    CONTINUE
         IF (SEMROW(2,DATA,NFMFP,J,K,LPN)) GOTO 70
   60 CONTINUE
C
      TRAIN4=.FALSE.
C
   70 RETURN
C
C Copyright (C) 1995:  Synoptics Ltd,  All Rights Reserved
C
      END
