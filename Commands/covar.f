C Semper 6 processing module COVAR
C
      SUBROUTINE COVAR
C
C Syntax:  Covariance :COVAR size= si2= position= po2=
C                            left right bottom top
C                            $1=sel from=$1 $2=fro to=$2
C                            open(lp1,old)=fro
C
C Calculates the covariance matrix and layer statistics (mean, standard
C deviation and range) of a subregion of the source picture and stores
C this in the output picture.  The number of rows/columns of the
C covariance matrix is equal to the number of layers in the subregion.
C Since the covariance matrix is symmetric, only the upper half of the
C matrix is calculated (the lower half is filled in by transposing the
C results).  The output picture contains the covariance matrix in rows
C 1 to N (where N = number of layers in the subregion) and the mean,
C standard deviation, minumum and maximum pixel values for each layer
C in rows N+1, N+2, N+3 and N+4 respectively.
C
      LOGICAL TSTSRG,SEMOPN,COVAR2
      INTEGER IVALPN,IPACK
C
      INTEGER NPIC
C
      INTEGER NCOL1,NROW1,NLAY1,NCOL2,NROW2,NLAY2,NCOL,NROW,NLAY
C
      INCLUDE 'COMMON'
C
      EQUIVALENCE (SMGI1,NCOL1),(SMGI2,NROW1),(SMGI3,NLAY1)
      EQUIVALENCE (SMGI4,NCOL2),(SMGI5,NROW2),(SMGI6,NLAY2)
      EQUIVALENCE (SMGI7,NCOL),(SMGI8,NROW),(SMGI9,NLAY)
C
C Process subregion keys/options to get 3D subregion limits and size
C
      IF (TSTSRG(1,LP1)) GOTO 10
C
C Open destination picture for covariance data and other statistics
C
      NPIC=IVALPN(-601)
      LP2=LP1
      IF (SEMOPN(2,NPIC,NLAY,NLAY+4,1,NCLUND,NFMFP,LP2)) GOTO 10
C
C Calculate covariance data
C
      IF (COVAR2(LP1,LP2)) GOTO 10
C
   10 RETURN
C
C Copyright (C) 1995-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module COVAR2
C
      LOGICAL FUNCTION COVAR2(LPS,LPC)
C
      INTEGER LPS,LPC
C
C Generates covariance matrix for multi-layer picture LPS and stores
C this in picture LPC together with mean, standard deviation and range
C for each layer.  Note: this routine is also called by PCT command.
C
      LOGICAL SEMROW,COVAR3
C
      INTEGER          I,J,K,L,N1,N2,NBUF
      REAL             PMIN,PMAX,PMEAN,PVAR
      DOUBLE PRECISION PSUM,PPSUM,PQSUM,RPSUM,RPPSUM,RPQSUM
C
      INCLUDE 'COMMON'
C
      INTEGER NCOL1,NROW1,NLAY1,NCOL2,NROW2,NLAY2,NCOL,NROW,NLAY
C
      EQUIVALENCE (SMGI1,NCOL1),(SMGI2,NROW1),(SMGI3,NLAY1)
      EQUIVALENCE (SMGI4,NCOL2),(SMGI5,NROW2),(SMGI6,NLAY2)
      EQUIVALENCE (SMGI7,NCOL),(SMGI8,NROW),(SMGI9,NLAY)
C
C Number of floating point values that can be stored in the row
C buffer array (less one row buffer for reading source data)
C
      INTEGER*4 NREAL
      PARAMETER ( NREAL = (NNBUF-1)*(LNBUF/LNREAL) )
C
      COVAR2=.TRUE.
C
C Process each source layer in turn
C
      DO 30 K=NLAY1,NLAY2
C
C Fetch first source row
C
         IF (SEMROW(1,RB1,NFMFP,NROW1,K,LPS)) GOTO 100
C
C Initialise minimum and maximum pixel values for current layer
C
         PMIN=RB1(NCOL1)
         PMAX=RB1(NCOL1)
C
C Initialise layer sums
C
         PSUM=0.0
         PPSUM=0.0
C
C Process current layer
C
         DO 20 J=NROW1,NROW2
C
C Fetch source row
C
            IF (J.NE.NROW1) THEN
               IF (SEMROW(1,RB1,NFMFP,J,K,LPS)) GOTO 100
            ENDIF
C
C Initialise row sums
C
            RPSUM=0.0
            RPPSUM=0.0
C
C Process row of data
C
            DO 10 I=NCOL1,NCOL2
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
   10       CONTINUE
C
C Add row sums to layer sums
C
            PSUM=PSUM+RPSUM
            PPSUM=PPSUM+RPPSUM
   20    CONTINUE
C
C Calculate mean and variance
C
         PMEAN=REAL(PSUM/(DBLE(NCOL)*DBLE(NROW)))
C
         PVAR=REAL(PPSUM/(DBLE(NCOL)*DBLE(NROW))-
     +             DBLE(PMEAN)*DBLE(PMEAN))
C
C Adjust variance for unbiased estimate
C
         PVAR=PVAR*(REAL(NCOL)*REAL(NROW))
     +            /(REAL(NCOL)*REAL(NROW)-1.0)
C
C Store mean, variance and range
C
         RB3(K)=PMEAN
         RB4(K)=PVAR
         RB5(K)=PMIN
         RB6(K)=PMAX
   30 CONTINUE
C
C Calculate standard deviation
C
      DO 40 K=NLAY1,NLAY2
         RB1(K)=SQRT(MAX(RB4(K),0.0))
   40 CONTINUE
C
C Write statistics to output picture
C
      IF (SEMROW(2,RB3(NLAY1),NFMFP,NLAY+1,1,LPC)) GOTO 100
      IF (SEMROW(2,RB1(NLAY1),NFMFP,NLAY+2,1,LPC)) GOTO 100
      IF (SEMROW(2,RB5(NLAY1),NFMFP,NLAY+3,1,LPC)) GOTO 100
      IF (SEMROW(2,RB6(NLAY1),NFMFP,NLAY+4,1,LPC)) GOTO 100
C
C Process source data to calculate upper half of covariance matrix
C
      DO 80 K=NLAY1,NLAY2
C
C Copy variance into covariance array
C
         RB5(K)=RB4(K)
C
C Process subsequent layers to obtain covariance sums
C
         DO 70 L=K+1,NLAY2
C
C Initialise layer sum
C
            PQSUM=0.0
C
C Calculate covariance sum between two layers
C
            DO 60 J=NROW1,NROW2
C
C Fetch source rows
C
               IF (SEMROW(1,RB1,NFMFP,J,K,LPS)) GOTO 100
               IF (SEMROW(1,RB2,NFMFP,J,L,LPS)) GOTO 100
C
C Initialise row sum
C
               RPQSUM=0.0
C
C Accumulate row sum
C
               DO 50 I=NCOL1,NCOL2
                  RPQSUM=RPQSUM+DBLE(RB1(I)*RB2(I))
   50          CONTINUE
C
C Add row sum to layer sum
C
               PQSUM=PQSUM+RPQSUM
   60       CONTINUE
C
C Calculate covariance
C
            PVAR=REAL(PQSUM/(DBLE(NCOL)*DBLE(NROW))-
     +                DBLE(RB3(K))*DBLE(RB3(L)))
C
C Adjust covariance for unbiased estimate
C
            PVAR=PVAR*(REAL(NCOL)*REAL(NROW))
     +               /(REAL(NCOL)*REAL(NROW)-1.0)
C
C Store covariance in covariance array
C
            RB5(L)=PVAR
   70    CONTINUE
C
C Write covariance data to output picture
C
         IF (SEMROW(2,RB5(NLAY1),NFMFP,1+K-NLAY1,1,LPC)) GOTO 100
   80 CONTINUE
C
C Determine number of available row buffers
C
      NBUF=MIN(NREAL/NLAY,NLAY)
C
C Transpose covariance data to fill lower half of covariance matrix
C
      DO 90 N1=1,NLAY,NBUF
         N2=MIN(N1+NBUF-1,NLAY)
         IF (COVAR3(RB1,RB2,NLAY,N1,N2,LPC)) GOTO 100
   90 CONTINUE
C
      COVAR2=.FALSE.
C
  100 RETURN
C
C Copyright (C) 1995:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module COVAR3
C
      LOGICAL FUNCTION COVAR3(DATA,BUFFER,N,N1,N2,LPN)
C
      INTEGER N,N1,N2,LPN
      REAL    DATA(N),BUFFER(N,N1:N2)
C
C Transposes rows N1 to N2 of the covariance matrix stored in rows 1
C to N of picture LPN.  DATA is a row buffer for reading the covariance
C data and BUFFER is large enough to store rows N1 to N2 of the
C covariance matrix.
C
      LOGICAL SEMROW
C
      INTEGER I,J
C
      INCLUDE 'PARAMS'
C
      COVAR3=.TRUE.
C
C Fill buffer array
C
      DO 10 J=N1,N2
         IF (SEMROW(1,BUFFER(1,J),NFMFP,J,1,LPN)) GOTO 70
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
         IF (SEMROW(2,BUFFER(1,J),NFMFP,J,1,LPN)) GOTO 70
   40 CONTINUE
C
C Transpose buffered data into subsequent rows of covariance matrix
C
      DO 60 J=N2+1,N
         IF (SEMROW(1,DATA,NFMFP,J,1,LPN)) GOTO 70
         DO 50 I=N1,N2
            DATA(I)=BUFFER(J,I)
   50    CONTINUE
         IF (SEMROW(2,DATA,NFMFP,J,1,LPN)) GOTO 70
   60 CONTINUE
C
      COVAR3=.FALSE.
C
   70 RETURN
C
C Copyright (C) 1995:  Synoptics Ltd,  All Rights Reserved
C
      END
