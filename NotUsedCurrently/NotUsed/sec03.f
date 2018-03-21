      SUBROUTINE ZERO
C----------------------------------------------------------------------
C
C Reports presence or absence of pixels of value ZERO in 'FROM' picture
C
C NOTE: For complex input elements whose real part is zero are detected
C
C----------------------------------------------------------------------
C
C  Semper syntax:
C
C  Zero   :ZERO  from=sel  open(lp1,old)=from
C
C----------------------------------------------------------------------
C
      LOGICAL SEMROW,SEMCON
      INTEGER NCOL,NROW,NLAY,I,J,K
C
      INCLUDE 'COMMON'
C
C Set up dimensions of picture
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      NLAY=NLAYS(LP1)
C
C For each row in each layer in turn..
C
      DO 30 K=1,NLAY
         DO 20 J=1,NROW
C
C Read in row data to row buffer RB1 in floating point form
C
            IF (SEMROW(1,RB1,NFMFP,J,K,LP1)) RETURN
C
C Scan each row for zero element
C
            DO 10 I=1,NCOL
C
C Check for zero element
C
               IF(RB1(I) .EQ. 0.0) THEN
C
C  Output message to console and exit
C
                  IF(SEMCON('Picture contains ZERO element')) RETURN
                  RETURN
               ENDIF
C
   10       CONTINUE
   20    CONTINUE
   30 CONTINUE
C
C Output alternative message
C
      IF(SEMCON('Picture contains only NONZERO elements')) RETURN
      RETURN
C
      END
