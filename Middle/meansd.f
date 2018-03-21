C Semper 6 subsidiary module MEANSD
C
      LOGICAL FUNCTION MEANSD(LPN)
C
      INTEGER LPN
C
C Scans LPN for MIN,MAX,MEAN,IMEAN,SD; calls RANGE to record MIN,MAX
C if appropriate finally
C
C Needs SMGL1 set to indicate whether subregion, if and so also needs
C SMGI1-6 set to define it (as for TSTSRG)
C
C     Change: LDM, July 2005
C     Ensure that gfortran does not think of range, unpack as intrinsics
      EXTERNAL RANGE, UNPACKF
      LOGICAL SEMROW,RANGE
      REAL P,S
      REAL PSI,PSI2,PSII,PSII2
      REAL LSI,LSI2,LSII,LSII2
      REAL RSI,RSI2,RSII,RSII2
      INTEGER I,J,K,I1,I2,J1,J2,K1,K2,INFORM
      LOGICAL SUBREG
C
      INCLUDE 'COMMON'
C
      EQUIVALENCE (SMGL1,SUBREG)
C
      MEANSD=.TRUE.
C
C Establish limits of region to be scanned
C
      IF (SUBREG) THEN
         I1=SMGI1
         J1=SMGI2
         K1=SMGI3
         I2=SMGI4
         J2=SMGI5
         K2=SMGI6
      ELSE
         I1=1
         J1=1
         K1=1
         I2=NCOLS(LPN)
         J2=NROWS(LPN)
         K2=NLAYS(LPN)
      ENDIF
C
C Calculate number of pixels in region
C
      S=REAL(I2-I1+1)*REAL(J2-J1+1)*REAL(K2-K1+1)
C
C Determine form for processing source picture
C
      IF (FORMN(LPN).NE.NFMCOM) THEN
         INFORM=NFMFP
      ELSE
         INFORM=NFMCOM
      ENDIF
C
C Initialise minimum and maximum values
C
      IF (SEMROW(1,RB1,INFORM,J1,K1,LPN)) GOTO 80
C
      VMIN=RB1(I1)
      VMAX=RB1(I1)
C
C Initialise intensity sums
C
      PSI=0.0
      PSI2=0.0
      PSII=0.0
      PSII2=0.0
C
C Sum picture intensities (real + imaginary parts for complex picture)
C
      DO 70 K=K1,K2
C
C Initialise layer sums
C
         LSI=0.0
         LSI2=0.0
         LSII=0.0
         LSII2=0.0
C
C Sum intensities across layer
C
         DO 60 J=J1,J2
C
C Fetch picture row
C
            IF (SEMROW(1,RB1,INFORM,J,K,LP1)) GOTO 80
C
C Process according to non-complex/complex data form
C
            IF (INFORM.NE.NFMCOM) THEN
C
C Initialise row sums
C
               RSI=0.0
               RSII=0.0
C
C Look for min,max and sum data along row (sum + sum of squares)
C
               DO 40 I=I1,I2
                  P=RB1(I)
                  IF (P.LT.VMIN) THEN
                     VMIN=P
                  ELSE IF (P.GT.VMAX) THEN
                     VMAX=P
                  ENDIF
C
                  RSI=RSI+P
                  RSII=RSII+P*P
   40          CONTINUE
C
C Add row sums to layer sums
C
               LSI=LSI+RSI
               LSII=LSII+RSII
            ELSE
C
C Initialise row sums
C
               RSI=0.0
               RSI2=0.0
               RSII=0.0
               RSII2=0.0
C
C Look for min,max and sum data along row (sum + sum of squares)
C
               DO 50 I=2*I1-1,2*I2,2
                  P=RB1(I)
                  IF (P.LT.VMIN) THEN
                     VMIN=P
                  ELSE IF (P.GT.VMAX) THEN
                     VMAX=P
                  ENDIF
                  RSI=RSI+P
                  RSII=RSII+P*P
C
                  P=RB1(I+1)
                  IF (P.LT.VMIN) THEN
                     VMIN=P
                  ELSE IF (P.GT.VMAX) THEN
                     VMAX=P
                  ENDIF
                  RSI2=RSI2+P
                  RSII2=RSII2+P*P
   50          CONTINUE
C
C Add row sums to layer sums
C
               LSI=LSI+RSI
               LSI2=LSI2+RSI2
               LSII=LSII+RSII
               LSII2=LSII2+RSII2
            ENDIF
   60    CONTINUE
C
C Add layer sums to overall sums
C
         PSI=PSI+LSI
         PSI2=PSI2+LSI2
         PSII=PSII+LSII
         PSII2=PSII2+LSII2
   70 CONTINUE
C
C Calculate mean and standard deviation
C
      VMEAN=PSI/S
      VME2=PSI2/S
C
      VSD=SQRT(MAX((PSII/S-VMEAN*VMEAN)+(PSII2/S-VME2*VME2),0.0))
C
C If safe, arrange for MIN, MAX to be recorded in label
C
      IF (.NOT.SUBREG) THEN
         IF (RANGE(4,LPN)) GOTO 80
      ENDIF
C
      MEANSD=.FALSE.
C
   80 RETURN
C
C Copyright (C) 1987-1993:  Synoptics Ltd,  All Rights Reserved
C
      END
