C Semper 6.3 processing module GENHST
C
      LOGICAL FUNCTION GENHST(LPN,NCHAN)
C
C Constructs NCHAN channel histogram of range MIN-MAX of LPN sub-reg
C in RB1, using RB2/3;  no longer provides estimate on ABANDN
C
C Needs SMGL1 set to indicate whether subregion, if and so also needs
C SMGI1-6 set to define it (as for TSTSRG)
C
      LOGICAL SEMROW,SUBREG
      INTEGER COUNT(256),M1,N1,L1,M2,N2,L2,NSAFE,I,J,L,N,IA,LPN,INFORM
      INTEGER NCHAN
      REAL SCALE,A
      INCLUDE 'COMMON'
C
      EQUIVALENCE (RB3,COUNT)
      EQUIVALENCE (SMGL1,SUBREG)
C
C Establish region to be scanned
C
      IF (SUBREG) THEN
         M1=SMGI1
         N1=SMGI2
         L1=SMGI3
         M2=SMGI4
         N2=SMGI5
         L2=SMGI6
      ELSE
         M1=1
         N1=1
         L1=1
         M2=NCOLS(LPN)
         N2=NROWS(LPN)
         L2=NLAYS(LPN)
      ENDIF
C
C Establish form and adjust loop parameters
C
      IF (FORMN(LPN).EQ.NFMCOM) THEN
         INFORM=NFMCOM
         M1=2*M1-1
         M2=2*M2
      ELSE
         INFORM=NFMFP
      ENDIF
C
C Establish channel scaling
C
         SCALE=(NCHAN-.01)/(VMAX-VMIN)
C
C Number of rows without danger of counts overflowing
C
         NSAFE=32767./REAL(M2-M1+1)
C
C Initialise final channel accumulators
         DO 10 I=1,NCHAN
            RB1(I)=0.
   10    CONTINUE
C
C Loop over picture rows
C
         N=0
         DO 60 L=L1,L2
            DO 50 J=N1,N2
C
C Initialise local counts?
C
               IF (N.EQ.0) THEN
                  DO 20 I=1,NCHAN
                     COUNT(I)=0
   20             CONTINUE
               ENDIF
C
C Fetch data
C
               IF (SEMROW(1,RB2,INFORM,J,L,LPN)) GOTO 70
C
C Count row data
C
               DO 30 I = M1,M2
                  A = RB2(I)
                  IF (A.GE.VMIN .AND. A.LE.VMAX) THEN
                     IA = INT((A-VMIN)*SCALE) + 1
                     COUNT(IA) = COUNT(IA) + 1
                  ENDIF
   30          CONTINUE
C
               N=N+1
               IF (N.GE.NSAFE.OR.J.EQ.N2) THEN
                  DO 40 I=1,NCHAN
                     RB1(I)=RB1(I)+COUNT(I)
   40             CONTINUE
                  N=0
               ENDIF
C
   50       CONTINUE
   60    CONTINUE
C
C Normal return
C
      GENHST=.FALSE.
      RETURN
C
C Error return
C
   70 GENHST=.TRUE.
      RETURN
C
C Copyright (C) 1987-1991:  Synoptics Ltd,  All Rights Reserved
C
      END
