C Semper 6 processing module HISTOG
C
      SUBROUTINE HISTOG
C
C Generates and displays/stores histograms, with character-form
C display if TYPE/LOG set
C
      INTEGER IVAL,IVALPN
C     Change: LDM, July 2005
C     Ensure that gfortran does not think of range as an intrinsic
      EXTERNAL RANGE
      LOGICAL SEMOPN,SEMROW,SEMMED,TSTSRG,RANGE,GENHST
C
      INCLUDE 'COMMON'
C
      REAL V,V1,V2
      INTEGER DEVICE,IOP,MEDIUM,NCHAN,NCOL,NHIST
      LOGICAL TODCTP
C
C Packed names
C
      INTEGER NTO,NCHANN
      PARAMETER (NTO=-601,NCHANN=5121)
C
C Output to display?
C
      NHIST=IVALPN(NTO)
      DEVICE=NHIST/1000
      IF (SEMMED(DEVICE,MEDIUM)) GOTO 10
      TODCTP=MEDIUM.NE.MEDDS
C
C Establish region and range
C
      IF (TSTSRG(1,LP1)) GOTO 10
      IF (RANGE(1,LP1)) GOTO 10
C
C Establish number of channels
C
      NCHAN=IVAL(NCHANN)
      IF (NCHAN.LE.0) THEN
         V1=VMIN
         V2=VMAX
         IF (V2.NE.VMAX) V2=V2+1.
         V=V2-V1+1.
         IF (V.GE.20.AND.V.LE.256.) THEN
            NCHAN=V
            VMIN=V1
            VMAX=V2
         ELSE
            NCHAN=256
         ENDIF
      ENDIF
C
      NCOL=NCHAN+2
      IF (NCOL.GT.LNBUF/LNREAL) THEN
         ERROR=55
         IDERR=NHIST
         GOTO 10
      ENDIF
C
C Open output for histogram; to temp if displaying
C
      IOP=2
      IF (.NOT.TODCTP) IOP=3
      LP2=LP1
      IF (SEMOPN(IOP,NHIST,NCOL,1,1,9,2,LP2)) GOTO 10
C
C Construct histogram
C
      IF (GENHST(LP1,NCHAN)) GOTO 10
C
C Insert range, output and quit
C
      RB1(NCHAN+1)=VMIN
      RB1(NCHAN+2)=VMAX
      IF (SEMROW(2,RB1,2,1,1,LP2)) GOTO 10
C
C Call DISP directly if output to display
C
      IF (.NOT.TODCTP) THEN
         LP1=LP2
         LBLINC=.FALSE.
         CALL DISP
      ENDIF
   10 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
