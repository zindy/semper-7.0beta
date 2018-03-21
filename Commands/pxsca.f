C Semper 6 processing module PXSCA
C
      SUBROUTINE PXSCA
C
C Rescales LP1 to LP2, with optional range truncation
C - Provides commands SCALE RANGE, SCALE MSD and NEGATE
C
      REAL VAL
      INTEGER IVALPN
C     Change: LDM, July 2005
C     Ensure that gfortran does not think of range as an intrinsic
      EXTERNAL RANGE
      LOGICAL SEMROW,RANGE,MEANSD,OPT,VARSET
C
      INCLUDE 'COMMON'
C
      REAL A,B,P,SB,ST,T
      INTEGER FORM,I,IB1(256),INFORM,J,L,MAP(0:255),NC2,NCOL,NLAY,NROW
      LOGICAL LMSD,FLSCAN,BYTEO,BOTHB
C
C Packed names
C
      INTEGER NSCALE,NFROM,NPRESE,NRANGE,NRA2,NMSD,NMS2
      PARAMETER (NSCALE=30521,NFROM=10335,NPRESE=26325)
      PARAMETER (NRANGE=28854,NRA2=28872,NMSD=21564,NMS2=21592)
C
      EQUIVALENCE (RB1,IB1),(RB2,MAP)
C
C Initialise
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      NLAY=NLAYS(LP1)
C
      FORM=FORMN(LP1)
      IF (FORM.EQ.NFMCOM .AND. FORMN(LP2).EQ.NFMCOM) THEN
         INFORM=NFMCOM
      ELSE
         INFORM=NFMFP
      ENDIF
C
      BYTEO = FORMN(LP2).EQ.NFMBYT
      BOTHB = FORM.EQ.NFMBYT .AND. BYTEO
      IF (BOTHB) INFORM=NFMINT
C
      IF (INFORM.EQ.NFMCOM) THEN
         NC2 = NCOL+NCOL
      ELSE
         NC2 = NCOL
      ENDIF
      LMSD=VARSET(NMSD)
      FLSCAN=.NOT.OPT(NPRESE)
C
C Establish source picture (or preset) range
      SMGL1=.FALSE.
      IF (LMSD) THEN
         IF (MEANSD(LP1)) GOTO 70
C
         IF (VSD.EQ.0.) THEN
            ERROR=12
            IDERR=IVALPN(NFROM)
            GOTO 70
         ENDIF
C
         A=VAL(NMS2)/VSD
         IF (A.LE.0.) THEN
            IDERR=NMSD
            GOTO 80
         ENDIF
         B=VAL(NMSD)-A*VMEAN
      ELSE
         IF (RANGE(1,LP1)) GOTO 70
         IF (ERROR.NE.0) FLSCAN=.FALSE.
         ERROR=0
         IF (VERB.EQ.NSCALE) THEN
            SB=VAL(NRANGE)
            ST=VAL(NRA2)
            IF (SB.EQ.ST) THEN
               IDERR=NRANGE
               GOTO 80
            ENDIF
         ELSE
            SB=VMAX
            ST=VMIN
         ENDIF
C
C        Check to see that there is structure in the picture
C
         IF ( VMAX .EQ. VMIN ) THEN
            ERROR=12
            IDERR=IVALPN(NFROM)
            GOTO 70
         ENDIF
         A=(ST-SB)/(VMAX-VMIN)
         B=SB-A*VMIN
      ENDIF
C
C Prepare map if source,output both byte
C
      IF (BOTHB) THEN
         P=0.
         DO 10 I=0,255
            T=MIN(MAX(A*P+B+.5,0.),255.)
            MAP(I)=T
            P=P+1.
   10    CONTINUE
      ENDIF
C
C Pass through data
C
      DO 60 L=1,NLAY
         DO 50 J=1,NROW
            IF (SEMROW(1,RB1,INFORM,J,L,LP1)) GOTO 70
C
C Apply map if source,output byte
C
            IF (BOTHB) THEN
               DO 20 I=1,NCOL
                  IB1(I)=MAP(IB1(I))
   20          CONTINUE
C
C Otherwise, scale directly, truncating if byte result
C
            ELSE IF (BYTEO) THEN
               DO 30 I=1,NC2
                  RB1(I)=AMIN1(AMAX1(A*RB1(I)+B,0.),255.)
   30          CONTINUE
            ELSE
               DO 40 I=1,NC2
                  RB1(I)=A*RB1(I)+B
   40          CONTINUE
            ENDIF
C
            IF (SEMROW(2,RB1,INFORM,J,L,LP2)) GOTO 70
   50    CONTINUE
   60 CONTINUE
C
C Record new MIN,MAX unless scan incomplete
C
      VMIN=A*VMIN+B
      VMAX=A*VMAX+B
      IF (BYTEO) THEN
         VMIN=MIN0(MAX1(VMIN+.5,0.),255)
         VMAX=MIN0(MAX1(VMAX+.5,0.),255)
      ENDIF
      IF (VMIN.GT.VMAX) THEN
         T=VMIN
         VMIN=VMAX
         VMAX=T
      ENDIF
      IF (FLSCAN) BYTEO=RANGE(4,LP2)
   70 RETURN
C
C Error
C
   80 ERROR=3
      GOTO 70
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
