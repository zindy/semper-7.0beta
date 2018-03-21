C Semper 6 standard processing module YMOD
C
      SUBROUTINE YMOD
C
C Displays a picture as an oblique scan 'perspective' view
C (intensity coded as surface height)
C
      LOGICAL DSPSUB,MAGSUB,SEMROW,OPTNO,FSLN61,SEMDPD,SEMCLS
      INTEGER IVAL
C
      INCLUDE 'COMMON'
C
C Packed names
C
      INTEGER NLETTE,NTIMES,NYMOD
      PARAMETER (NLETTE=19420,NTIMES=-374,NYMOD=-8536)
C
      LOGICAL IMPART
      INTEGER PNF,PND,FRAME,IFS
      INTEGER INFORM,MAGN,M1,M2,N1,N2
      INTEGER NCOL,NROW,NP1,NP2,NY,JS,NCS,NRS,NHOR,IS,IYC
      INTEGER MYMAX,MYMAX2,I,J,JJ,K,MX,MY,LX,LY
      REAL PBOT,SPSI,SCALE,XSC,YSC,YS,XSTART,ALX,ALY,BB
C
C Call DSPSUB - establishes range, opens display and adds text
C
      MAGN=IVAL(NTIMES)
      IF (DSPSUB(PNF,PND,0,OPTNO(NLETTE),INFORM,MAGN,M1,M2,N1,N2))
     +   GOTO 80
C
C Fault multi-layer sub-region
C
      IF (SMGI3.NE.SMGI6) THEN
         ERROR=62
         IDERR=NYMOD
         GOTO 80
      ENDIF
C
C Record display type 4 in DPD and close 'display' pic
C
      DPTYP=4
      IF (SEMDPD(2,PICN(LP2))) GOTO 80
      IF (SEMCLS(LP2)) GOTO 80
C
      IFS = DEVN(LP2)
C
C Note region size and form
C
      NCOL=NCOLS(LP2)
      NROW=NROWS(LP2)
      IF (NROW.EQ.1) THEN
         IDERR=PNF
         ERROR=5
         GOTO 80
      ENDIF
      IF (INFORM.NE.NFMCOM) INFORM=NFMFP
C
C Fix elevation at 0.3rad
C
      SPSI=SIN(0.3)
C
C Determine max possible surface dimensions
C
      NP1=NCOL+NROW
      NP2=NP1*(SPSI+0.2)
C
C Determine lateral scaling
C
      NY=DPSI2
      IF (INFORM.EQ.NFMCOM) NY=NY/2
      SCALE=AMIN1(REAL(DPSIZ)/REAL(NP1),REAL(NY)/REAL(NP2))
C
C Determine sampling so that min vector JS*SCALE is 3
C
      JS=3./SCALE
      IF (JS.LE.0) JS=1
C
C Number of (undersampled) points across field
C
   10 NCS=(NCOL-1)/JS+1
      NRS=(NROW-1)/JS+1
C
C Ensure horizon buffers adequate
C
      NHOR=NCS+NRS-1
      IF (NHOR.GT.LNBUF/LNCOMP) THEN
         JS=JS+1
         GOTO 10
      ENDIF
C
C Fault undersampling if magnifying as well
C
      IF (MAGN.GT.1 .AND. JS.GT.1) THEN
         ERROR=54
         IDERR=PND
         GOTO 80
      ENDIF
C
C Accommodate fp/complex form switch
C
      IS=JS
      IF (INFORM.EQ.NFMCOM) THEN
         IS=2*IS
         M1=2*M1-1
         M2=2*M2
      ENDIF
C
C Set display position for first part, and prepare second
C
      IYC=DPTLY+NY/2
      IMPART=.FALSE.
C
C Set in-plane and out-of-plane scaling
C
      XSC=-SCALE*JS
      YSC=XSC*SPSI
      MYMAX=NP1*0.1*SCALE
      MYMAX2=2*MYMAX
      YS=MYMAX2/(VMAX-VMIN)
C
      XSTART=DPTLX+DPSIZ/2+(.5*REAL(NHOR-1)-(NCS-1))*XSC
C
C Initialise horizons: upper in re(), lower in im()
C
      PBOT=DPTLY+DPSI2
   20 DO 30 I=1,NHOR
         RB2(2*I-1)=0.
         RB2(2*I)=PBOT
   30 CONTINUE
C
C Pass through data
C
      JJ=0
      DO 70 J=N1,N2,JS
         IF (MAGN.GT.1) THEN
            IF (MAGSUB(MAGN,RB1,J,SMGI3,.TRUE.)) GOTO 80
         ELSE
            IF (SEMROW(1,RB1,INFORM,J,SMGI3,LP1)) GOTO 80
         ENDIF
         ALY=YSC*JJ+IYC-.5*YSC*REAL(NHOR-1)-YSC
         ALX=XSTART-XSC*JJ-XSC
         K=NCS+JJ
         JJ=JJ+1
         DO 60 I=M1,M2,IS
            ALY=ALY+YSC
            ALX=ALX+XSC
            MX=ALX
            BB=RB1(I)
            MY=(BB-VMIN)*YS
            IF (MY.LT.0) MY=0
            IF (MY.GT.MYMAX2) MY=MYMAX2
            MY=MY-MYMAX
            MY=ALY-MY
            IF (I.EQ.M1) GOTO 50
            K=K-1
            BB=(MY+LY)/2
C
            IF (RB2(2*K-1).GT.BB) THEN
               IF (RB2(2*K).LT.BB) GOTO 50
               RB2(2*K)=BB
            ELSE
               RB2(2*K-1)=BB
               IF (RB2(2*K).GE.BB) RB2(2*K)=BB
            ENDIF
C
            IF (OVLIND(IFS)) THEN
               DO 40 FRAME=FRAME1(LP2),FRAME2(LP2)
                  IF (FSLN61(MX,MY,LX,LY,FRAME,ERROR)) GOTO 80
   40          CONTINUE
            ELSE
               IF (FSLN61(MX,MY,LX,LY,0,ERROR)) GOTO 80
            ENDIF
C
            REQFSF=.TRUE.
   50       LX=MX
            LY=MY
   60    CONTINUE
   70 CONTINUE
C
C Imaginary part too?
C
      IF (INFORM.EQ.NFMCOM .AND. .NOT.IMPART) THEN
         IMPART=.TRUE.
         M1=M1+1
         IYC=IYC+NY
         GOTO 20
      ENDIF
   80 RETURN
C
C Copyright (C) 1987,1988,1989,1990:  Synoptics Ltd, All Rights Reserved
C
      END
