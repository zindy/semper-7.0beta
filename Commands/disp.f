C Semper 6.3 processing module DISP
C
      SUBROUTINE DISP
C
C Displays LP1 in grey levels on the display (as a graph if 1-D or
C histogram; in character form if TERMINAL or LOG)
C
      INTEGER NCLEVS
      PARAMETER (NCLEVS=9)
C
      REAL VAL
C
      REAL BL,WH,PRANGE,YS,XS,ASPR,S,XLHS,XRHS,XP,DX,X,DY,CX,T
C
      CHARACTER*1 GREY(NCLEVS),CHA
      INTEGER CLASS,FORM,DEST,DGRAN,PNDIS
      INTEGER SAM
      INTEGER K,K1,K2,IK,IX,IY,IY0,I,J,L,N,IYO,M1,M2,N1,N2,NY,IX2,IY2,
     +   IBS,INFORM,IWID,ILEN,MODE,IB,MAGN,IVAL,NCOL,NROW,NLAY
C
      LOGICAL DSPSUB,GRAPH,MAGSUB,SEMROW,FSRO61,FSINIT,FSBORD,ABANDN
      LOGICAL VARSET,OPT,OPTNO,SEMLU,SEMDPD,SEMCLS,CONOPT,SEMCON,SEMLOG
      LOGICAL SEMTPS
      LOGICAL LTYPE,LLOG,IMPART,INTERP,ONED
      INTEGER NTYPE,NLOG,NASPEC,NTIMES,NREPLI,NLETTE
      INTEGER NBORDE,NHEIGH,NRE,NIM,NWIDTH
C
C Packed names
C
      PARAMETER (NTYPE=-1017,NLOG=19807)
      PARAMETER (NASPEC=2376,NTIMES=-374,NREPLI=29016,NLETTE=19420)
      PARAMETER (NBORDE=3818,NHEIGH=13009,NRE=29000,NIM=14920)
      PARAMETER (NWIDTH=-5165)
C
      INCLUDE 'COMMON'
C
      INTEGER MAXIPB
      PARAMETER (MAXIPB=LNBUF/LNINT)
      INTEGER CGXY(MAXIPB,2)
C
      EQUIVALENCE (RB2,CGXY)
      EQUIVALENCE (SMGR1,BL),(SMGR2,WH)
C
C ****** CHANGE ******
C Adjust these character definitions to suit local terminal types
      DATA GREY / ' ', '.', ',', '-', '+', 'o', 'h', 'H', '@' /
C
C ****** ****** ******
C
C Get options and check valid
C TYPE and LOG are exclusive
C
      IF (CONOPT(NLOG,NTYPE)) GOTO 80
C
      LTYPE = OPT(NTYPE)
      LLOG = OPT(NLOG)
C
      IF (LTYPE.OR.LLOG) THEN
         IF (LTYPE) DEST = 1
         IF (LLOG) DEST = 2
C
C Fetch current terminal page size
C
         IF (SEMTPS(IWID,ILEN)) GOTO 80
C
C See if WIDTH key determines output width and fault bad value
C
         IF (VARSET(NWIDTH)) THEN
            IWID = IVAL(NWIDTH)
C
            IF (IWID.LE.0.OR.IWID.GT.LEN(RECORD)) THEN
               ERROR = 3
               IDERR = NWIDTH
               GOTO 80
            ENDIF
         ENDIF
C
C Determine aspect ratio
C
         IF (VARSET(NASPEC)) THEN
            ASPR=VAL(NASPEC)
         ELSE
            ASPR=TERASP
         ENDIF
C
         IF (ASPR.LE.0.0) THEN
            ERROR = 3
            IDERR = NASPEC
            GOTO 80
         ENDIF
      ELSE
         DEST = 0
      ENDIF
C
      CLASS = CLASSN(LP1)
      MAGN = IVAL(NTIMES)
C
C Call DSPSUB - establishes region, black/white levels, opens display
C and adds text
C
      IF (DSPSUB(IX,PNDIS,DEST,OPTNO(NLETTE),FORM,MAGN,M1,M2,N1,N2))
     +   GOTO 80
      ONED=N1.EQ.N2
      INTERP=.NOT.OPT(NREPLI)
C
C Divert if character form
C
      IF (LTYPE.OR.LLOG) GOTO 90
C
C True display (non-character) modes
C ----------------------------------
C
C Note o/p parameters
C
      NCOL=NCOLS(LP2)
      NROW=NROWS(LP2)
      NLAY=NLAYS(LP2)
      SAM=PXSAM(LP2)
      IX=XLEFT(LP2)
      IY0=YTOP(LP2)
      DGRAN=GPSIZ(DEVN(LP2))
C
C Constrain form for less simple cases
C
      IF (MAGN.GT.1.OR.ONED) FORM=MAX(FORM,NFMFP)
C
C Loop over picture layers
C
      L=SMGI3
C
C Loop over picture rows
C
   10 J=N1
      K1=FRAME1(LP2)+L-SMGI3
      K2=FRAME2(LP2)
      IF (.NOT.ONED)  IY = IY0
   20 IF (MAGN.GT.1) THEN
         IF (MAGSUB(MAGN,RB1,J,L,INTERP)) GOTO 80
      ELSE
         IF (SEMROW(1,RB1,FORM,J,L,LP1)) GOTO 80
      ENDIF
      IF (.NOT.ONED) THEN
C
C 2-D display
C
         DO 50 K=K1,K2,NLAY
            IF (FSRO61(RB1,M1,M2,SAM,FORM,IX,IY,K,BL,WH,0,ERROR))
     +         GOTO 80
            IF (FORM.EQ.NFMCOM) THEN
               IF (FSRO61(RB1(2),M1,M2,SAM,FORM,IX+DPIMO,IY,K,BL,WH,
     +                    0,ERROR)) GOTO 80
            ENDIF
   50    CONTINUE
         IY=IY+DGRAN
         GOTO 70
      ENDIF
C
C 1-D graph: first time round, establish frame dimensions & y-pos
C
      IF (L.NE.SMGI3) GOTO 60
      NCOL=(NCOL-1)/SAM+1
      NROW=IVAL(NHEIGH)
      IF (NROW.LE.2.OR.NROW.GT.DPSI2) NROW=MIN0(NCOL,DPSI2)/2
      IF (NROW.LT.2) NROW = 2
      IY=DPTLY+(DPSI2-NROW)/2
C
C Close 'display'
C
      IF (SEMCLS(LP2)) GOTO 80
C
C Set graph mode, DPD type and Y axis position..
C
      IF (CLASS.NE.NCLHIS) THEN
C
C ..for line graph
C
         MODE=1
         DPTYP=2
C
C (note integer undersampling assumption)
C
         IYO=(CCOLN(LP1)-SMGI1)*MAGN/SAM+IX
      ELSE
C
C ..or histogram
C
         MODE=2
         DPTYP=3
         IYO=-1
C
C (reset DPD x-map to reflect pixel values rather than coords)
C
         DPMA=(NCOL-1)/(VMAX-VMIN)
         DPMB=IX-DPMA*VMIN
         DPLEF=VMIN
         DPRIG=VMAX
      ENDIF
C
C Set DPD y-map
C
      DPMA2=(NROW-1)/(BL-WH)
      DPMB2=IY-DPMA2*WH
      DPBOT=MIN(BL,WH)
      DPTOP=MAX(BL,WH)
      IF (SEMDPD(2,PICN(LP2))) GOTO 80
C
C Better lettering in due course?
C
      IF (FORM.EQ.NFMCOM) THEN
         M1=2*M1-1
         M2=2*M2
         SAM=2*SAM
      ENDIF
   60 IF (GRAPH(MODE,RB1,M1,M2,SAM,IX,IY,K1,K2,NLAY,
     +          BL,WH,NROW,IYO)) GOTO 80
      IF (FORM.EQ.NFMCOM) THEN
         IF (GRAPH(MODE,RB1,M1+1,M2,SAM,IX+DPIMO,IY,K1,K2,NLAY,
     +             BL,WH,NROW,IYO+DPIMO)) GOTO 80
      ENDIF
C
C End of row loop
C
   70 J=J+SAM
      IF (J.LE.N2) GOTO 20
C
C End of layer loop
C
      L=L+1
      IF (L.LE.SMGI6) GOTO 10
C
C Frame result
C
      IF (.NOT.OPTNO(NBORDE)) THEN
         IF (SEMLU(2,NRE,0.)) GOTO 80
         IF (SEMLU(2,NIM,0.)) GOTO 80
         IF (FSINIT(3,PNDIS)) GOTO 80
         IF (FSBORD()) GOTO 80
      ENDIF
C
   80 CONTINUE
      RETURN
C
C Character modes
C ---------------
C Prepare grey scaling parameters for 2-D case
C
   90 PRANGE=WH-BL
      YS=(NCLEVS-.02)/PRANGE
      XS=(1.01*WH-(NCLEVS+.99)*BL)/PRANGE
C
C Establish form and pixel subscript increment
C
      INFORM=FORM
      IF (INFORM.NE.NFMCOM) INFORM=NFMFP
C
C Establish (non-integral) sampling
C
      SAM=1
C
  100 IF (ONED) THEN
         NCOL=1+(M2-M1)/SAM
      ELSE
         S=REAL(SAM)/ASPR
         NCOL=1+INT(REAL(M2-M1)/S)
      ENDIF
C
      IF (NCOL.GT.IWID) THEN
         SAM=SAM+1
         GOTO 100
      ENDIF
C
      XLHS=REAL(M1)
      XRHS=REAL(M2)
C
C Pass through picture rows, undersampling if necessary
C
      IF (MAGN.GT.1.AND.SAM.NE.1) THEN
         ERROR = 54
         GOTO 80
      ENDIF
C
      IMPART=.FALSE.
C
C Loop over layers
C
  110 L=SMGI3
C
C Loop over rows
C
  120 J=N1
  130 IF (MAGN.GT.1) THEN
         IF (MAGSUB(MAGN,RB1,J,L,INTERP)) GOTO 80
      ELSE
         IF (SEMROW(1,RB1,INFORM,J,L,LP1)) GOTO 80
      ENDIF
      IF (.NOT.ONED) THEN
C
C 2-D display: pass along row obtaining interpolated values
C
         NCOL=1
         RECORD(1:1)=' '
         IX=0
         XP=XLHS
  140    I=INT(XP)
         X=XP-REAL(I)
         CX=1.-X
         IF (INFORM.EQ.NFMCOM) THEN
            IF (IMPART) THEN
               I=I+I
            ELSE
               I=I+I-1
            ENDIF
            T=RB1(I)*CX+RB1(I+2)*X
         ELSE
            T=RB1(I)*CX+RB1(I+1)*X
         ENDIF
         K=INT(YS*T+XS)
C
C Convert value to corresponding graphic
C
         IF (K.LT.1) K=1
         IF (K.GT.NCLEVS) K=NCLEVS
C
         CHA=GREY(K)
C
         IX=IX+1
         RECORD(IX:IX)=CHA
C
         IF (CHA.NE.' ') NCOL=IX
C
C Next pixel?
C
         XP=XP+S
         IF (XP.LE.XRHS) GOTO 140
C
C Output line of char graphic grey levels
C
         IF (LTYPE) THEN
            IF (SEMCON(RECORD(1:NCOL))) GOTO 80
         ENDIF
C
         IF (LLOG) THEN
            IF (SEMLOG(RECORD(1:NCOL))) GOTO 80
         ENDIF
      ELSE
C
C 1-D graph: convert graph to list of terminal col,row numbers to mark
C
C Initialise
C
         NROW=NINT(REAL(NCOL/2)/ASPR)
         NROW=MAX(MIN(NROW,TERLEN-4),5)
         DX=REAL(NCOL-1)/(M2-M1)
         DY=REAL(NROW-1)/PRANGE
         K=0
C
C Begin loop over source line segments
C
         DO 160 I=M1,M2
            X=1.0+REAL(I-M1)*DX
C
C Set IX,2 IY,2 to segment start,end wrt terminal (X across, Y down)
C
            IX=NINT(X)
            IX2=NINT(X+DX)
            IB=I
            IBS=SAM
            IF (INFORM.EQ.NFMCOM) THEN
               IF (IMPART) THEN
                  IB=IB+IB
               ELSE
                  IB=IB+IB-1
               ENDIF
               IBS=IBS+IBS
            ENDIF
            IY=1+NINT((WH-RB1(IB))*DY)
            IF (IB+IBS.LE.M2) THEN
               IY2=1+NINT((WH-RB1(IB+IBS))*DY)
            ELSE
               IY2=IY
            ENDIF
C
C Set vertical line length, increment
C
            NY=MAX(1,ABS(IY2-IY))
            IF (IY2.GE.IY) THEN
               IY2=1
               N=NY/2
            ELSE
               IY2=-1
               N=NY/2+1
            ENDIF
C
C Override with single point if histogram
C
            IF (CLASSN(LP1).EQ.NCLHIS) THEN
               NY=1
               IX=(I-M1)/SAM+1
            ENDIF
C
C List X,Y values for segment
C
            DO 150 IK=1,NY
               IF (K.LT.MAXIPB) THEN
                  K=K+1
                  CGXY(K,1)=IX
                  CGXY(K,2)=IY
               ENDIF
               IF (IK.EQ.N) IX=IX2
               IY=IY+IY2
  150       CONTINUE
  160    CONTINUE
C
C Note Y value for horizontal axis
C
         IYO=NINT(WH*DY)+1.
C
C Loop over terminal rows, marking listed pixels
C
         DO 190 IY=1,NROW
C
C Initialise buffer with frame elements
C
            IF (IY.EQ.NROW.OR.IY.EQ.1.OR.IY.EQ.IYO) THEN
               CHA='-'
            ELSE
               CHA=' '
            ENDIF
C
            RECORD(1:1)='|'
C
            DO 170 IX=2,NCOL-1
               RECORD(IX:IX)=CHA
  170       CONTINUE
C
            RECORD(NCOL:NCOL)='|'
C
            IX=(CCOLN(LP1)-SMGI1)*MAGN/SAM+1
            IF (IX.GT.1.AND.IX.LT.NCOL) RECORD(IX:IX)='|'
C
C Scan data for intersections with this row
C
            DO 180 I=1,K
               IF (CLASSN(LP1).EQ.NCLHIS) THEN
                  IF (CGXY(I,2).GT.IY) GOTO 180
               ELSE
                  IF (CGXY(I,2).NE.IY) GOTO 180
               ENDIF
               IX=CGXY(I,1)
               RECORD(IX:IX)='*'
  180       CONTINUE
C
            IF (LTYPE) THEN
               IF (SEMCON(RECORD(1:NCOL))) GOTO 80
            ENDIF
C
            IF (LLOG) THEN
               IF (SEMLOG(RECORD(1:NCOL))) GOTO 80
            ENDIF
            IF (ABANDN(ERROR)) GOTO 80
  190    CONTINUE
      ENDIF
C
C End of row loop
C
      J=J+SAM
      IF (J.LE.N2) GOTO 130
C
C End of loop over layers
C
      L=L+1
      IF (L.LE.SMGI6) GOTO 120
C
C Repeat char display for im part?
C
      IF (.NOT.IMPART.AND.INFORM.EQ.NFMCOM) THEN
C
         IF (LTYPE) THEN
            IF (SEMCON('Im part:')) GOTO 80
         ENDIF
C
         IF (LLOG) THEN
            IF (SEMLOG('Im part:')) GOTO 80
         ENDIF
C
         IMPART=.TRUE.
         GOTO 110
      ENDIF
C
      GOTO 80
C
C Copyright (C) 1987-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
