C Semper 6 processing module ANALYS
C
      SUBROUTINE ANALYS
C
      LOGICAL VARSET,TSTSRG,SEMOPN,SEMROW,SEMLAB,ANALY2,ANALY6,ANALY7
      LOGICAL OPT,OPTNO,SEMLU,MRKREG,MARSET,FSINIT,FSTEXT,FSLIST,SEMCON
      LOGICAL SEMRNG
      INTEGER SEMFRM,IVALPN
      REAL ANALY8,VAL
C
      INCLUDE 'COMMON'
C
C Indices for integer chord parameters
C
      INTEGER NIDENT,NFORWP,NBACKP,NCOL1,NCOL2,NCMIN,NCMAX,NRMIN
      INTEGER NX1MIN,NX1MAX,NX2MIN,NX2MAX,NVERT1,NVERT2
      PARAMETER (NIDENT=1)
      PARAMETER (NFORWP=NIDENT+1)
      PARAMETER (NBACKP=NFORWP+1)
      PARAMETER (NCOL1=NBACKP+1)
      PARAMETER (NCOL2=NCOL1+1)
      PARAMETER (NCMIN=NCOL2+1)
      PARAMETER (NCMAX=NCMIN+1)
      PARAMETER (NRMIN=NCMAX+1)
      PARAMETER (NX1MIN=NRMIN+1)
      PARAMETER (NX1MAX=NX1MIN+1)
      PARAMETER (NX2MIN=NX1MAX+1)
      PARAMETER (NX2MAX=NX2MIN+1)
      PARAMETER (NVERT1=NX2MAX+1)
      PARAMETER (NVERT2=NVERT1+1)
C
C Indices for floating point chord parameters
C
      INTEGER NHOLES,NHPROJ,NVPROJ,NPERIM,NSUM,NSUMC,NSUMR
      INTEGER NSUMCC,NSUMRR,NSUMCR
      PARAMETER (NHOLES=1)
      PARAMETER (NHPROJ=NHOLES+1)
      PARAMETER (NVPROJ=NHPROJ+1)
      PARAMETER (NPERIM=NVPROJ+1)
      PARAMETER (NSUM=NPERIM+1)
      PARAMETER (NSUMC=NSUM+1)
      PARAMETER (NSUMR=NSUMC+1)
      PARAMETER (NSUMCC=NSUMR+1)
      PARAMETER (NSUMRR=NSUMCC+1)
      PARAMETER (NSUMCR=NSUMRR+1)
C
C Number of integer and real parameters for each chord
C
      INTEGER NIPAR,NRPAR
      PARAMETER (NIPAR=NVERT2)
      PARAMETER (NRPAR=NSUMCR)
C
C Maximum number of chords in chord buffer
C
      INTEGER NCHORD
      PARAMETER (NCHORD=4*(LNBUF/(NIPAR*LNINT+NRPAR*LNREAL)))
C
C Number of parameters for each particle
C
      INTEGER NPPAR
      PARAMETER (NPPAR=25)
C
C Maximum number of blocks of particle data that can be stored in
C five row buffers
C
      INTEGER NPBLK,NSQRT
      PARAMETER (NPBLK=5*((LNBUF-(LNBLK/5))/(NPPAR*LNREAL)))
C
      PARAMETER (NSQRT=2*LNEDGE)
C
C Local variables
C
C
      REAL AREA(2),CORNER,PMIN,PMAX
      INTEGER FORM,OLDID,IDIG(0:9),TEXT(5)
      INTEGER I,J,J1,J2,K,K1,K2,L,M,N,LNBUFS
      INTEGER IH,IV,KN,KO,KOF,KOP,KNF,KNP,KOB
      INTEGER ICOL1,ICOL2,LPT1,LPT2,MARK,MODE,MAXID,NEWID,NPPL
      INTEGER NCOL,NROW,NPAR,NSEG,NCHD,NSTART,NEND,NOVER
      INTEGER LASCHD,LASPAR,KOLD,NOLD,KNEW,NNEW
      LOGICAL LSEG,ANNOT,LVERIF
C
      INTEGER ICOVER(NCHORD,4),LABEL(256),IB1(LNBUF/LNINT)
      INTEGER IB2(LNBUF/LNINT)
      REAL    RPBUFF(NPPAR,NPBLK)
      INTEGER ICBUFF(NCHORD,NIPAR)
      REAL    RCBUFF(NCHORD,NRPAR),RB3456(4*(LNBUF/LNREAL))
      REAL    SQRT1(NSQRT)
C
      EQUIVALENCE (ICOVER,LABEL,IB1,RB1)
      EQUIVALENCE (IB2,RPBUFF,RB2)
      EQUIVALENCE (RCBUFF,RB3456,RB3)
      EQUIVALENCE (ICBUFF,RB3456(NCHORD*NRPAR+1))
      EQUIVALENCE (SQRT1,RB1RHS)
C
C Packed names
C
      INTEGER NGE,NLE,NTO,NSEGME,NAREA,NAR2,NCM,NID,NFROM
      INTEGER NPIMAG,NPPLIS,NPSEGM,NVERIF,NN
      PARAMETER (NGE=11400,NLE=19400,NTO=-601,NSEGME=30607)
      PARAMETER (NAREA=2325,NAR2=2352,NCM=5320,NID=14560,NFROM=10335)
      PARAMETER (NPIMAG=25973,NPPLIS=26252,NPSEGM=26365,NN=22400)
      PARAMETER (NVERIF=-3419)
C
      DATA IDIG /KZERO,KONE,KTWO,KTHREE,KFOUR,
     +           KFIVE,KSIX,KSEVEN,KEIGHT,KNINE/
C
      MAXID=LNBUF/LNINT
C
      CORNER=SQRT(2.0)/2.0
C
      DO 10 I=1,NSQRT
         SQRT1(I)=SQRT(1.0+REAL(I)*REAL(I))
   10 CONTINUE
C
      MODE=0
C
      IF (VARSET(NGE)) THEN
         MODE=MODE+1
      ENDIF
C
      IF (VARSET(NLE)) THEN
         MODE=MODE+2
      ENDIF
C
      IF (MODE.EQ.0) THEN
         IF (SEMRNG(1,PMIN,PMAX,LP1)) GOTO 260
         PMIN=(PMIN+PMAX)/2.0
         MODE=1
      ELSE
         PMIN=VAL(NGE)
         PMAX=VAL(NLE)
C
         IF (MODE.EQ.3) THEN
            IF (PMIN.GT.PMAX) MODE=4
         ENDIF
      ENDIF
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
C
      IF (VARSET(NAREA)) THEN
         AREA(1)=VAL(NAREA)
      ELSE
         AREA(1)=0.0
      ENDIF
C
      IF (VARSET(NAR2)) THEN
         AREA(2)=VAL(NAR2)
      ELSE
         AREA(2)=REAL(NCOL)*REAL(NROW)
      ENDIF
C
      IF (AREA(1).GT.AREA(2)) THEN
         ERROR=3
         IDERR=NAREA
         GOTO 260
      ENDIF
C
      IF (TSTSRG(1,LP1)) GOTO 260
C
      IF (MRKREG(0)) GOTO 260
C
      LSEG=VARSET(NSEGME)
C
      IF (LSEG) THEN
         NSEG=IVALPN(NSEGME)
         FORM=SEMFRM(NFMINT)
         LP2=LP1
         IF (SEMOPN(2,NSEG,NCOL,NROW,1,NCLIMA,FORM,LP2)) GOTO 260
C
         IF (FORM.EQ.NFMBYT) MAXID=255
      ENDIF
C
      LNBUFS = SHORTF(LNBUF)
      LPT1=0
      IF (SEMOPN(3,0,LNBUFS,2,1,NCLIMA,NFMBYT,LPT1))
     +   GOTO 260
C
      LPT2=0
      IF (SEMOPN(3,0,NPPAR,256,1,NCLIMA,NFMFP,LPT2)) GOTO 260
C
      NPAR=0
      NCHD=0
      LASPAR=0
      LASCHD=0
C
      DO 20 I=1,MAXID
         IB2(I)=I
   20 CONTINUE
C
      KOLD=1
      NOLD=0
C
      DO 180 J=1,NROW+1
C
         KNEW=KOLD+NOLD
         IF (KNEW.GT.NCHORD) KNEW=KNEW-NCHORD
C
         IF (J.GT.NROW) THEN
C
            NNEW=0
C
         ELSE
C
            IF (ANALY2(J,PMIN,PMAX,MODE,NCHORD-NOLD,KNEW,NNEW)) GOTO 260
         ENDIF
C
         CALL ANALY3(KOLD,NOLD,KNEW,NNEW,NSTART,NEND,NOVER)
C
         DO 30 K=1,NSTART
C
            KN=ICOVER(K,1)
C
            NCHD=NCHD+1
C
            ICBUFF(KN,NIDENT)=NCHD
C
            ICBUFF(KN,NFORWP)=KN
            ICBUFF(KN,NBACKP)=KN
C
            RCBUFF(KN,NPERIM)=
     +         ANALY8(ICBUFF(KN,NCOL2)-ICBUFF(KN,NCOL1)+1)
   30    CONTINUE
C
         DO 50 K=1,NEND
C
            KO=ICOVER(K,2)
C
            IF (ICBUFF(KO,NVERT1).NE.0) RCBUFF(KO,NPERIM)=
     +         RCBUFF(KO,NPERIM)+ANALY8(ICBUFF(KO,NVERT1))-CORNER
C
            IF (ICBUFF(KO,NVERT2).NE.0) RCBUFF(KO,NPERIM)=
     +         RCBUFF(KO,NPERIM)+ANALY8(ICBUFF(KO,NVERT2))-CORNER
C
            RCBUFF(KO,NPERIM)=RCBUFF(KO,NPERIM)+
     +         ANALY8(ICBUFF(KO,NCOL2)-ICBUFF(KO,NCOL1)+1)
C
            KOF=ICBUFF(KO,NFORWP)
C
            IF (KOF.EQ.KO) THEN
C
               OLDID=ICBUFF(KO,NIDENT)
C
               IF (ANALY6(NPAR,KO,J-1,AREA,LPT1,LPT2)) GOTO 260
C
               NEWID=ICBUFF(KO,NIDENT)
C
               DO 40 I=1,LASCHD
                  IF (IB2(I).EQ.OLDID) IB2(I)=-NEWID
   40          CONTINUE
C
            ELSE
C
               CALL ANALY5(KOF,KO)
C
               KOB=ICBUFF(KO,NBACKP)
C
               ICBUFF(KOB,NFORWP)=KOF
               ICBUFF(KOF,NBACKP)=KOB
            ENDIF
C
            ICBUFF(KO,NIDENT)=0
   50    CONTINUE
C
         KO=-1
         KN=-1
C
         DO 80 K=1,NOVER+1
C
            KOP=KO
            KNP=KN
C
            IF (K.GT.NOVER) THEN
               KO=0
               KN=0
            ELSE
               KO=ICOVER(K,3)
               KN=ICOVER(K,4)
            ENDIF
C
            IF (KO.EQ.KOP) THEN
C
               ICBUFF(KN,NIDENT)=ICBUFF(KO,NIDENT)
C
               KNF=ICBUFF(KNP,NFORWP)
C
               ICBUFF(KNP,NFORWP)=KN
               ICBUFF(KN,NBACKP)=KNP
               ICBUFF(KN,NFORWP)=KNF
               ICBUFF(KNF,NBACKP)=KN
C
               RCBUFF(KN,NPERIM)=
     +            ANALY8(ICBUFF(KN,NCOL1)-ICBUFF(KNP,NCOL2)-1)
C
            ELSE IF (KN.EQ.KNP) THEN
C
               IF (ICBUFF(KO,NIDENT).EQ.ICBUFF(KOP,NIDENT)) THEN
C
                  RCBUFF(KO,NHOLES)=RCBUFF(KO,NHOLES)+1.0
C
               ELSE
C
                  OLDID=ICBUFF(KO,NIDENT)
                  NEWID=ICBUFF(KN,NIDENT)
C
                  DO 60 I=1,LASCHD
                     IF (IB2(I).EQ.OLDID) IB2(I)=NEWID
   60             CONTINUE
C
   70             IF (ICBUFF(KO,NIDENT).EQ.OLDID) THEN
C
                     ICBUFF(KO,NIDENT)=NEWID
C
                     KO=ICBUFF(KO,NFORWP)
C
                     GOTO 70
                  ENDIF
C
                  KOF=ICBUFF(KOP,NFORWP)
                  KOB=ICBUFF(KO,NBACKP)
C
                  ICBUFF(KOP,NFORWP)=KO
                  ICBUFF(KO,NBACKP)=KOP
                  ICBUFF(KOB,NFORWP)=KOF
                  ICBUFF(KOF,NBACKP)=KOB
               ENDIF
C
               IF (ICBUFF(KOP,NVERT2).NE.0) RCBUFF(KO,NPERIM)=
     +            RCBUFF(KO,NPERIM)+ANALY8(ICBUFF(KOP,NVERT2))-CORNER
C
               IF (ICBUFF(KO,NVERT1).NE.0) RCBUFF(KO,NPERIM)=
     +            RCBUFF(KO,NPERIM)+ANALY8(ICBUFF(KO,NVERT1))-CORNER
C
               RCBUFF(KO,NPERIM)=RCBUFF(KO,NPERIM)+
     +            ANALY8(ICBUFF(KO,NCOL1)-ICBUFF(KOP,NCOL2)-1)
C
               CALL ANALY5(KN,KO)
C
            ELSE
C
               IF (K.GE.2) THEN
C
                  IH=ABS(ICBUFF(KNP,NCOL2)-ICBUFF(KOP,NCOL2))
                  IV=ICBUFF(KOP,NVERT2)
C
                  IF (IV.EQ.0) THEN
C
                     IF (IH.EQ.0) THEN
C
                        ICBUFF(KNP,NVERT2)=2
C
                        RCBUFF(KNP,NPERIM)=RCBUFF(KNP,NPERIM)-CORNER
C
                     ELSE
C
                        ICBUFF(KNP,NVERT2)=0
C
                        RCBUFF(KNP,NPERIM)=RCBUFF(KNP,NPERIM)+ANALY8(IH)
                     ENDIF
C
                  ELSE
C
                     IF (IH.EQ.0) THEN
C
                        ICBUFF(KNP,NVERT2)=IV+1
C
                     ELSE IF (IH.EQ.1) THEN
C
                        ICBUFF(KNP,NVERT2)=1
C
                        RCBUFF(KNP,NPERIM)=RCBUFF(KNP,NPERIM)+ANALY8(IV)
C
                     ELSE
C
                        ICBUFF(KNP,NVERT2)=0
C
                        RCBUFF(KNP,NPERIM)=RCBUFF(KNP,NPERIM)+
     +                     ANALY8(IV)+ANALY8(IH)-CORNER
                     ENDIF
                  ENDIF
               ENDIF
C
               IF (K.LE.NOVER) THEN
C
                  ICBUFF(KN,NIDENT)=ICBUFF(KO,NIDENT)
C
                  KOF=ICBUFF(KO,NFORWP)
C
                  ICBUFF(KO,NFORWP)=KN
                  ICBUFF(KN,NBACKP)=KO
                  ICBUFF(KN,NFORWP)=KOF
                  ICBUFF(KOF,NBACKP)=KN
C
                  IH=ABS(ICBUFF(KN,NCOL1)-ICBUFF(KO,NCOL1))
                  IV=ICBUFF(KO,NVERT1)
C
                  IF (IV.EQ.0) THEN
C
                     IF (IH.EQ.0) THEN
C
                        ICBUFF(KN,NVERT1)=2
C
                        RCBUFF(KN,NPERIM)=-CORNER
C
                     ELSE
C
                        ICBUFF(KN,NVERT1)=0
C
                        RCBUFF(KN,NPERIM)=ANALY8(IH)
                     ENDIF
C
                  ELSE
C
                     IF (IH.EQ.0) THEN
C
                        ICBUFF(KN,NVERT1)=IV+1
C
                        RCBUFF(KN,NPERIM)=0.0
C
                     ELSE IF (IH.EQ.1) THEN
C
                        ICBUFF(KN,NVERT1)=1
C
                        RCBUFF(KN,NPERIM)=ANALY8(IV)
C
                     ELSE
C
                        ICBUFF(KN,NVERT1)=0
C
                        RCBUFF(KN,NPERIM)=ANALY8(IV)+ANALY8(IH)-CORNER
                     ENDIF
                  ENDIF
C
                  CALL ANALY5(KN,KO)
               ENDIF
            ENDIF
   80    CONTINUE
C
         KO=KOLD
C
         DO 90 K=1,NOLD
C
            IF (ICBUFF(KO,NIDENT).NE.0) THEN
C
               KOF=ICBUFF(KO,NFORWP)
               KOB=ICBUFF(KO,NBACKP)
C
               ICBUFF(KOB,NFORWP)=KOF
               ICBUFF(KOF,NBACKP)=KOB
            ENDIF
C
            KO=KO+1
            IF (KO.GT.NCHORD) KO=1
   90    CONTINUE
C
         IF (NCHD+LASPAR.GT.MAXID.OR.J.GT.NROW) THEN
C
            NCHD=0
C
            DO 100 I=1,LASCHD
C
               IF (IB2(I).LE.0) THEN
C
                  IB2(I)=-IB2(I)
C
               ELSE IF (IB2(I).EQ.I) THEN
C
                  NCHD=NCHD+1
C
                  IB2(I)=NCHD+NPAR
C
               ELSE
C
                  IB2(I)=-(IB2(I))
               ENDIF
  100       CONTINUE
C
            DO 110 I=1,LASCHD
C
               IF (IB2(I).LT.0) IB2(I)=IB2(-IB2(I))
  110       CONTINUE
C
            DO 120 K=1,NSTART
C
               KN=ICOVER(K,1)
C
               ICBUFF(KN,NIDENT)=0
  120       CONTINUE
C
            KN=KNEW
C
            DO 130 K=1,NNEW
C
               IF (ICBUFF(KN,NIDENT).EQ.0) THEN
C
                  NCHD=NCHD+1
C
                  ICBUFF(KN,NIDENT)=NCHD
C
               ELSE
C
                  ICBUFF(KN,NIDENT)=IB2(ICBUFF(KN,NIDENT))-NPAR
               ENDIF
C
               KN=KN+1
               IF (KN.GT.NCHORD) KN=1
  130       CONTINUE
C
            IF (LSEG) THEN
C
               IF (ANALY7(J-1,LASPAR)) GOTO 260
            ENDIF
C
            LASPAR=NPAR
C
            DO 140 I=1,MAXID
               IB2(I)=I
  140       CONTINUE
         ENDIF
C
         IF (LSEG.AND.J.LE.NROW) THEN
C
            DO 150 I=1,NCOL
               IB1(I)=0
  150       CONTINUE
C
            KN=KNEW
C
            DO 170 K=1,NNEW
C
               NEWID=ICBUFF(KN,NIDENT)+LASPAR
C
               IF (NEWID.GT.MAXID) THEN
                  ERROR=153
                  GOTO 260
               ENDIF
C
               ICOL1=ICBUFF(KN,NCOL1)
               ICOL2=ICBUFF(KN,NCOL2)
C
               DO 160 I=ICOL1,ICOL2
                  IB1(I)=NEWID
  160          CONTINUE
C
               KN=KN+1
               IF (KN.GT.NCHORD) KN=1
  170       CONTINUE
C
            IF (SEMROW(2,RB1,NFMINT,J,1,LP2)) GOTO 260
         ENDIF
C
         LASCHD=NCHD
C
         KOLD=KNEW
         NOLD=NNEW
  180 CONTINUE
C
C See if verification is required (default = yes)
C
      LVERIF=.NOT.OPTNO(NVERIF)
C
      IF (NPAR.EQ.0) THEN
C
         IF (LVERIF) THEN
            IF (SEMCON('No particles found')) GOTO 260
         ENDIF
      ELSE
C
         IF (LVERIF) THEN
            WRITE (RECORD,270) NPAR
            IF (SEMCON(RECORD)) GOTO 260
         ENDIF
C
         NPPL=IVALPN(NTO)
         LP3=0
         IF (SEMOPN(2,NPPL,NPAR,1,NPPAR,NCLPLI,NFMFP,LP3)) GOTO 260
C
         IF (SEMLAB(1,LABEL,LP3)) GOTO 260
C
         LABEL(LBPLTY)=1
C
         IF (SEMLAB(2,LABEL,LP3)) GOTO 260
C
         DO 220 N=1,NPAR,NPBLK
C
            J1=N
            J2=MIN(N+NPBLK-1,NPAR)
C
            M=0
C
            DO 190 J=J1,J2
C
               M=M+1
C
               IF (SEMROW(1,RPBUFF(1,M),NFMFP,J,1,LPT2)) GOTO 260
  190       CONTINUE
C
            DO 210 K=1,NPPAR
C
               IF (N.NE.1) THEN
                  IF (SEMROW(1,RB1,NFMFP,1,K,LP3)) GOTO 260
               ENDIF
C
               M = 0
C
               DO 200 J=J1,J2
                  M = M + 1
                  RB1(J) = RPBUFF(K,M)
  200          CONTINUE
C
               IF (SEMROW(2,RB1,NFMFP,1,K,LP3)) GOTO 260
  210       CONTINUE
  220    CONTINUE
C
         IF (MARSET(ANNOT,MARK)) GOTO 260
C
         IF (ANNOT) THEN
C
            IF (FSINIT(3,MARK)) GOTO 260
C
            IF (FSPTYP.EQ.1) THEN
C
               IF (OPT(NCM)) THEN
                  K1=20
                  K2=21
               ELSE
                  K1=1
                  K2=2
               ENDIF
C
               IF (SEMROW(1,RB1,NFMFP,1,K1,LP3)) GOTO 260
               IF (SEMROW(1,RB2,NFMFP,1,K2,LP3)) GOTO 260
C
               IF (OPT(NID)) THEN
C
                  DO 250 I=1,NPAR
C
                     M=10000
                     N=5
C
  230                IF (I.LT.M) THEN
                        M=M/10
                        N=N-1
                        GOTO 230
                     ENDIF
C
                     M=I
C
                     DO 240 J=N,1,-1
                        L=M/10
                        TEXT(J)=IDIG(M-10*L)
                        M=L
  240                CONTINUE
C
                     IF (FSTEXT(TEXT,N,RB1(I),RB2(I),0,0)) GOTO 260
  250             CONTINUE
C
               ELSE
C
                  IF (FSLIST(RB1,RB2,NPAR,FSMMOD,FSMSIZ)) GOTO 260
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
      IF (SEMLU(1,NPIMAG,REAL(IVALPN(NFROM)))) GOTO 260
C
      IF (NPAR.EQ.0) THEN
         IF (SEMLU(0,NPPLIS,0.0)) GOTO 260
      ELSE
         IF (SEMLU(1,NPPLIS,REAL(NPPL))) GOTO 260
      ENDIF
C
      IF (LSEG) THEN
         IF (SEMLU(1,NPSEGM,REAL(NSEG))) GOTO 260
      ELSE
         IF (SEMLU(0,NPSEGM,0.0)) GOTO 260
      ENDIF
C
      IF (SEMLU(1,NN,REAL(NPAR))) GOTO 260
C
  260 RETURN
C
  270 FORMAT ('Number of particles found:',I5)
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module ANALY5
C
      SUBROUTINE ANALY5(ICHORD,JCHORD)
      INTEGER ICHORD,JCHORD
C
C Combines the contributions from chords ICHORD and JCHORD and stores
C the result in chord ICHORD.
C
      INCLUDE 'COMMON'
C
C Indices for integer chord parameters
C
      INTEGER NIDENT,NFORWP,NBACKP,NCOL1,NCOL2,NCMIN,NCMAX,NRMIN
      INTEGER NX1MIN,NX1MAX,NX2MIN,NX2MAX,NVERT1,NVERT2
      PARAMETER (NIDENT=1)
      PARAMETER (NFORWP=NIDENT+1)
      PARAMETER (NBACKP=NFORWP+1)
      PARAMETER (NCOL1=NBACKP+1)
      PARAMETER (NCOL2=NCOL1+1)
      PARAMETER (NCMIN=NCOL2+1)
      PARAMETER (NCMAX=NCMIN+1)
      PARAMETER (NRMIN=NCMAX+1)
      PARAMETER (NX1MIN=NRMIN+1)
      PARAMETER (NX1MAX=NX1MIN+1)
      PARAMETER (NX2MIN=NX1MAX+1)
      PARAMETER (NX2MAX=NX2MIN+1)
      PARAMETER (NVERT1=NX2MAX+1)
      PARAMETER (NVERT2=NVERT1+1)
C
C Indices for floating point chord parameters
C
      INTEGER NHOLES,NHPROJ,NVPROJ,NPERIM,NSUM,NSUMC,NSUMR
      INTEGER NSUMCC,NSUMRR,NSUMCR
      PARAMETER (NHOLES=1)
      PARAMETER (NHPROJ=NHOLES+1)
      PARAMETER (NVPROJ=NHPROJ+1)
      PARAMETER (NPERIM=NVPROJ+1)
      PARAMETER (NSUM=NPERIM+1)
      PARAMETER (NSUMC=NSUM+1)
      PARAMETER (NSUMR=NSUMC+1)
      PARAMETER (NSUMCC=NSUMR+1)
      PARAMETER (NSUMRR=NSUMCC+1)
      PARAMETER (NSUMCR=NSUMRR+1)
C
C Number of integer and real parameters for each chord
C
      INTEGER NIPAR,NRPAR
      PARAMETER (NIPAR=NVERT2)
      PARAMETER (NRPAR=NSUMCR)
C
C Maximum number of chords in chord buffer
C
      INTEGER NCHORD
      PARAMETER (NCHORD=4*(LNBUF/(NIPAR*LNINT+NRPAR*LNREAL)))
C
      INTEGER ICBUFF(NCHORD,NIPAR)
      REAL    RCBUFF(NCHORD,NRPAR),RB3456(4*(LNBUF/LNREAL))
C
      EQUIVALENCE (RCBUFF,RB3456,RB3)
      EQUIVALENCE (ICBUFF,RB3456(NCHORD*NRPAR+1))
C
C Determine minimum and maximum column numbers for chords
C
      IF (ICBUFF(JCHORD,NCMIN).LT.ICBUFF(ICHORD,NCMIN))
     +   ICBUFF(ICHORD,NCMIN)=ICBUFF(JCHORD,NCMIN)
      IF (ICBUFF(JCHORD,NCMAX).GT.ICBUFF(ICHORD,NCMAX))
     +   ICBUFF(ICHORD,NCMAX)=ICBUFF(JCHORD,NCMAX)
C
C Determine minimum row number for chords
C
      IF (ICBUFF(JCHORD,NRMIN).LT.ICBUFF(ICHORD,NRMIN))
     +   ICBUFF(ICHORD,NRMIN)=ICBUFF(JCHORD,NRMIN)
C
C Determine minimum and maximum (column-row) and (column+row) for chords
C
      IF (ICBUFF(JCHORD,NX1MIN).LT.ICBUFF(ICHORD,NX1MIN))
     +   ICBUFF(ICHORD,NX1MIN)=ICBUFF(JCHORD,NX1MIN)
      IF (ICBUFF(JCHORD,NX1MAX).GT.ICBUFF(ICHORD,NX1MAX))
     +   ICBUFF(ICHORD,NX1MAX)=ICBUFF(JCHORD,NX1MAX)
      IF (ICBUFF(JCHORD,NX2MIN).LT.ICBUFF(ICHORD,NX2MIN))
     +   ICBUFF(ICHORD,NX2MIN)=ICBUFF(JCHORD,NX2MIN)
      IF (ICBUFF(JCHORD,NX2MAX).GT.ICBUFF(ICHORD,NX2MAX))
     +   ICBUFF(ICHORD,NX2MAX)=ICBUFF(JCHORD,NX2MAX)
C
C Add togther hole counts
C
      RCBUFF(ICHORD,NHOLES)=RCBUFF(ICHORD,NHOLES)+RCBUFF(JCHORD,NHOLES)
C
C Add together chord areas
C
      RCBUFF(ICHORD,NSUM)=RCBUFF(ICHORD,NSUM)+RCBUFF(JCHORD,NSUM)
C
C Add together chord perimeter lengths
C
      RCBUFF(ICHORD,NPERIM)=RCBUFF(ICHORD,NPERIM)+RCBUFF(JCHORD,NPERIM)
C
C Add together horizontal projections and vertical projections
C
      RCBUFF(ICHORD,NHPROJ)=RCBUFF(ICHORD,NHPROJ)+RCBUFF(JCHORD,NHPROJ)
      RCBUFF(ICHORD,NVPROJ)=RCBUFF(ICHORD,NVPROJ)+RCBUFF(JCHORD,NVPROJ)
C
C Add together first, second and cross moments of area for chords
C
      RCBUFF(ICHORD,NSUMC)=RCBUFF(ICHORD,NSUMC)+RCBUFF(JCHORD,NSUMC)
      RCBUFF(ICHORD,NSUMR)=RCBUFF(ICHORD,NSUMR)+RCBUFF(JCHORD,NSUMR)
      RCBUFF(ICHORD,NSUMCC)=RCBUFF(ICHORD,NSUMCC)+RCBUFF(JCHORD,NSUMCC)
      RCBUFF(ICHORD,NSUMRR)=RCBUFF(ICHORD,NSUMRR)+RCBUFF(JCHORD,NSUMRR)
      RCBUFF(ICHORD,NSUMCR)=RCBUFF(ICHORD,NSUMCR)+RCBUFF(JCHORD,NSUMCR)
C
      RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module ANALY6
C
      LOGICAL FUNCTION ANALY6(NPAR,ICHORD,IROW,AREA,LPT1,LPT2)
      INTEGER NPAR,ICHORD,IROW,LPT1,LPT2
      REAL AREA(2)
C
C Stores away the parameters describing a new particle.  The data is
C obtained from the values stored in chord ICHORD.  These will be the
C final sums for the particle in question.  The particle is rejected
C if its area is outside the range AREA(1) to AREA(2) or if its centre
C of area (centroid) lies outside a specified sub-region of the source
C picture and its id is set to zero.  Otherwise, the particle id is the
C last particle id NPAR plus 1.  The particle's reference point is the
C right-most pixel of its bottom-most row.  The particle's centre of
C area is not suitable as the reference point because it is not
C guaranteed to lie on the particle.  The reference point also lies on
C the particle's perimeter which might make it useful in the future for
C extracting the the particle's perimeter.  The full set of particle
C parameters are determined and stored on disc at this point, in order
C to simplify the transposition of the final results.  The parent id
C (0 = background) and the particle/hole flag are fixed and not used,
C but have been included with a view to providing for the processing of
C holes as nested particles.
C
C Each row of the temporary picture opened to accumulate the results
C will hold the results for one particle.  If it is full, a temporary
C picture twice the size (up to a limit depending on the row-buffer
C length) is opened and the results are transferred into it.
C
      LOGICAL SEMOPN,SEMROW,SEMCLS
C
      INCLUDE 'COMMON'
C
C Indices for integer chord parameters
C
      INTEGER NIDENT,NFORWP,NBACKP,NCOL1,NCOL2,NCMIN,NCMAX,NRMIN
      INTEGER NX1MIN,NX1MAX,NX2MIN,NX2MAX,NVERT1,NVERT2
      PARAMETER (NIDENT=1)
      PARAMETER (NFORWP=NIDENT+1)
      PARAMETER (NBACKP=NFORWP+1)
      PARAMETER (NCOL1=NBACKP+1)
      PARAMETER (NCOL2=NCOL1+1)
      PARAMETER (NCMIN=NCOL2+1)
      PARAMETER (NCMAX=NCMIN+1)
      PARAMETER (NRMIN=NCMAX+1)
      PARAMETER (NX1MIN=NRMIN+1)
      PARAMETER (NX1MAX=NX1MIN+1)
      PARAMETER (NX2MIN=NX1MAX+1)
      PARAMETER (NX2MAX=NX2MIN+1)
      PARAMETER (NVERT1=NX2MAX+1)
      PARAMETER (NVERT2=NVERT1+1)
C
C Indices for floating point chord parameters
C
      INTEGER NHOLES,NHPROJ,NVPROJ,NPERIM,NSUM,NSUMC,NSUMR
      INTEGER NSUMCC,NSUMRR,NSUMCR
      PARAMETER (NHOLES=1)
      PARAMETER (NHPROJ=NHOLES+1)
      PARAMETER (NVPROJ=NHPROJ+1)
      PARAMETER (NPERIM=NVPROJ+1)
      PARAMETER (NSUM=NPERIM+1)
      PARAMETER (NSUMC=NSUM+1)
      PARAMETER (NSUMR=NSUMC+1)
      PARAMETER (NSUMCC=NSUMR+1)
      PARAMETER (NSUMRR=NSUMCC+1)
      PARAMETER (NSUMCR=NSUMRR+1)
C
C Number of integer and real parameters for each chord
C
      INTEGER NIPAR,NRPAR
      PARAMETER (NIPAR=NVERT2)
      PARAMETER (NRPAR=NSUMCR)
C
C Maximum number of chords in chord buffer
C
      INTEGER NCHORD
      PARAMETER (NCHORD=4*(LNBUF/(NIPAR*LNINT+NRPAR*LNREAL)))
C
C Maximum number of particles that can be generated
C
      INTEGER MAXPAR
      PARAMETER (MAXPAR=LNBUF/LNREAL)
C
C Number of parameters for each particle
C
      INTEGER NPPAR
      PARAMETER (NPPAR=25)
C
      INTEGER ICBUFF(NCHORD,NIPAR),FIRST(3),LAST(3)
      REAL    RCBUFF(NCHORD,NRPAR),RB3456(4*(LNBUF/LNREAL))
      LOGICAL SUBREG
C
      EQUIVALENCE (RCBUFF,RB3456,RB3)
      EQUIVALENCE (ICBUFF,RB3456(NCHORD*NRPAR+1))
      EQUIVALENCE (FIRST,SMGI1),(LAST,SMGI4),(SUBREG,SMGL1)
C
C Local variables
C
      REAL GCOL,GROW,P,PX,PY,SUM,SUMXX,SUMXY,SUMYY,SUMRR
      INTEGER CCOL,CROW,NCOL,NROW,LPT3,J
C
      ANALY6=.TRUE.
C
C Zero chord id
C
      ICBUFF(ICHORD,NIDENT)=0
C
C Fetch particle area
C
      SUM=RCBUFF(ICHORD,NSUM)
C
C Return if particle area too small or too large
C
      IF (SUM.LT.AREA(1).OR.SUM.GT.AREA(2)) GOTO 20
C
C Determine particle centre of area in pixel coordinates
C
      GCOL=(RCBUFF(ICHORD,NSUMC)/2.0)/SUM
      GROW=RCBUFF(ICHORD,NSUMR)/SUM
C
C Return if particle centre of area outside guard sub-region (if any)
C
      IF (SUBREG) THEN
         IF (GCOL.LT.REAL(FIRST(1)).OR.GCOL.GT.REAL(LAST(1)).OR.
     +       GROW.LT.REAL(FIRST(2)).OR.GROW.GT.REAL(LAST(2))) GOTO 20
      ENDIF
C
C Next particle id
C
      NPAR=NPAR+1
C
C Fault too large particle id
C
      IF (NPAR.GT.MAXPAR) THEN
         ERROR=151
         GOTO 30
      ENDIF
C
C Check for space in the temporary picture
C
      IF (NPAR.GT.NROWS(LPT2)) THEN
C
C Save contents of RB1 and RB2 (these are overwritten by SEMOPN)
C
         IF (SEMROW(2,RB1,NFMBYT,1,1,LPT1)) GOTO 30
         IF (SEMROW(2,RB2,NFMBYT,2,1,LPT1)) GOTO 30
C
C Establish new temporary picture size
C
         NROW=MIN(2*NROWS(LPT2),MAXPAR)
C
C Open new temporary picture
C
         LPT3=0
         IF (SEMOPN(3,0,NPPAR,NROW,1,NCLIMA,NFMFP,LPT3)) GOTO 30
C
C Copy results from old to new temporary picture
C
         DO 10 J=1,NROWS(LPT2)
            IF (SEMROW(1,RB1,NFMFP,J,1,LPT2)) GOTO 30
            IF (SEMROW(2,RB1,NFMFP,J,1,LPT3)) GOTO 30
   10    CONTINUE
C
C Close old temporary picture (to stop overflow of LP table)
C
         IF (SEMCLS(LPT2)) GOTO 30
         LPT2=LPT3
C
C Restore contents of RB1 and RB2
C
         IF (SEMROW(1,RB1,NFMBYT,1,1,LPT1)) GOTO 30
         IF (SEMROW(1,RB2,NFMBYT,2,1,LPT1)) GOTO 30
      ENDIF
C
C Store particle id as end chord id
C
      ICBUFF(ICHORD,NIDENT)=NPAR
C
C Fetch source picture size and centre position
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
C
      CCOL=CCOLN(LP1)
      CROW=CROWN(LP1)
C
C Determine particle reference point (right most pixel of last row)
C
      RB1(1)=REAL(ICBUFF(ICHORD,NCOL2)-CCOL)
      RB1(2)=REAL(CROW-IROW)
C
C Particle id
C
      RB1(3)=REAL(NPAR)
C
C Parent id is always zero (background)
C
      RB1(4)=0.0
C
C Fetch number of holes contained within particle
C
      RB1(5)=RCBUFF(ICHORD,NHOLES)
C
C Background flag is always unset (holes not counted as particles)
C
      RB1(6)=0.0
C
C Set frame contact flag if particle touches edge of source picture
C
      IF (ICBUFF(ICHORD,NCMIN).EQ.1.OR.ICBUFF(ICHORD,NCMAX).EQ.NCOL.OR.
     +    ICBUFF(ICHORD,NRMIN).EQ.1.OR.IROW.EQ.NROW) THEN
         RB1(7)=1.0
      ELSE
         RB1(7)=0.0
      ENDIF
C
C Determine particle limits
C
      RB1(8)=REAL(ICBUFF(ICHORD,NCMIN)-CCOL)
      RB1(9)=REAL(ICBUFF(ICHORD,NCMAX)-CCOL)
      RB1(10)=RB1(2)
      RB1(11)=REAL(CROW-ICBUFF(ICHORD,NRMIN))
C
C Determine ferets diameters
C
      RB1(12)=REAL(ICBUFF(ICHORD,NCMAX)-ICBUFF(ICHORD,NCMIN)+1)
      RB1(13)=REAL(IROW-ICBUFF(ICHORD,NRMIN)+1)
      RB1(14)=REAL(ICBUFF(ICHORD,NX1MAX)-ICBUFF(ICHORD,NX1MIN))/
     +          SQRT(2.0)+1.0
      RB1(15)=REAL(ICBUFF(ICHORD,NX2MAX)-ICBUFF(ICHORD,NX2MIN))/
     +          SQRT(2.0)+1.0
C
C Fetch particle vertical and horizontal projections
C
      RB1(16)=RCBUFF(ICHORD,NVPROJ)
      RB1(17)=RCBUFF(ICHORD,NHPROJ)
C
C Fetch particle perimeter and area
C
      RB1(18)=RCBUFF(ICHORD,NPERIM)
      RB1(19)=SUM
C
C Convert centre of area to picture coordinates
C
      RB1(20)=GCOL-REAL(CCOL)
      RB1(21)=REAL(CROW)-GROW
C
C Determine second moments of area about centre of area
C
      SUMXX=(RCBUFF(ICHORD,NSUMCC)/6.0)/SUM-GCOL*GCOL
      SUMYY=RCBUFF(ICHORD,NSUMRR)/SUM-GROW*GROW
      SUMXY=GCOL*GROW-(RCBUFF(ICHORD,NSUMCR)/2.0)/SUM
C
C Determine principal moments
C
      SUMRR=SUMXX+SUMYY
      PX=(SUMXX-SUMYY)/2.0
      PY=SUMXY
      P=SQRT(PX*PX+PY*PY)
C
      RB1(22)=SUMRR/2.0-P
      RB1(23)=SUMRR/2.0+P
C
C Determine particle orientation (axis with least moment)
C
      IF (P.EQ.0.0) THEN
         RB1(24)=0.0
      ELSE
         RB1(24)=ATAN2(PY,PX)/2.0
      ENDIF
C
C Determine particle circularity = (4*pi*area)/(perimeter*perimeter)
C
      RB1(25)=4.0*PI*SUM/(RB1(18)*RB1(18))
C
C Store results in temporary disc picture
C
      IF (SEMROW(2,RB1,NFMFP,NPAR,1,LPT2)) GOTO 30
C
   20 ANALY6=.FALSE.
C
   30 RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module ANALY7
C
      LOGICAL FUNCTION ANALY7(NROW,LASPAR)
      INTEGER NROW,LASPAR
C
C Relabels the segmented output picture LP2 to take into account the
C results so far of the particle scan.  Relabeling is carried out
C from the top of the output picture down to row NROW.  LASPAR is
C the largest particle id currently recorded in the output picture.
C Each particle chord is labeled with an id larger than LASPAR.  This
C id is propagated down through subsequent overlapping chords.  When
C two series of chords merge into one, the id for the second series of
C chords must be made the same as for the first series.  The array
C IB2, which is equivalenced to row buffer array RB2, records any
C change of id.  Changes of id caused by the completion of scanning of
C a particle are similarly recorded.  In this case the new id is the
C particle id, or zero, if the particle has been rejected.
C A relabeling pass is required when the source picture has been
C processed, and there may also be some intermediate relabeling passes
C if all valid chord id's have been used.
C
      LOGICAL SEMROW
C
      INCLUDE 'COMMON'
C
      INTEGER I,J,NCOL
      LOGICAL DIFFER
C
      INTEGER IB1(LNBUF/LNINT),IB2(LNBUF/LNINT)
C
      EQUIVALENCE (IB1,RB1),(IB2,RB2)
C
      ANALY7=.TRUE.
C
C Fetch output picture row size
C
      NCOL=NCOLS(LP2)
C
C Relabel output picture
C
      DO 20 J=1,NROW
C
C Read row from output picture
C
         IF (SEMROW(1,RB1,NFMINT,J,1,LP2)) GOTO 30
C
C Initialise change flag
C
         DIFFER=.FALSE.
C
C Relabel output picture row
C
         DO 10 I=1,NCOL
C
C Only values greater than LASPAR need be changed
C
            IF (IB1(I).GT.LASPAR) THEN
C
C Relabel pixel
C
               IB1(I)=IB2(IB1(I)-LASPAR)
C
C Set change flag
C
               DIFFER=.TRUE.
            ENDIF
   10    CONTINUE
C
C If output picture row changed, store it
C
         IF (DIFFER) THEN
            IF (SEMROW(2,RB1,NFMINT,J,1,LP2)) GOTO 30
         ENDIF
   20 CONTINUE
C
      ANALY7=.FALSE.
C
   30 RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module ANALY8
C
      REAL FUNCTION ANALY8(ID)
      INTEGER ID
C
      INCLUDE 'COMMON'
C
      INTEGER NSQRT
      PARAMETER (NSQRT=LNEDGE+LNEDGE)
C
      REAL SQRT1(NSQRT)
C
      EQUIVALENCE (SQRT1,RB1RHS)
C
      IF (ID.GT.NSQRT) THEN
         ANALY8=REAL(ID)+1.0/REAL(ID+ID)
      ELSE
         ANALY8=SQRT1(ID)
      ENDIF
C
      RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
