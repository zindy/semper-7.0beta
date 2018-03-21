C Semper VI processing module LLMANU
C
      SUBROUTINE LLMANU
C
C Provides verbs LLCREATE, LLADD, LLDELETE, LLEXAMINE and LLZERO, which
C manage lattice-line data in the form of a class Undefined picture with
C the following contents:
C
C LLCREATE creates a l-l picture with space for SIZE samples per line
C    row pairs allocated for all required lines up to resolution XMAX
C LLADD adds picture SECTION to the l-l set
C LLDELETE deletes any samples with serial in given range, or ALL
C LLEXAMINE types information on data (options FULL, MP)
C LLZERO deletes/zeros data on a given line
C
C VDs required for the complete LL package:
C $ll :LLGRFT mp line= li2= peak=
C LLgraph >Select $2=dis to=$2 exclude serial= se2= >$ll
C LLexamine :LLMANU all full mp log line= li2= >Select
C
C LLadd :LLMANU section= serial= >Select
C LLcreate :LLMANU $1=sel from=$1 size= xmax= zmax= p1 p2 p3 p4 p6
C LLdelete :LLMANU all serial= se2= >Select
C LLzero :LLMANU line= li2= >Select
C LLfit >$r3 size= zmax= >$ll
C LLresample :LLRES >$r3 size= zmax= radius= width=
C $sl :LLSL xmax= size= si2= $2=999 to=$2 >Select
C LLSection width= >$sl
C LLLayer layer= la2= >$sl
C LLXform :LLFT1D >$r3
C
C Format of lattice line set pictures (class Undefined):
C Row 1: Header record - successive REALs as below:
C        1: Reserved (formerly # plates incorporated)
C        2: Fixed value 9. (for recognition purposes)
C        3: U1 ) Components of 2-D reciprocal lattice base vector U
C        4: U2 )
C        5: V1 ) The same for base vector V
C        6: V2 )
C        7: XMAX - Max horiz sp. freq. of l-ls stored
C        8: ZMAX - Max vert sp. freq. accomodated along l-ls
C        9: N1 ) first 3 letters of symmetry abbreviation, e.g. 'P6 '
C       10: N2 ) remaining letters.. [(9),(10) in fact unused]
C       11: # samples along interpolated l-ls
C       12: Symmetry type index (1,2,3,4,5 for p1,p2,p3,p4,p6)
C       13: ZRMAX - Max real space Z value (dist from central plane)
C Spatial frequencies are given in cycles/pixel across the image;
C 'horizontal' components mean in the crystal plane, vertical means
C normal to it
C
C Rows 2ff: taken in pairs, contain the l-l data themselves, with
C all independent symmetry related data combined; row formats -
C        1: [maxptr,status],[re,im],[re,im]...
C        2: [h,k],[Z*/Z,serial],[Z*/Z,serial]...
C Status values are:
C        1: raw sample collection (used initially)
C        2: fitted/interpolated (used after LLFIT)
C        3: transformed (used after LLFIT and LLXFORM)
C
      LOGICAL SEMOPN,SEMROW,SEMTFC,OPT,VARSET,ABANDN,LLM3,SEMCON,SEMDIA
      LOGICAL SERSET,DELETE,SINGLE,HPL,FULL,MP,ALL,CONJ,CSYMM
      REAL ROT(3,3),K,KS,KX,KY
      PARAMETER (NTYPES=5)
      INTEGER NPN(NTYPES)
      INTEGER PTR,STATUS,SYMTYP,SPN,CLASS,FORM
      INTEGER*4 I4,I4MP
      CHARACTER CHSTAT(3)*23,CHZ(3)*2,CHSYM(5)*2,VALSTR*9
C
C Packed names
      PARAMETER (NLLCRE=19683,NZMAX=-10122,NXMAX=-6922,NSIZE=30786)
      PARAMETER (NLLADD=19681,NSECTI=30603,NSERIA=30618,NLLZER=19706)
      PARAMETER (NLLDEL=19684,NMP=21440,NALL=2092,NSE2=30632)
      PARAMETER (NLINE=19574,NLI2=19592,NFULL=10452)
      PARAMETER (NPHI=25929,NALPHA=2096,NPSI=26369,NFROM=10335)
      PARAMETER (NU=-1601,NU2=-2881,NV=-3201,NV2=-4481)
C
      INCLUDE 'COMMON'
      PARAMETER (NRPB=LNBUF/LNREAL)
C
C NPN contains P1,P2,P3,P4 and P6
      DATA NPN/26840,26880,26920,26960,27040/
C
      DATA CHZ/'Z*','Z*','Z'/
      DATA CHSYM/'p1','p2','p3','p4','p6'/
C
C l-l status values
      DATA CHSTAT/'Raw sample collection','Fitted and interpolated',
     +   '1-D transformed'/
C
      IF (VERB.NE.NLLCRE) GOTO 150
C
C Code for CREATE mode
C --------------------
C Establish symmetry type
      SYMTYP=0
      DO 10 I=1,NTYPES
         IF (OPT(NPN(I))) SYMTYP=I
   10 CONTINUE
      IF (SYMTYP.EQ.0) THEN
         IF (SEMDIA('Symmetry type not found',NDIERR)) RETURN
         ERROR=10
         RETURN
      ENDIF
      IF (LLM3(SYMTYP,NSRLS,S11,S12,S21,S22,CSYMM)) RETURN
C
      UX=VAL(NU)
      UY=VAL(NU2)
C For symmetry types 3,4,5, force V from U
      IF (SYMTYP.EQ.3) THEN
         VX=-.5*UX-.8666666*UY
         VY=.8666666*UX-.5*UY
      ELSE IF (SYMTYP.EQ.4) THEN
         VX=-UY
         VY=UX
      ELSE IF (SYMTYP.EQ.5) THEN
         VX=.5*UX-.8666666*UY
         VY=.8666666*UX+.5*UY
      ELSE
C Otherwise, pick up V directly
         VX=VAL(NV)
         VY=VAL(NV2)
      ENDIF
      IF (UX*VY.EQ.UY*VX) THEN
         ERROR=79
         RETURN
      ENDIF
      XMAX=VAL(NXMAX)
      IF (XMAX.LE.0) THEN
         ERROR=3
         IDERR=NXMAX
         RETURN
      ENDIF
      ZMAX=VAL(NZMAX)
      IF (ZMAX.LE.0.) THEN
         ERROR=3
         IDERR=NZMAX
         RETURN
      ENDIF
      PTR=2
C
C Compile list of indices for asymmetric unit
C
C H is scanned from 0 to 16 simply, within a K scan from 0 to 16 and
C from -1 to -16 - the apparently perverse ordering ensures that the
C lines finally recorded have the 'nicest' indices possible; for each
C line, the h,k and those of any symm-rel(s.r.) lines are sought in
C a list, and H,K is added to the list if none are found.
C
C Begin level 1 loop, over K
      K=0.
C
C Begin level 2 loop, over H
   20 H=0.
C
C Begin level 3 loop, over lines in s.r. set
   30 DSTAR=SQRT((H*UX+K*VX)**2+(H*UY+K*VY)**2)
      IF (DSTAR.GT.XMAX) GOTO 80
      HS=H
      KS=K
      DO 60 NL=1,NSRLS
      CONJ=.FALSE.
C
C Level 4 loop: search existing list for line
   40 IF (PTR.LT.4) GOTO 70
      DO 50 I=4,PTR,2
         IF (HS.EQ.RB5(I-1)) THEN
C If line found, long-jump to next H,K
            IF (KS.EQ.RB5(I)) GOTO 80
         ENDIF
   50 CONTINUE
C
C Line not found: try (conjugate, then) next s.r. line
      HS=-HS
      KS=-KS
      IF (.NOT.CONJ) THEN
         CONJ=.TRUE.
         GOTO 40
      ENDIF
      T=HS*S11+KS*S12
      KS=HS*S21+KS*S22
      HS=T
   60 CONTINUE
C
C End of level 3 loop: line not found, so add to list
   70 IF (PTR.GE.NRPB) THEN
         IF (SEMDIA('Too many lines - reduce KMAX',NDIERR)) RETURN
         ERROR=10
         RETURN
      ENDIF
      PTR=PTR+2
      RB5(PTR-1)=H
      RB5(PTR)=K
C Store Dstar as well for later sorting
      RB4(PTR-1)=DSTAR
C
C Next H
   80 H=H+1.
      IF (H.LE.16.) GOTO 30
C
C End of level 2 loop: next K
      IF (K.LT.0.) GOTO 90
      K=K+1.
      IF (K.LE.16.) GOTO 20
      K=0.
   90 K=K-1.
      IF (K.GE.-16.) GOTO 20
C
C End of level 1 loop
C
C RB5 now contains indices for asymm-unit, and RB4 the corresponding
C Dstar values.  Sort by the latter.
C
      CALL LLM4(RB5,RB4,PTR/2-1,NRPB,.TRUE.)
C
C Open the l-l picture
      NCOL=IVAL(NSIZE)+1
      NROW=PTR-1
      LLPN=IVALPN(NFROM)
      IF (NCOL.LT.17) THEN
         ERROR=3
         IDERR=NSIZE
         RETURN
      ENDIF
      LP1=0
      IF (SEMOPN(2,LLPN,NCOL,NROW,1,NCLUND,NFMCOM,LP1)) RETURN
C
C Prepare first row
      NCOL2=NCOL*2
      DO 100 I=1,NCOL2
         RB1(I)=0.
  100 CONTINUE
      RB1(2)=9.
      RB1(3)=UX
      RB1(4)=UY
      RB1(5)=VX
      RB1(6)=VY
      RB1(7)=XMAX
      RB1(8)=ZMAX
      RB1(9)=NPN(SYMTYP)
      RB1(10)=0.
      RB1(12)=SYMTYP
      IF (SEMROW(2,RB1,NFMCOM,1,1,LP1)) RETURN
C
C Now write out empty (but initialised) row pairs for line data
      DO 110 I=1,9
  110 RB1(I)=0.
      PTR=2
      DO 120 J=2,NROW,2
         PTR=PTR+2
         RB1(1)=2.
C Status 1 initially
         RB1(2)=1.
         IF (SEMROW(2,RB1,NFMCOM,J,1,LP1)) RETURN
         RB1(1)=RB5(PTR-1)
         RB1(2)=RB5(PTR)
         IF (SEMROW(2,RB1,NFMCOM,J+1,1,LP1)) RETURN
  120 CONTINUE
C
C Verify the result
      IF (SEMCON('Lattice lines included:')) RETURN
      N1=3
  130 N2=MIN(N1+9,PTR)
      IF (N2.GE.N1) THEN
         WRITE (RECORD,140) (INT(RB5(I)),I=N1,N2)
  140    FORMAT (5('(',I3,',',I3,')    '))
         IF (SEMCON(RECORD)) RETURN
         N1=N1+10
         GOTO 130
      ENDIF
C
C Merge with LLEXAMINE code so as to verify result comprehensively
C
C Process existing l-l set
C ------------------------
C Check l-l set
  150 LLPN=IVALPN(NFROM)
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      IF (SEMROW(1,RB1,NFMCOM,1,1,LP1)) RETURN
      IF (NROW.LT.3.OR.CLASSN(LP1).NE.NCLUND.OR.RB1(2).NE.9.) THEN
         WRITE (RECORD,160) LLPN
  160    FORMAT ('Picture',I5,' is not a lattice-line set')
         IF (SEMCON(RECORD)) RETURN
         ERROR=10
         RETURN
      ENDIF
      NCOL2=2*NCOL
      SINGLE=VARSET(NLINE)
      IF (SINGLE) THEN
         H=VAL(NLINE)
         K=VAL(NLI2)
      ENDIF
      IF (VERB.EQ.NLLZER) GOTO 290
C
      UX=RB1(3)
      UY=RB1(4)
      VX=RB1(5)
      VY=RB1(6)
      XMAX=RB1(7)
      ZMAX=RB1(8)
      SYMTYP=RB1(12)
      IF (LLM3(SYMTYP,NSRLS,S11,S12,S21,S22,CSYMM)) RETURN
C
      DELETE=VERB.EQ.NLLDEL
      ALL=OPT(NALL)
      SER1=VAL(NSERIA)
      SERSET=SER1.NE.0.
      SER2=SER1
      IF (DELETE) THEN
         IF (VARSET(NSE2)) SER2=VAL(NSE2)
         IF (.NOT.(SERSET.OR.ALL)) THEN
            ERROR=3
            IDERR=NSERIA
            RETURN
         ENDIF
      ENDIF
      PHI=VAL(NPHI)
      ALPHA=VAL(NALPHA)
      PSI=VAL(NPSI)
C
C Default case: merely examine l-l set
C ------------------------------------
      IF (VERB.EQ.NLLADD.OR.DELETE) GOTO 350
C
C Verify basic parameters
      WRITE (RECORD,170) CHSYM(SYMTYP),NCOL-1
  170 FORMAT ('Symmetry type ',A,'   Max number of line samples',I4)
      IF (SEMCON(RECORD)) RETURN
      WRITE (RECORD,180) UX,UY,VX,VY
  180 FORMAT ('Base vectors   U= ',F7.4,1H,,F7.4,'   V= ',F7.4,1H,,F7.4)
      IF (SEMCON(RECORD)) RETURN
      WRITE (RECORD,190) XMAX
  190 FORMAT ('Maximum in-plane frequency (XMAX)',F10.4,
     +   ' cycles/pixel')
      IF (SEMCON(RECORD)) RETURN
      WRITE (RECORD,200) ZMAX
  200 FORMAT ('Maximum out-of-plane frequency (ZMAX)',F10.4,
     +   ' cycles/pixel')
      IF (SEMCON(RECORD)) RETURN
      WRITE (RECORD,210) .5/ZMAX
  210 FORMAT ('Real space layer spacing ',F6.3,' pixels')
      IF (SEMCON(RECORD)) RETURN
C
C If not verifying any lines, report first line status at least
      IF (.NOT.(ALL.OR.SINGLE)) THEN
         IF (SEMROW(1,RB4,NFMCOM,2,1,LP1)) RETURN
         WRITE (RECORD,220) CHSTAT(INT(RB4(2)))
  220    FORMAT ('First line status: ',A)
         IF (SEMCON(RECORD)) RETURN
         GOTO 280
      ENDIF
C
      VALSTR='real,imag'
      MP=OPT(NMP)
      IF (MP) VALSTR='mod,phase'
      FULL=OPT(NFULL).OR.MP
C
C Read through all lines in turn
      NROW=NROWS(LP1)
      DO 270 J=2,NROW,2
      IF (SEMROW(1,RB4,NFMCOM,J+1,1,LP1)) RETURN
      IF (SINGLE) THEN
         IF (RB4(1).NE.H.OR.RB4(2).NE.K) GOTO 270
      ENDIF
      IF (SEMROW(1,RB1,NFMCOM,J,1,LP1)) RETURN
      PTR=RB1(1)
      IH=RB4(1)
      IK=RB4(2)
      DSTAR=SQRT((RB4(1)*UX+RB4(2)*VX)**2+(RB4(1)*UY+RB4(2)*VY)**2)
      STATUS=RB1(2)
      IF (SEMCON(' ')) RETURN
      WRITE (RECORD,230) IH,IK,DSTAR,CHSTAT(STATUS)
  230 FORMAT ('Line ',I3,',',I3,' In-plane frequency',F7.2,2X,A)
      IF (SEMCON(RECORD)) RETURN
      N=PTR/2-1
      IF (N.EQ.0) THEN
         IF (SEMCON('No samples recorded')) RETURN
         GOTO 270
      ELSE
         WRITE (RECORD,240) N
  240    FORMAT ('Number of samples recorded',I5) 
         IF (SEMCON(RECORD)) RETURN
      ENDIF
      IF (FULL) THEN
         DO 260 I=4,PTR,2
            IF (ABANDN(ERROR)) RETURN
            X=RB1(I-1)
            Y=RB1(I)
            IF (MP) THEN
               T=SQRT(X*X+Y*Y)
               P=0.
               IF (T.NE.0.) P=ATAN2(Y,X)
               X=T
               Y=P
            ENDIF
            WRITE (RECORD,250)
     +         CHZ(STATUS),RB4(I-1),VALSTR,X,Y,INT(RB4(I))
  250       FORMAT (A,1X,F7.3,'   Value (',A,'): ',G12.4,
     +         1X,G12.4,'  Serial',I7)
            IF (SEMCON(RECORD)) RETURN
  260    CONTINUE
      ENDIF
  270 CONTINUE
  280 RETURN
C
C Code for LLZERO: force line LINE,LI2 to empty/zero
C ---------------
  290 IF (.NOT.SINGLE) THEN
         ERROR=25
         IDERR=NLINE
         RETURN
      ENDIF
C Search for line
      DO 320 J=2,NROW,2
         IF (SEMROW(1,RB4,NFMCOM,J+1,1,LP1)) RETURN
         IF (RB4(1).NE.H) GOTO 320
         IF (RB4(2).NE.K) GOTO 320
C Found: fetch samples
         IF (SEMROW(1,RB1,NFMCOM,J,1,LP1)) RETURN
C Mark empty, or fill with zeros acc to status
         IF (RB1(2).GE.2.) GOTO 300
         RB1(1)=2.
         GOTO 340
  300    DO 310 I=3,NCOL2
  310    RB1(I)=0.
         GOTO 340
  320 CONTINUE
C Line search failed
      IH=H
      IK=K
      WRITE (RECORD,330) IH,IK
  330 FORMAT ('Line ',I3,1H,,I3,' not found')
      IF (SEMCON(RECORD)) RETURN
      ERROR=10
      RETURN
C
C Rewrite amended line and quit
  340 IF (SEMROW(2,RB1,NFMCOM,J,1,LP1)) RETURN
      RETURN
C
C Code for LLADD: Open new section and load into RB5/RB6
C --------------
  350 IF (DELETE) GOTO 380
      SPN=IVALPN(NSECTI)
      IF (SEMOPN(1,SPN,NCS1,NRS1,NLS1,CLASS,FORM,LP2)) RETURN
      IF (CLASS.NE.NCLFOU) THEN
         ERROR=6
         IDERR=SPN
         RETURN
      ENDIF
      IF (SEMTFC(LP2,HPL)) RETURN
      IF (.NOT.HPL) THEN
         ERROR=63
         IDERR=SPN
         RETURN
      ENDIF
C If too big for RB5/RB6, reduce dimensions pro rata
      NCBUF=NCS1
      NRBUF=NRS1
      X=NCBUF
      X=X*NRBUF/FLOAT(NRPB)
      IF (X.GT.1.) THEN
         X=SQRT(X)
         NCBUF=NCS1/X
         NRBUF=NRS1/X
      ENDIF
C
C Load section into RB5/RB6
      J1=CROWN(LP2)-NRBUF/2
      I4=0
      DO 370 J=1,NRBUF
         IF (SEMROW(1,RB1,NFMCOM,J1,1,LP2)) RETURN
         DO 360 I=1,2*NCBUF
            I4=I4+1
  360       RB5(I4)=RB1(I)
  370    J1=J1+1
      I4MP=I4
C
C Check than neither ALPHA nor PSI is near pi/2 (or odd multiple)
      IF (ABS(COS(ALPHA)).LT.1E-3) THEN
         ERROR=3
         IDERR=NALPHA
         RETURN
      ENDIF
      IF (ABS(COS(PSI)).LT.1E-3) THEN
         ERROR=3
         IDERR=NPSI
         RETURN
      ENDIF
C Prepare rotation matrix
      CALL LLM2(PHI,ALPHA,PSI,ROT)
C
C Modify lines in turn
C --------------------
C Begin level 1 loop - over lattice lines from asymm-unit
  380 DO 460 J=2,NROW,2
C
C Load/initialise line
      IF (SEMROW(1,RB1,NFMCOM,J,1,LP1)) RETURN
      IF (SEMROW(1,RB4,NFMCOM,J+1,1,LP1)) RETURN
      PTR=RB1(1)
C
C Delete all samples?
      IF (DELETE.AND.ALL) THEN
         PTR=2
         GOTO 450
      ENDIF
C
C If adding, and serial zero, no condense needed
      IF (.NOT.DELETE.AND.SER1.EQ.0.) GOTO 420
C
C Condense list omitting samples with non-zero serial in range
      I1=2
      I2=2
      GOTO 400
  390 IF (RB4(I1).GE.SER1.AND.RB4(I1).LE.SER2) GOTO 410
      IF (I1.NE.I2) THEN
         RB1(I2-1)=RB1(I1-1)
         RB1(I2)=RB1(I1)
         RB4(I2-1)=RB4(I1-1)
         RB4(I2)=RB4(I1)
      ENDIF
  400 I2=I2+2
  410 I1=I1+2
      IF (I1.LE.PTR) GOTO 390
      PTR=I2-2
      IF (DELETE) GOTO 450
C
C Begin level 2 loop - over symmetry-related lines
  420 H=RB4(1)
      K=RB4(2)
      HS=H
      KS=K
      DO 440 IS=1,NSRLS
C
C Ensure Z within range held for this line
      KX=HS*UX+KS*VX
      KY=HS*UY+KS*VY
C (ROT(3,3) is cos(PSI)cos(ALPHA) and cannot therefore vanish)
      Z=-(ROT(3,1)*KX+ROT(3,2)*KY)/ROT(3,3)
      IF (ABS(Z).LE.ZMAX) THEN
C Try direct point (h,k) first..
         CONJ=.FALSE.
C If (h,k) in right half plane [omitting lower half of central col,
C which duplicates data in top half]
  430    IF (HS.GT.0..OR.HS.EQ.0.AND.KS.GE.0) THEN
C ..and point lies within projection buffer..
            I4=NRBUF/2-INT(KS)
            I4=2*(I4*NCBUF+INT(HS)+1)
            IF (I4.GT.0.AND.I4.LE.I4MP) THEN
C ..and there is room in the line buffer..
               IF (PTR.GE.NCOL2) THEN
                  IF (SEMDIA('Too many samples per line',NDIERR)) RETURN
                  ERROR=10
                  RETURN
               ENDIF
C ..deposit sample X,Y..
               PTR=PTR+2
               RB1(PTR-1)=RB5(I4-1)
               RB1(PTR)=RB5(I4)
C ..and serial, and Z
               RB4(PTR)=SER1
               RB4(PTR-1)=Z
C If line c-symm, replace -Z data by conjugated data at +Z
               IF (CSYMM.AND.Z.LT.0.) THEN
                  RB1(PTR)=-RB1(PTR)
                  RB4(PTR-1)=-RB4(PTR-1)
               ENDIF               
            ENDIF
         ENDIF
C Now try conjugate point (-h,-k), unless (0,0)
         IF (.NOT.(HS.EQ.0.AND.KS.EQ.0.)) THEN
            HS=-HS
            KS=-KS
            IF (.NOT.CONJ) THEN
               Z=-Z
               CONJ=.TRUE.
               GOTO 430
            ENDIF
         ENDIF
      ENDIF
C
C Generate next symmetry-related line
      T=HS*S11+KS*S12
      KS=HS*S21+KS*S22
      HS=T
C Avoid repetition if symmetry-related loop stops short,
C e.g. on (0,0) line
      IF (HS.EQ.H.AND.KS.EQ.K) GOTO 450
  440 CONTINUE
C
C End of level 2 loop - re-sort line wrt Z and output
  450 RB1(1)=PTR
C
      CALL LLM4(RB1,RB4,PTR/2-1,NRPB,.TRUE.)
      IF (SEMROW(2,RB1,NFMCOM,J,1,LP1)) RETURN
      IF (SEMROW(2,RB4,NFMCOM,J+1,1,LP1)) RETURN
C
C End of level 1 loop - repeat for next line
  460 CONTINUE
      RETURN
C
      END
C Semper VI subsidiary module LLM2
C
      SUBROUTINE LLM2(PHI,ALPHA,PSI,R)
C
C Calculates the rotation matrix elements Rij
C
C PHI        Azimuth of tilt axis wrt microscope X-axis
C ALPHA      Inclination of specimen plane to tilt axis
C PSI0       Tilt angle for first tilted plate
C
C The specimen orientation is described by  two successive rotations
C about horizontal axes (the microscope defining horiz and vert)
C First, a rotation of ALPHA is applied about an axis with and azimuth
C PHI+PI/2 wrt X, describing the inclination of the specimen plane
C to the tilt axis; second, a rotation of PSI about an axis with azimuth
C PHI, i.e. the tilt axis itself.  The first
C of these is described by the matrix
C
C            2        2
C       /   c   c  + s        ,  s   c   (c  - 1)   ,   -c   s    \
C       |    ph  a    ph          ph  ph   a              ph  a   |
C       |                                                         |
C       |                         2        2                      |
C   E =|                         2        2                      |
C   E = |   s   c   (c  - 1)  ,  s   c  + c         ,  -s   s     |
C       |    ph  ph   a           ph  a    ph            ph  a    |
C       |                                                         |
C       |      c   s          ,     s   s           ,    c        |
C       \       ph  a                ph  a                a       /
C
C and the second by the matrix
C
C            2         2
C       /   s   c   + c        ,  s   c   (1 - c  )   ,   -s   s     \
C       |    ph  si    ph          ph  ph       si          ph  si   |
C       |                                                            |
C       |                          2         2                       |
C   T = |   s   c   (1 - c  )  ,  c   c   + s         ,    c   s     |
C       |    ph  ph       si       ph  si    ph            ph  si    |
C       |                                                            |
C       |      s   s           ,    -c   s            ,     c        |
C       \       ph  si                ph  si                 si      /
C
      REAL R(3,3)
C
C Prepare trig factors
      SP=SIN(PHI)
      SP2=SP*SP
      CP=COS(PHI)
      CP2=CP*CP
      SPC=SP*CP
      SA=SIN(ALPHA)
      CA=COS(ALPHA)
      CS=COS(PSI)
      SS=SIN(PSI)
C
C Construct Eij
      E11=CP2*CA+SP2
      E12=SPC*(CA-1.)
      E13=-CP*SA
      E21=E12
      E22=SP2*CA+CP2
      E23=-SP*SA
      E31=-E13
      E32=-E23
      E33=CA
C Construct Tij
      T11=SP2*CS+CP2
      T12=SPC*(1.-CS)
      T13=-SP*SS
      T21=T12
      T22=CP2*CS+SP2
      T23=CP*SS
      T31=-T13
      T32=-T23
      T33=CS
C
C Construct product
      R(1,1)=T11*E11+T12*E21+T13*E31
      R(1,2)=T11*E12+T12*E22+T13*E32
      R(1,3)=T11*E13+T12*E23+T13*E33
      R(2,1)=T21*E11+T22*E21+T23*E31
      R(2,2)=T21*E12+T22*E22+T23*E32
      R(2,3)=T21*E13+T22*E23+T23*E33
      R(3,1)=T31*E11+T32*E21+T33*E31
      R(3,2)=T31*E12+T32*E22+T33*E32
      R(3,3)=T31*E13+T32*E23+T33*E33
      RETURN
C
      END
C Semper VI subsidiary module LLM3
C
      LOGICAL FUNCTION LLM3(SYMTYP,NSRLS,S11,S12,S21,S22,CSYMM)
C
C Supplies various parameters describing lattice line symmetry for
C given symmetry type index SYMTYP
C
C Line (h,k) is identical with the NSRLS lines generated by successive
C application of matrix ( s11  s12 )
C                       ( s21  s22 )
C The set generated does NOT however include the simple conjugate-
C reflection relating line (-h,-k) to (h,k) for all symmetries, and
C the calling program is responsible for considering this; it must
C also test for premature termination of NSRLS loop (e.g. for line 0,0)
C by testing whether (h',k') is same as (h,k).  Individual lines are
C internally conjugate-symmetric if CSYM returned .TRUE.
C
      LOGICAL SEMDIA,LINSYM(5),CSYMM
      REAL SYMOPM(2,2,5)
      INTEGER NSYMRS(5),SYMTYP
C
      INCLUDE 'COMMON'
C
C Matrices generating next line of symmetry-related sets (stored by
C columns, not rows, so as to follow normal mathematical convention)
      DATA SYMOPM/1.,0.,0.,1.,
     +            1.,0.,0.,1.,
     +            0.,1.,-1.,-1.,
     +            0.,1.,-1.,0.,
     +            0.,1.,-1.,1./
C
C List of numbers of symmetry-related lines for each case
      DATA NSYMRS/1,1,3,2,3/
C
C List of individual line symmetry indicators
      DATA LINSYM/.FALSE.,.TRUE.,.FALSE.,.TRUE.,.TRUE./
C
      LLM3=.TRUE.
      IF (SYMTYP.LE.0.OR.SYMTYP.GT.5) THEN
         IF (SEMDIA('Bad lattice line symmetry index',NDIERR)) RETURN
         ERROR=10
         RETURN
      ENDIF
C
      NSRLS=NSYMRS(SYMTYP)
      S11=SYMOPM(1,1,SYMTYP)
      S12=SYMOPM(1,2,SYMTYP)
      S21=SYMOPM(2,1,SYMTYP)
      S22=SYMOPM(2,2,SYMTYP)
      CSYMM=LINSYM(SYMTYP)
      LLM3=.FALSE.
      RETURN
C
      END
C Semper VI subsidiary routine LLM4
C
      SUBROUTINE LLM4(X,VALUE,N,NM,ASCEND)
C
C Re-arranges N datum pairs in arrays X(3-NM) and VALUE(3-NM),
C by Shell-sorting the values in real part of VALUE.
C
      REAL X(NM),VALUE(NM)
      LOGICAL ASCEND
C
C Determine number of passes required
      NPASS=0
      M=1
   10 IF (M.LT.N) THEN
         NPASS=NPASS+1
         M=2*M+1
         GO TO 10
      ENDIF
C
C Make required number of passes through data
      DO 40 IPASS=1,NPASS
C
C Determine next increment
         M=M/2
C
C Sort data with next increment
         DO 30 I=1,N-M
C
            DO 20 J=2*I+1,3,-2*M
C
               K=J+2*M
C
               V1=VALUE(J)
               V2=VALUE(K)
C
               IF (ASCEND) THEN
                  IF (V1.LE.V2) GO TO 30
               ELSE
                  IF (V2.LE.V1) GO TO 30
               ENDIF
C
               VALUE(J)=V2
               VALUE(K)=V1
               XY=VALUE(J+1)
               VALUE(J+1)=VALUE(K+1)
               VALUE(K+1)=XY
C
               XY=X(J)
               X(J)=X(K)
               X(K)=XY
               XY=X(J+1)
               X(J+1)=X(K+1)
               X(K+1)=XY
C
   20       CONTINUE
   30    CONTINUE
   40 CONTINUE
      RETURN
C
      END
C Semper VI processing module LLGRFT
C
      SUBROUTINE LLGRFT
C
C Provides verbs LLGRAPH and LLFIT, for plotting and manual fitting
C of lattice-line sets in the form managed by LL. LP1 (source) is
C opened by interpreter prior to call.
C
C - LLFIT normally fits any status 1 (raw sample collection) lines
C   in LP1, outputting to (pre-existing) TO; if SIZE is quoted,
C   a new TO is created first, identical to LP1 except that
C   (a) the row length is the larger of SIZE and the source length;
C   (b) SIZE is recorded in the header as the # fitted samples; and
C   (c) header ZMAX is replaced by any new keyed value.
C   If LINE is quoted, the indicated line is fitted only, and its
C   its initial status is ignored.
C - LLGRAPH draws graphs of all lines in LP1; key SERIAL (pair)
C   restricts marking to samples with serials in given range (or
C   excludes these samples if opt EXCLUDE); MP requests mod/phase mode
C   graphs rather than re/im; key PEAK can be used to force the maximum
C   ordinate
C
C Additional facilities needed in due course:
C - LINE mode (as opposed to isolated points); could use GRAPH
C   except for the problem of phase wrap-round...
C - Overwriting U,V in header with 'nicely' oriented versions
C
      LOGICAL SEMOPN,SEMDEL,SEMROW,OPT,OPTNO,VARSET,SEMCP2,SEMDPN,SEMXA1
      LOGICAL SEMCON,SEMDIA,ABANDN,LLM3
      LOGICAL FSINIT,FSERAS,FSMARK,FSLINE,FSTEXT,FSBORD,FSFLUS,FSXWIR
      LOGICAL CONJ,CSYMM,SINGLE,MP,FORCEV,EXCLUD,SERIAL,ERASE
      LOGICAL MARKNG
      REAL K
      INTEGER TEXT(12),SIZE,STATUS
      INTEGER CLASS,FORM,SYMTYP,DEVICE,PARTN,CH
C Packed names
      PARAMETER (NSIZE=30786,NLINE=19574,NLI2=19592,NZMAX=-10122)
      PARAMETER (NLLFIT=19686,NMP=21440,NMARK=20858,NERASE=8721)
      PARAMETER (NEXCLU=8963,NPEAK=25801)
      PARAMETER (NSERIA=30618,NSE2=30632,NFROM=10335,NTO=-601)
C
      INCLUDE 'COMMON'
C
      DATA TEXT/76,105,110,101,8*32/
C
      EXCLUD=OPT(NEXCLU)
      ERASE=.NOT.OPTNO(NERASE)
      SERIAL=VARSET(NSERIA)
      IF (SERIAL) THEN
         SER1=VAL(NSERIA)
         SER2=MAX(SER1,VAL(NSE2))
      ENDIF
      FORCEV=VARSET(NPEAK)
      IF (FORCEV) THEN
         VM=VAL(NPEAK)
         IF (VM.LE.0.) THEN
            ERROR=3
            IDERR=NPEAK
         ENDIF
      ENDIF
      MP=OPT(NMP)
      SINGLE=VARSET(NLINE).OR.VARSET(NLI2)
      H=VAL(NLINE)
      K=VAL(NLI2)
C
C Check source suitable
      IDERR=IVALPN(NFROM)
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      IF (SEMROW(1,RB4,NFMCOM,1,1,LP1)) RETURN
      IF (NROW.LT.3.OR.CLASSN(LP1).NE.NCLUND.OR.RB4(2).NE.9.)
     +   GOTO 160
      ZSMAX=RB4(8)
      ZRMAX=RB4(13)
      SYMTYP=RB4(12)
      IF (LLM3(SYMTYP,I,X,Y,Z,T,CSYMM)) RETURN
C
C Initialisation for LLFIT: warn about line symmetry,
C and establish output
      IF (VERB.EQ.NLLFIT) THEN
         IF (CSYMM) THEN
            IF (SEMCON(
     +         'Lines are conj-symm: draw right hand side only')) RETURN
         ELSE
            IF (SEMCON(
     +         'Lines are not conj-symm: draw both sides')) RETURN
         ENDIF
C
C If SIZE keyed, create new output
         IF (VARSET(NSIZE)) THEN
C Is SIZE factorisable and at least 16?
            SIZE=IVAL(NSIZE)
            IDERR=IVALPN(NTO)
            IF (SEMCP2(SIZE,1)) RETURN
            IF (SIZE.LT.16) THEN
               ERROR=5
               RETURN
            ENDIF
C New ZMAX?
            IF (VARSET(NZMAX)) ZSMAX=VAL(NZMAX)
            IF (ZSMAX.LE.0.) THEN
               ERROR=3
               IDERR=NZMAX
               RETURN
            ENDIF
C Make o/p row length larger of SIZE and source length
            NC=MAX(SIZE,NCOL)
            LP2=LP1
            IF (SEMOPN(2,IDERR,NC+1,NROW,1,NCLUND,NFMCOM,LP2)) RETURN
C Insert # interpolated samples and new ZMAX into header
            RB4(8)=ZSMAX
            RB4(11)=SIZE
            RB4(13)=.25*RB4(11)/ZSMAX
            IF (SEMROW(2,RB4,NFMCOM,1,1,LP2)) RETURN
C If necessary, copy source to output
            IF (LP1.NE.LP2) THEN
               DO 10 J=2,NROW
                  IF (SEMROW(1,RB1,NFMCOM,J,1,LP1)) RETURN
                  IF (SEMROW(2,RB1,NFMCOM,J,1,LP2)) RETURN
   10          CONTINUE
            ENDIF
C Use output as source subsequently
            LP1=LP2
C
C Pre-existing output - open and check
         ELSE
            IDERR=IVALPN(NTO)
            IF (SEMOPN(1,IDERR,NC,NR,NL,CLASS,FORM,LP2)) RETURN
            IF (SEMROW(1,RB4,NFMCOM,1,1,LP1)) RETURN
            IF (NR.LT.3.OR.CLASSN(LP2).NE.NCLUND.OR.RB4(2).NE.9.)
     +         GOTO 160
            IF (NR.NE.NROW) THEN
               ERROR=5
               RETURN
            ENDIF
            SIZE=RB4(11)
            IF (SIZE.EQ.0) SIZE=NCOL-1
            ZSMAX=RB4(8)
         ENDIF
      ENDIF
C
C Establish display to be used
      IF (VERB.EQ.NLLFIT) THEN
         MARK=IVAL(NMARK)
         IF (MARK.EQ.0) MARK=DISPLA
      ELSE
         MARK=IVAL(NTO)
      ENDIF
      IF (SEMDPN(MARK,DEVICE,PARTN)) RETURN
      MARK=1000*DEVICE+PARTN
      IF (SEMDEL(1,MARK)) RETURN
C
C Begin level 1 loop - over all lines
C ------------------
      DO 150 J=2,NROW,2
      IF (SEMROW(1,RB1,NFMCOM,J,1,LP1)) RETURN
      IF (SEMROW(1,RB2,NFMCOM,J+1,1,LP1)) RETURN
      LAST=RB1(1)
      STATUS=RB1(2)
      IH=RB2(1)
      IK=RB2(2)
C
C Skip this line?
      IF (SINGLE) THEN
         IF (H.NE.RB2(1)) GOTO 150
         IF (K.NE.RB2(2)) GOTO 150
      ELSE
         IF (VERB.EQ.NLLFIT.AND.STATUS.NE.1) GOTO 150
      ENDIF
C
C No - any data for it?
      IF (LAST.LT.4) THEN
         WRITE (RECORD,20) IH,IK
   20    FORMAT ('No samples found for line ',I3,',',I3)
         IF (SEMCON(RECORD)) RETURN
         GOTO 150
      ENDIF
C
C Establish vertical range
      IF (.NOT.FORCEV) THEN
         DO 30 I=3,LAST,2
            T=RB1(I)**2+RB1(I+1)**2
            IF (I.EQ.3) THEN
               VM=T
            ELSE
               VM=MAX(VM,T)
            ENDIF
   30    CONTINUE
         VM=SQRT(VM)
      ENDIF
C Check range
      IF (VM.EQ.0.) THEN
         WRITE (RECORD,40) IH,IK
   40    FORMAT ('Line ',I3,1H,,I3,': all values zero')
         IF (SEMCON(RECORD)) RETURN
         GOTO 150
      ENDIF
C
C
C Begin level 2 loop - over the two graphs
C ------------------
      MARKNG=.TRUE.
   50 DO 130 NG=1,2
C
C Reinitialise partition graphics
      IF (FSINIT(2,MARK)) RETURN
      DISPLA=MARK
C
C On first cycle, report scale..
      IF (NG.EQ.1.AND.MARKNG) THEN
         WRITE (RECORD,60) IH,IK,VM
   60    FORMAT ('Line ',I3,1H,,I3,'   Peak ordinate',2G12.4)
         IF (SEMCON(RECORD)) RETURN
C
C ..erase..
         IF (ERASE) THEN
            IF (FSERAS(3,FSXMIN,FSXMAX,FSYMIN,FSYMAX)) RETURN
         ENDIF
C
C ..and add lettering showing line indices
         N=6
         IF (SEMXA1(4,TEXT,13,N,RB2(1),I)) CONTINUE
         TEXT(N)=KCOMMA
         N=N+1
         IF (SEMXA1(4,TEXT,13,N,RB2(2),I)) CONTINUE
         IF (FSTEXT(TEXT,N-1,0.,0.,0,0)) RETURN
      ENDIF
C
C Remap graphics coordinates for re/im/mod/phase
      IF (MP) THEN
         IF (NG.EQ.1) THEN
            BL=0.
            WH=VM
         ELSE
            WH=1.5*PI
            BL=-WH
         ENDIF
      ELSE
         WH=VM
         BL=-WH
      ENDIF
C Target border bottom,top..
      B=FSBTOP*.1
      T=FSBTOP*.9
C ..shifted down on second cycle
      IF (NG.NE.1) THEN
         B=B-FSBTOP
         T=T-FSBTOP
      ENDIF
      IF (STATUS.EQ.3) THEN
         ZM=ZRMAX
      ELSE
         ZM=ZSMAX
      ENDIF
      CALL LLMAP(FSBLEF*.9,FSBRIG*.9,B,T,-ZM,ZM,BL,WH)
C
      IF (MARKNG) THEN
C Mark graph border and axes
         IF (FSBORD(0)) RETURN
         IF (FSLINE(0.,FSBBOT,0.,FSBTOP)) RETURN
         IF (FSLINE(FSBLEF,0.,FSBRIG,0.)) RETURN
C If MP, add lines at PI and -PI to lower graph
         IF (MP) THEN
            IF (NG.EQ.2) THEN
               IF (FSLINE(-ZM,PI,ZM,PI)) RETURN
               IF (FSLINE(-ZM,-PI,ZM,-PI)) RETURN
            ENDIF
         ENDIF
C
C Begin level 3 loop - over points to be marked
C ------------------
         DO 80 I=3,LAST,2
C Sample excluded by serial?
         IF (SERIAL) THEN
            X=RB2(I+1)
            IF (EXCLUD.EQV.(X.GE.SER1.AND.X.LE.SER2)) GOTO 80
         ENDIF
C No: select re/im/mod/phase
         X=RB1(I)
         Y=RB1(I+1)
         IF (MP) THEN
            T=SQRT(X*X+Y*Y)
            P=0.
            IF (T.NE.0.) P=ATAN2(Y,X)
            X=T
            Y=P
         ENDIF
         IF (NG.EQ.1) Y=X
C Set X coord for graph
         X=RB2(I)
         CONJ=.FALSE.
C Mark it
   70    IF (FSMARK(X,Y,FSMMOD,FSMSIZ)) RETURN
C Duplicate phase modulo 2.pi
         IF (MP.AND.NG.NE.1) THEN
            IF (FSMARK(X,Y-TWOPI,FSMMOD,FSMSIZ)) RETURN
            IF (FSMARK(X,Y+TWOPI,FSMMOD,FSMSIZ)) RETURN
         ENDIF
C Mark conjugated point too?
         IF (STATUS.EQ.1.AND.CSYMM.AND..NOT.CONJ) THEN
            CONJ=.TRUE.
            X=-X
            IF (NG.EQ.2) Y=-Y
            GOTO 70
         ENDIF
   80    CONTINUE
C End of level 3 loop
C
      ELSE
C Accept hand-drawn replacement curve
         DZ=ZM/REAL(SIZE/2)
         MC=SIZE/2+1
C
         X=-ZM
         IF (CSYMM) X=0.
         Z=X
         X1=X
C
C Read first point from cursor
         X0=X
         CALL FSXS61(0)
         IF (FSXWIR(X0,0.,X,Y)) RETURN
C Force 1st point if symm/antisymm
         X=X1
         IF (NG.EQ.2) Y=0.
C Mark it
         IF (FSMARK(X,Y,5,0)) RETURN
C
C Read next point, with rubberbanded line cursor
   90    X1=X
         Y1=Y
  100    IF (ABANDN(ERROR)) RETURN
         CALL FSXS61(1)
         IF (FSXWIR(X1,Y1,X,Y)) RETURN
C Ignore it unless rightward movement
         IF (X.LE.X1) GOTO 100
         IF (FSLINE(X1,Y1,X,Y)) RETURN
         GRAD=(Y-Y1)/(X-X1)
C
C Generate samples from last point to this
  110    IF (Z.GT.ZM) GOTO 120
         IF (Z.GT.X) GOTO 90
         R=Y1+GRAD*(Z-X1)
         IZ=MC+NINT(Z/DZ)
         IF (IZ.LE.SIZE) THEN
            RB1(2*IZ+NG)=R
            RB2(2*IZ+1)=Z
            RB2(2*IZ+2)=0.
         ENDIF
         IF (CSYMM) THEN
            IZ=MC-NINT(Z/DZ)
            IF (IZ.GE.1) THEN
               IF (NG.EQ.2) R=-R
               RB1(2*IZ+NG)=R
               RB2(2*IZ+1)=-Z
               RB2(2*IZ+2)=0.
            ENDIF
         ENDIF
         Z=Z+DZ
         GOTO 110
C
      ENDIF
C
C Graphical output/input for one graph complete: flush display
  120 IF (FSFLUS(0)) RETURN
C
C End of level 2 loop
  130 CONTINUE
C
C If MP, recover re/im parts
      IF (MP) THEN
         DO 140 I=3,SIZE*2,2
         T=MAX(RB1(I),0.)
         P=RB1(I+1)
         RB1(I)=T*COS(P)
  140    RB1(I+1)=T*SIN(P)
      ENDIF
C
C Output fitted lines
      IF (VERB.EQ.NLLFIT) THEN
C Update number of samples and line status
         RB1(1)=2*SIZE+2
         RB1(2)=2.
C Output interpolated data
         IF (SEMROW(2,RB1,NFMCOM,J,1,LP2)) RETURN
         IF (SEMROW(2,RB2,NFMCOM,J+1,1,LP2)) RETURN
      ENDIF
C
C Force terminal pause on next output, to give user
C opportunity to study display
      IF (J.LT.NROW-2.AND..NOT.SINGLE) THEN
         IF (VERB.NE.NLLFIT.OR..NOT.MARKNG) THEN
            TERCNT=TERLEN-1
         ENDIF
      ENDIF
C
C Follow marking with fitting pass?
      IF (VERB.EQ.NLLFIT) THEN
         IF (MARKNG) THEN
            MARKNG=.FALSE.
            GOTO 50
         ENDIF
      ENDIF
C
C End of level 1 loop
      IF (SINGLE) RETURN
  150 CONTINUE
      RETURN
C
C Error
  160 WRITE (RECORD,170) IDERR
  170 FORMAT ('Picture',I5,' is not a lattice-line set')
      IF (SEMDIA(RECORD,NDIERR)) RETURN
      ERROR=10
      RETURN
C
      END
C Semper VI subsidiary routine LLMAP
C
      SUBROUTINE LLMAP(OX1,OX2,OY1,OY2,NX1,NX2,NY1,NY2)
C
C Adjusts graphics mapping parameters so that what was OX1-OX2,OY1-OY2
C becomes NX1-NX2,NY1-NY2; the following only are affected:
C   FSxsca,xoff,ysca,yoff
C   --xmin,xmax,ymin,ymax
C   --blef,brig,bbot,btop
C
      REAL NX1,NX2,NY1,NY2
      INCLUDE 'COMMON'
C
      S1=FSXSCA
      O1=FSXOFF
      FSXSCA=S1*(OX2-OX1)/(NX2-NX1)
      FSXOFF=O1+OX1*S1-NX1*FSXSCA
C To retain original display space border, clipping limits, include:
C     S=S1/FSXSCA
C     O=(O1-FSXOFF)/FSXSCA
C     FSXMIN=FSXMIN*S+O
C     FSXMAX=FSXMAX*S+O
C etc.; for the present case, set them to the new mapped frame
      FSBLEF=NX1
      FSBRIG=NX2
      FSXMIN=NX1
      FSXMAX=NX2
C
      S1=FSYSCA
      O1=FSYOFF
      FSYSCA=S1*(OY2-OY1)/(NY2-NY1)
      FSYOFF=O1+OY1*S1-NY1*FSYSCA
C Sim..
C     S=S1/FSYSCA
C     O=(O1-FSYOFF)/FSYSCA
C     FSYMIN=FSYMIN*S+O
C     FSYMAX=FSYMAX*S+O
C etc.; sim.
      FSBBOT=NY1
      FSBTOP=NY2
      FSYMIN=NY1
      FSYMAX=NY2
      RETURN
C
      END
C Semper VI processing module LLRES
C
      SUBROUTINE LLRES
C
C Provides verb LLRESAMPLE, for re-sampling and/or masking of lattice-
C line set LP1.  SIZE sets the number of samples in the revised lines,
C and ZMAX the extreme Z value - both defaulting to the source values.
C
C Evenly spaced samples are generated as follows:
C   (i) for those lines which are 'raw sample collections', by
C       averaging samples over a range WIDTH around the required
C       site, by interpolating between the nearest two if no samples
C       fall within this range but samples are available to either
C       side, and as zero if Z is out of range completely
C  (ii) for those which are 'fitted and interpolated', by linear
C       interpolation between existing sample values, or as zero if
C       Z is out of range completely
C In addition, a 3-D Gaussian-edged l/p filter (RADIUS, WIDTH) is
C imposed, tapering data smoothly to zero if RADIUS is set.
C
      LOGICAL LLRES2,SEMOPN,SEMROW,VARSET,SEMCP2,LLM3,SEMCON,SEMDIA
      LOGICAL CSYMM
      INTEGER SIZE,STATUS,OSIZE,SYMTYP
C
C Packed names
      PARAMETER (NFROM=10335,NTO=-601,NSIZE=30786,NZMAX=-10122)
      PARAMETER (NRADIU=28844,NWIDTH=-5165)
C
      INCLUDE 'COMMON'
C
C Check source suitable
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      IF (SEMROW(1,RB4,NFMCOM,1,1,LP1)) RETURN
      IF (NROW.LT.3.OR.CLASSN(LP1).NE.NCLUND.OR.RB4(2).NE.9.) THEN
         WRITE (RECORD,10) IVALPN(NFROM)
   10    FORMAT ('Picture',I5,' is not a lattice-line set')
         IF (SEMDIA(RECORD,NDIERR)) RETURN
         ERROR=10
         RETURN
      ENDIF
      UX=RB4(3)
      UY=RB4(4)
      VX=RB4(5)
      VY=RB4(6)
      ZMAX=RB4(8)
      OZMAX=ZMAX
      OSIZE=RB4(11)
      IF (OSIZE.EQ.0) OSIZE=NCOL-1
      SYMTYP=RB4(12)
      IF (LLM3(SYMTYP,I,X,Y,Z,F,CSYMM)) RETURN
C
C Default interpolated SIZE to source size, and ensure factorisable
      IF (VARSET(NSIZE)) THEN
         SIZE=IVAL(NSIZE)
      ELSE
         SIZE=OSIZE
      ENDIF
      IF (SIZE.LT.16.OR.SEMCP2(SIZE,1)) THEN
         ERROR=3
         IDERR=NSIZE
         RETURN
      ENDIF
C New ZMAX provided?
      IF (VARSET(NZMAX)) THEN
         ZMAX=VAL(NZMAX)
         IF (ZMAX.LE.0.) THEN
            ERROR=3
            IDERR=NZMAX
            RETURN
         ENDIF
      ENDIF
C
C Note width and radius
      W=VAL(NWIDTH)
      IF (W.LE.0.) W=ZMAX/4.
      R=VAL(NRADIU)
C
C Make row length long enough to accommodate original sample
C collections, in case resampling process is interrupted leaving
C mixed status lines
      NC=MAX(SIZE,NCOL-1)
      LP2=LP1
      IF (SEMOPN(2,IVALPN(NTO),NC+1,NROW,1,NCLUND,NFMCOM,LP2)) RETURN
C Insert # interpolated samples and new ZMAX into header
      RB4(8)=ZMAX
      RB4(11)=SIZE
      RB4(13)=.25*RB4(11)/ZMAX
C Zero unused part in case length extended
      DO 20 I=14,2*NC+2
   20    RB4(I)=0.
C Output new header
      IF (SEMROW(2,RB4,NFMCOM,1,1,LP2)) RETURN
C
C Begin level 1 loop - over all lines
C ------------------
      DO 90 J=2,NROW,2
      IF (SEMROW(1,RB1,NFMCOM,J,1,LP1)) RETURN
      IF (SEMROW(1,RB2,NFMCOM,J+1,1,LP1)) RETURN
      LAST=RB1(1)
      STATUS=RB1(2)
      DSTAR2=(RB2(1)*UX+RB2(2)*VX)**2+(RB2(1)*UY+RB2(2)*VY)**2
      IH=RB2(1)
      IK=RB2(2)
C
C This line OK?
      IF (LAST.LT.4) THEN
         WRITE (RECORD,30) IH,IK
   30    FORMAT ('No samples found for line ',I3,1H,,I3)
         IF (SEMDIA(RECORD,NDIERR)) RETURN
         ERROR=10
         RETURN
      ENDIF
      IF (STATUS.EQ.3) THEN
         WRITE (RECORD,40) IH,IK
   40    FORMAT ('Line ',I3,1H,,I3,' is already transformed')
         IF (SEMDIA(RECORD,NDIERR)) RETURN
         ERROR=10
         RETURN
      ENDIF
C
C Yes - modify the data
C
C Establish lateral ranges
      CONV=SIZE/2./ZMAX
      OCONV=OSIZE/2./OZMAX
      SIZE2=SIZE*2+2
C
C Begin level 2 loop: over sample positions
C -----------------------------------------
      X0=OSIZE/2+1
      X1=OSIZE
      Z=-ZMAX
      DO 70 I=3,SIZE2,2
         IF (STATUS.EQ.1) THEN
C
C Raw sample collection: obtain local sample average
            IF (LLRES2(RB1,RB2,LAST,Z,W,X,Y,CSYMM)) GOTO 50
C
C Fitted and interpolated: use linear interpolation
         ELSE
            X=Z*OCONV+X0
            IF (X.LT.1.) GOTO 50
            IF (X.GT.X1) GOTO 50
            K=X
            F=X-K
            FC=1.-F
            K=2*K+1
            X=RB1(K)*FC+RB1(K+2)*F
            Y=RB1(K+1)*FC+RB1(K+3)*F
         ENDIF
C
C Impose mask?
         IF (R.GT.0.) THEN
            F=SQRT(Z*Z+DSTAR2)-R
            IF (F.GT.0.) THEN
               F=(F/W)**2
               IF (F.GT.10.) GOTO 50
               F=EXP(-F)
               X=X*F
               Y=Y*F
            ENDIF
         ENDIF
C
C Store the result
         RB3(I)=X
         RB3(I+1)=Y
         GOTO 60
C Store zero
   50    RB3(I)=0.
         RB3(I+1)=0.
   60    Z=Z+1./CONV
C
C End of level 2 loop
   70 CONTINUE
C
C Set flags and output revised samples
      RB3(1)=SIZE2
      RB3(2)=2.
      IF (SEMROW(2,RB3,NFMCOM,J,1,LP2)) RETURN
C
C Generate revised Z values
      Z=-ZMAX
      DO 80 I=3,SIZE2,2
         RB2(I)=Z
         RB2(I+1)=0.
         Z=Z+1./CONV
   80 CONTINUE
C Set flags and output Z values
      RB2(1)=IH
      RB2(2)=IK
      IF (SEMROW(2,RB2,NFMCOM,J+1,1,LP2)) RETURN
C
C End of level 1 loop
   90 CONTINUE
      RETURN
C
      END
C Semper VI subsidiary module LLRES2
C
      LOGICAL FUNCTION LLRES2(B1,B2,N,ZSTAR,WIDTH,X,Y,CSYMM)
C
C Obtains in (X,Y) estimate of lattice line value at ZSTAR by averaging
C sample values over a range WIDTH of ZSTAR; samples are in B1(3/4,5/6..
C N) with corresponding ZSTAR values in B2(3,5..N); average is made
C with triangular rather than uniform weighting.
C
C Interpolates between nearest points if there are no samples available
C in the requested interval
C
      LOGICAL CONJ,CSYMM
      REAL B1(N),B2(N)
C
      LLRES2=.FALSE.
      X=0.
      Y=0.
C
      WB2=WIDTH/2.
      Z1=ZSTAR-WB2
      Z2=ZSTAR+WB2
      T1=1000.
      T2=1000.
      TW=0.
C
C Loop over all available samples
      DO 20 I=3,N,2
         Z=B2(I)
         SX=B1(I)
         SY=B1(I+1)
         CONJ=.FALSE.
C If in range, accumulate it
   10    IF (Z.GE.Z1) THEN
            IF (Z.LE.Z2) THEN
               W=WB2-ABS(Z-ZSTAR)
               X=X+SX*W
               Y=Y+SY*W
               TW=TW+W
C Otherwise, note nearest to each limit
            ELSE IF (Z-ZSTAR.LT.T2) THEN
               T2=Z-ZSTAR
               X2=SX
               Y2=SY
            ENDIF
         ELSE IF (ZSTAR-Z.LT.T1) THEN
            T1=ZSTAR-Z
            X1=SX
            Y1=SY
         ENDIF
C Repeat for conjugate sample?
         IF (CSYMM.AND..NOT.CONJ) THEN
            CONJ=.TRUE.
            SY=-SY
            Z=-Z
            GOTO 10
         ENDIF
   20 CONTINUE
C
C If samples found in range, return average
      IF (TW.NE.0) THEN
         X=X/TW
         Y=Y/TW
         RETURN
C Otherwise, interpolate if samples available on both sides
      ELSE IF (T1.LT.1000..AND.T2.LT.1000.) THEN
         F=T1/(T1+T2)
         X=X1+(X2-X1)*F
         Y=Y1+(Y2-Y1)*F
C Failing this too, report no value available
      ELSE
         LLRES2=.TRUE.
      ENDIF
      RETURN
C
      END
C Semper VI processing module LLSL
C
      SUBROUTINE LLSL
C
C Provides verbs LLSECTION and LLLAYER
C - LLSECTION generates in TO a central section with given PHI,ALPHA,PSI
C   through the l-l set LP1, by averaging for each l-l any sample
C   values found within a given z-WIDTH of the appropriate value.
C - LLLAYER generates in TO,TO+1.. one or more Z-layers through an
C   interpolated l-l set, picking out corresponding points from each
C   line; sets U,V ready for image space 'EXTRACT UV...'
C
      LOGICAL SEMOPN,SEMCLS,SEMCEN,SEMROW,SEMLU,VARSET,LLM3,LLRES2
      LOGICAL SEMCON,SEMDIA,LAYER,CSYMM
      REAL ROT(3,3),K,KS
      INTEGER CLASS,FORM,SIZE,SYMTYP,SOURCE,OUTPUT
      INTEGER*4 I4MAXP,I4P
C
C Packed names
      PARAMETER (NSIZE=30786,NSI2=30792,NWIDTH=-5165)
      PARAMETER (NPHI=25929,NALPHA=2096,NPSI=26369,NLLLAY=19692)
      PARAMETER (NLAYER=19265,NLA2=19272,NXMAX=-6922)
      PARAMETER (NU=-1601,NU2=-2881,NV=-3201,NV2=-4481)
      PARAMETER (NFROM=10335,NTO=-601)
C
      INCLUDE 'COMMON'
C
      LAYER=VERB.EQ.NLLLAY
      SOURCE=IVALPN(NFROM)
      OUTPUT=IVALPN(NTO)
C
C Check source suitable
      NROW=NROWS(LP1)
      IF (SEMROW(1,RB4,NFMCOM,1,1,LP1)) RETURN
      IF (NROW.LT.3.OR.CLASSN(LP1).NE.NCLUND.OR.RB4(2).NE.9.) THEN
         WRITE (RECORD,10) SOURCE
   10    FORMAT ('Picture',I5,' is not a lattice-line set')
         IF (SEMDIA(RECORD,NDIERR)) RETURN
         ERROR=10
         RETURN
      ENDIF
C
      SIZE=RB4(11)
      IF (SIZE.EQ.0) SIZE=NCOLS(LP1)-1
      UX=RB4(3)
      UY=RB4(4)
      VX=RB4(5)
      VY=RB4(6)
      XMAX=VAL(NXMAX)
      IF (XMAX.LE.0.) XMAX=1E10
      XMAX=XMAX*XMAX
      ZMAX=RB4(8)
      SYMTYP=RB4(12)
      IF (LLM3(SYMTYP,NSRLS,S11,S12,S21,S22,CSYMM)) RETURN
C
C Establish size of output
      NCOP=33
      IF (VARSET(NSIZE)) NCOP=IVAL(NSIZE)
      NCOP2=2*NCOP
      NROP=64
      IF (VARSET(NSI2)) NROP=IVAL(NSI2)
C If too large for RB2/RB3, reduce dimensions pro rata
      NCBUF=NCOP
      NRBUF=NROP
      X=REAL(NCBUF)*REAL(NRBUF)/REAL(LNBUF/LNCOMP)/2.
      IF (X.GT.1.) THEN
         X=SQRT(X)
         NCBUF=NCOP/X
         NRBUF=NROP/X
      ENDIF
      I4MAXP=NCBUF
      I4MAXP=I4MAXP*NRBUF*2
C
C Pick up keys acc to verb
      IF (LAYER) THEN
C NB: layers now numbered from back rather than from centre,
C for consistency with the rest of Semper VI
         LAY1=IVAL(NLAYER)
         LAY2=IVAL(NLA2)
         IF (LAY2.EQ.0) LAY2=LAY1
         IF (LAY1.LE.0.OR.LAY1.GT.SIZE) LAY1=1
         IF (LAY2.LE.0.OR.LAY2.GT.SIZE) LAY2=SIZE
         NL=LAY1
      ELSE
         LAY1=1
         LAY2=1
         WIDTH=VAL(NWIDTH)
         IF (WIDTH.LE.0.) WIDTH=ZMAX/4.
         PHI=VAL(NPHI)
         ALPHA=VAL(NALPHA)
         PSI=VAL(NPSI)
C Check than neither ALPHA nor PSI is near pi/2 (or odd multiple)
         IF (ABS(COS(ALPHA)).LT.1E-3) THEN
            IDERR=NALPHA
            ERROR=3
            RETURN
         ENDIF
         IF (ABS(COS(PSI)).LT.1E-3) THEN
            IDERR=NPSI
            ERROR=3
            RETURN
         ENDIF
C Generate rotation matrix elements
         CALL LLM2(PHI,ALPHA,PSI,ROT)
      ENDIF
C
C Begin level 1 loop - over output layers
C ------------------
C Initialise output to zero
   20 DO 30 I=1,I4MAXP
   30 RB5(I)=0.
C
C Begin level 2 loop - over lattice lines from asymm-unit
C ------------------
      DO 70 J=2,NROW,2
C
C Load/initialise line
      IF (SEMROW(1,RB1,NFMCOM,J,1,LP1)) RETURN
      IF (SEMROW(1,RB2,NFMCOM,J+1,1,LP1)) RETURN
      LAST=RB1(1)
      H=RB2(1)
      K=RB2(2)
C Sp freq too high?
      X=H*UX+K*VX
      Y=H*UY+K*VY
      IF (X*X+Y*Y.GT.XMAX) GOTO 70
C No data recorded?
      IF (LAST.LT.4) GOTO 70
C If LLL, line data must be evenly sampled
      IF (LAYER) THEN
         IF (RB1(2).LT.2.) THEN
            WRITE (RECORD,40) INT(H),INT(K)
   40       FORMAT ('Line ',I4,',',I4,' is not yet interpolated')
            IF (SEMDIA(RECORD,NDIERR)) RETURN
            ERROR=10
            RETURN
         ENDIF
      ENDIF
C
C Begin level 3 loop - over symmetry-related lines
C ------------------
      HS=H
      KS=K
      DO 60 IS=1,NSRLS
C
C Layer wanted: lift sample value directly
         IF (LAYER) THEN
            X=RB1(2*NL+1)
            Y=RB1(2*NL+2)
C Section wanted: average samples over Z-strip
         ELSE
            X=HS*UX+KS*VX
            Y=HS*UY+KS*VY
C (ROT(3,3) is cos(PSI)cos(ALPHA) and cannot therefore vanish)
            Z=-(ROT(3,1)*X+ROT(3,2)*Y)/ROT(3,3)
            IF (LLRES2(RB1,RB2,LAST,Z,WIDTH,X,Y,CSYMM)) GOTO 50
         ENDIF
C
C Provided within buffered section, deposit sample directly..
         IF (HS.GE.0.) THEN
            I4P=NRBUF/2-INT(KS)
            I4P=(I4P*NCBUF+INT(HS)+1)*2
            IF (I4P.GT.0.AND.I4P.LE.I4MAXP) THEN
               RB5(I4P-1)=X
               RB5(I4P)=Y
            ENDIF
         ENDIF
C .. and/or conjugated to conjugate position
         IF (HS.LE.0.) THEN
            I4P=NRBUF/2+INT(KS)
            I4P=(I4P*NCBUF-INT(HS)+1)*2
            IF (I4P.GT.0.AND.I4P.LE.I4MAXP) THEN
               RB5(I4P-1)=X
               RB5(I4P)=-Y
            ENDIF
         ENDIF
C
C Generate next symmetry-related line
   50    T=HS*S11+KS*S12
         KS=HS*S21+KS*S22
         HS=T
C Avoid repetition if symmetry-related loop stops short,
C e.g. on (0,0) line
         IF (HS.EQ.H.AND.KS.EQ.K) GOTO 70
C
C End of level 3 loop - repeat for next symmetry-related line
   60 CONTINUE
C
C End of level 2 loop - repeat for next line
   70 CONTINUE
C
C Open output
      IDERR=OUTPUT
C Avoid implicit deletion (LLLAYER multiple output mode) for safety
      IF (LAYER) THEN
         IF (LAY1.NE.LAY2) THEN
            IF (.NOT.SEMOPN(1,OUTPUT,NC,NR,NLAY,CLASS,FORM,LP2)) THEN
               ERROR=46
               RETURN
            ELSE
               IF (ERROR.NE.30) RETURN
               ERROR=0
            ENDIF
         ENDIF
      ENDIF
      LP2=LP1
      IF (SEMOPN(2,IDERR,NCOP,NROP,1,NCLFOU,NFMCOM,LP2)) RETURN
C Set section origin left
      IF (SEMCEN(LP2,1,NROP/2+1,1)) RETURN
      OUTPUT=OUTPUT+1
C
C Break up section buffer and output
C
      J1=NROP/2+1-NRBUF/2
      J2=J1+NRBUF-1
C Pad top?
      DO 80 I=1,NCOP2
         RB1(I)=0.
   80 CONTINUE
      DO 90 J=1,J1-1
         IF (SEMROW(2,RB1,NFMCOM,J,1,LP2)) RETURN
   90 CONTINUE
      I4P=1
      DO 110 J=J1,J2
C Copy row data
         DO 100 I=1,2*NCBUF
            RB2(I)=RB5(I4P)
            I4P=I4P+1
  100    CONTINUE
C Output
         IF (SEMROW(2,RB2,NFMCOM,J,1,LP2)) RETURN
  110 CONTINUE
C Pad bottom?
      DO 120 J=J2+1,NROP
         IF (SEMROW(2,RB1,NFMCOM,J,1,LP2)) RETURN
  120 CONTINUE
C
C End of level 1 loop - repeat for next layer?
      IF (LAYER) THEN
         NL=NL+1
         IF (NL.LE.LAY2) THEN
C Close output
            IF (SEMCLS(LP2)) RETURN
            GOTO 20
         ENDIF
      ENDIF
C
C Set U,V for image space extraction:
C replace reciprocal by real space vectors..
      X=ABS(UX*VY-UY*VX)
      T=UX
      UX=VY/X
      VY=T/X
      T=UY
      UY=-VX/X
      VX=-T/X
C ..rotated if LAYER, ..
      IF (.NOT.LAYER) THEN
         T=UX
         UX=ROT(1,1)*T+ROT(1,2)*UY
         UY=ROT(2,1)*T+ROT(2,2)*UY
         T=VX
         VX=ROT(1,1)*T+ROT(1,2)*VY
         VY=ROT(2,1)*T+ROT(2,2)*VY
      ENDIF
C ..then inverse
      X=UX*VY-UY*VX
      NCOP=2*(NCOP-1)
      IF (SEMLU(1,NU,VY*NCOP/X)) RETURN
      IF (SEMLU(1,NU2,-UY*NROP/X)) RETURN
      IF (SEMLU(1,NV,-VX*NCOP/X)) RETURN
      IF (SEMLU(1,NV2,UX*NROP/X)) RETURN
      RETURN
C
      END
C Semper VI processing module LLFT1D
C
      SUBROUTINE LLFT1D
C
C Transforms l-l set in LP1 line by line to TO - created with
C reduced row length if possible; status 1 lines (raw sample
C collections) are faulted; status 2 lines are transformed and
C their max ptr forced appropriately; status 3 lines are copied
C
      LOGICAL SEMOPN,SEMROW,SEMDIA
      INTEGER SIZE
C
C Packed names
      PARAMETER (NFROM=10335,NTO=-601)
C
      INCLUDE 'COMMON'
C
C Check source suitable
      NROW=NROWS(LP1)
      IF (SEMROW(1,RB4,NFMCOM,1,1,LP1)) RETURN
      IF (NROW.LT.3.OR.CLASSN(LP1).NE.NCLUND.OR.RB4(2).NE.9.) THEN
         WRITE (RECORD,10) IVALPN(NFROM)
   10    FORMAT ('Picture',I5,' is not a lattice-line set')
         GOTO 70
      ENDIF
      SIZE=RB4(11)
      IF (SIZE.EQ.0) THEN
         WRITE (RECORD,20)
   20    FORMAT ('Lattice lines not yet interpolated')
         GOTO 70
      ENDIF
      RNORM=1./SIZE
C Real space Z sampling: 1/(2.ZMAX)
      DZ=.5/RB4(8)
C
C Create output
      LP2=LP1
      IF (SEMOPN(2,IVALPN(NTO),SIZE+1,NROW,1,NCLUND,NFMCOM,LP2)) RETURN
C Copy first row
      IF (LP1.NE.LP2) THEN
         IF (SEMROW(2,RB4,NFMCOM,1,1,LP2)) RETURN
      ENDIF
C
C Begin loop over all lines
      DO 60 J=2,NROW,2
      IF (SEMROW(1,RB1,NFMCOM,J,1,LP1)) RETURN
      IF (SEMROW(1,RB2,NFMCOM,J+1,1,LP1)) RETURN
      IF (RB1(2).EQ.1.) THEN
         WRITE (RECORD,30) INT(RB2(1)),INT(RB2(2))
   30    FORMAT ('Line',I4,',',I4,' not yet interpolated')
         GOTO 70
      ENDIF
C
C If not already transformed..
      IF (RB1(2).NE.3.) THEN
C Zero lhs to guarantee c-symmetry, and inverse-transform data
         RB1(4)=0.
         CALL FT1D(RB1(3),SIZE,1,.FALSE.,.FALSE.,.FALSE.)
C Normalise
         DO 40 I=1,2*SIZE
   40    RB1(I+2)=RB1(I+2)*RNORM
C Force max ptr and update status
         RB1(1)=2*SIZE+2
         RB1(2)=3.
C Generate fresh (real space) Z values
         Z=-(SIZE/2)*DZ
         DO 50 I=1,2*SIZE,2
         RB2(I+2)=Z
   50    Z=Z+DZ
C Output line
         IF (SEMROW(2,RB1,NFMCOM,J,1,LP2)) RETURN
         IF (SEMROW(2,RB2,NFMCOM,J+1,1,LP2)) RETURN
      ENDIF
C
C End of level 1 loop
   60 CONTINUE
      RETURN
C
C Error return
   70 IF (SEMDIA(RECORD,NDIERR)) RETURN
      ERROR=10
      RETURN
C
      END
