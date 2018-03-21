C Semper 6 processing module DMANU
C
      SUBROUTINE DMANU
C
C Provides verbs COMPRESS, RENUMBER and DIRECTORY
C - disc management utilites operating at the SD level
C
      INTEGER IVAL,IVALPN
      LOGICAL SEMKTX,SEMMED,SEMCLS,DISC,ABANDN,VARSET
      LOGICAL PRUFLS,PRUCOM,PRUIND,PRUDIR,PRUENT,OPTNO
      LOGICAL SEMCON,SEMDIA
C
      INCLUDE 'COMMON'
C
      REAL FNU,FNF,FN2
C
      INTEGER*4 SDL(256),I42,I4N1,I4N2,I4NF,I4NU,I4NC,I4N,NTR
      INTEGER*4 BLKN,DSIZE,SSIZE,DIRSZ4
      PARAMETER (I42=2)
C
      INTEGER LABEL(256),OUT(256),OUT2(256)
      INTEGER I,ILL,ISL,ITL,ITY,N,NF,NU,NPIC,NSD,NUM1,NUM2
      INTEGER FROM,DEVICE,SDPTR,SDPTR2,LENOUT,LEN2,MEDIUM,MAXBL
      LOGICAL LVERIF,REQABA
C
C Packed names
C
      INTEGER NRENUM,NFROM,NTO,NDEVIC,NDIREC,NCOMPR,NPROGR,NAS,NVERIF
      PARAMETER (NRENUM=29014,NFROM=10335,NTO=-601,NDEVIC=6622)
      PARAMETER (NDIREC=6778,NCOMPR=5413,NPROGR=26335,NAS=2360)
      PARAMETER (NVERIF=-3419)
C
      CHARACTER*(PRNACT) PROG,PRAS
C
      EQUIVALENCE (RB2,SDL),(RB1,LABEL,OUT),(RB1(257),OUT2)
C
C See if verification is required (default = yes)
C
      LVERIF = .NOT.OPTNO(NVERIF)
C
C Initialise - check for program text options first
C
      IF (VERB.EQ.NRENUM) THEN
         IF (VARSET(NPROGR) .OR. VARSET(NAS)) THEN
            LENOUT=PRNACT
            IF (SEMKTX(NPROGR,'Program name (as textstring): ',
     +                 OUT,LENOUT,.FALSE.)) GOTO 250
            IF (LENOUT .EQ. 0) GOTO 250
C
            CALL SEMCHS(PROG,OUT,LENOUT)
            LEN2=PRNACT
            IF (SEMKTX(NAS,'New program name (as textstring): ',
     +                 OUT2,LEN2,.FALSE.)) GOTO 250
            IF (LEN2 .EQ. 0) GOTO 250
C
C Find a first entry
C
            DEVICE=0
            IF (PRUENT(DEVICE,ISL,LENOUT,OUT)) GOTO 250
            IF (ISL .EQ. 0) THEN
               ERROR = 125
               IDMESS = PROG(1:LENOUT)
               GOTO 250
            ENDIF
C
            IF (PROTN(DEVICE) .NE. 0) THEN
               ERROR = 41
               IDERR = DEVICE
               GOTO 250
            ENDIF
C
C Check new name does not already exist on device
C
            IF (PRUENT(DEVICE,I,LEN2,OUT2)) GOTO 250
            IF (I .NE. 0) THEN
               ERROR = 126
               IDMESS = PROG(1:LEN2)
               GOTO 250
            ENDIF
C
C Perform the rename and report the operation
C
            IF (PRUIND(1,DEVICE,ISL,I4N1,I4N2,ITY,ITL,ILL,LENOUT,OUT))
     +         GOTO 250
            IF (PRUIND(2,DEVICE,ISL,I4N1,I4N2,ITY,ITL,ILL,LEN2,OUT2))
     +         GOTO 250
            IF (PRUFLS(DEVICE)) GOTO 250
C
            IF (LVERIF) THEN
               CALL SEMCHS(PROG,OUT,LENOUT)
               CALL SEMCHS(PRAS,OUT2,LEN2)
               WRITE (RECORD,10) PROG(1:LENOUT),PRAS(1:LEN2)
               IF (SEMCON(RECORD)) GOTO 250
            ENDIF
C
   10       FORMAT ('Program ''',A,''' renamed as ''',A,'''')
C
            GOTO 250
         ENDIF
C
C Here if not a Program file
C
         FROM=IVALPN(NFROM)
         IDERR=FROM
         DEVICE=FROM/1000
      ELSE
         DEVICE=IVAL(NDEVIC)
         IDERR=DEVICE
      ENDIF
C
      IF (SEMMED(DEVICE,MEDIUM)) GOTO 250
      IF (.NOT.(MEDIUM.EQ.MEDDC.OR.MEDIUM.EQ.MEDVM)) THEN
         ERROR=29
         IDERR=DEVICE
         GOTO 250
      ENDIF
C
      I = DVTYP(DEVICE)
      IF (I.EQ.FLTRUN) THEN
C
C Compress,Directory (and Renumber/Rename - dealt with above) are
C allowed on a Program Text library
C
         IF (VERB .EQ. NCOMPR) THEN
            IF (PROTN(DEVICE) .NE. 0) THEN
                ERROR=41
                GOTO 250
            ENDIF
            IF (.NOT.PRUCOM(DEVICE)) THEN
               IF (LVERIF) THEN
                  WRITE (RECORD,20) DEVICE
   20             FORMAT ('Device ',I3,' compressed')
                  IF (SEMCON(RECORD)) GOTO 250
               ENDIF
            ENDIF
         ELSE IF (VERB .EQ. NDIREC) THEN
            IF (PRUDIR(DEVICE)) GOTO 250
         ELSE
            GOTO 260
         ENDIF
         GOTO 250
      ELSE IF (I.EQ.FLTPIC) THEN
C
C Fetch disc directory
C
         DIRSZ4=DRSIZ(DEVICE)*LNBLK
         IF (DISC(1,DEVICE,DIRSZ4,SDL,I42,NFMBYT,NFMBYT)) GOTO 250
C
         IF (VERB.EQ.NDIREC) GOTO 120
         IF (VERB.EQ.NRENUM) GOTO 90
      ELSE
C
C Nothing can be done with a Help library
C
         GOTO 260
      ENDIF
C
C Code for COMPRESS
C -----------------
C Close all disc/memory pictures
C
      DO 30 I=1,NLPS
         IF (DEVN(I).GT.0.AND.DEVN(I).LE.NDVS) THEN
            IF (MEDN(DEVN(I)).EQ.MEDDC.OR.MEDN(DEVN(I)).EQ.MEDVM) THEN
               IF (SEMCLS(I)) GOTO 250
            ENDIF
         ENDIF
   30 CONTINUE
C
C Find maximum number of blocks fitting buffer
C
      MAXBL=LNBUF/LNBLK
C
C Pass through SDs
C
      SDPTR=1
      SDPTR2=1
      BLKN=SDL(2)
      REQABA=.FALSE.
   40 NPIC=SDL(SDPTR)
C
C Terminator?
C
      IF (NPIC.LT.1000) THEN
         SSIZE=SDL(SDPTR+3)-SDL(SDPTR+1)
C
C Empty?
C
         IF (NPIC.GT.0) THEN
C
C Picture found: write new SD
C
            SDL(SDPTR2)=NPIC
            SDL(SDPTR2+1)=BLKN
            SDPTR2=SDPTR2+2
C
C Copy data
C
            I4N2=BLKN
            I4N1=SDL(SDPTR+1)
            IF (I4N2.NE.I4N1) THEN
               I4NC=0
   50          NTR=SSIZE-I4NC
               IF (NTR.GT.MAXBL) NTR=MAXBL
               I4N=NTR*LNBLK/LNINT
               IF (DISC(1,DEVICE,I4N,LABEL,I4N1,NFMINT,NFMINT)) GOTO 250
               IF (DISC(2,DEVICE,I4N,LABEL,I4N2,NFMINT,NFMINT)) GOTO 250
C
C Abandon requested?
C
               IF (ABANDN(ERROR)) THEN
                  IF (SEMDIA('Will abandon when possible',NDIMES))
     +               GOTO 250
                  REQABA=.TRUE.
                  ERROR=0
               ENDIF
C
               I4NC=I4NC+NTR
               I4N1=I4N1+NTR
               I4N2=I4N2+NTR
               IF (I4NC.NE.SSIZE) GOTO 50
            ENDIF
C
C Data copy complete: bump BLKN
C
            BLKN=BLKN+SSIZE
C
C Give up if abandon requested
C
            IF (REQABA) GOTO 70
         ENDIF
C
C Bump SDPTR
C
         SDPTR=SDPTR+2
         GOTO 40
      ENDIF
C
C Directory scan complete: record any final free segment
C and terminate
C
      DSIZE=SDL(SDPTR+1)
      IF (BLKN.NE.DSIZE) THEN
         SDL(SDPTR2)=0
         SDL(SDPTR2+1)=BLKN
         SDPTR2=SDPTR2+2
      ENDIF
C
      SDL(SDPTR2)=1000
      SDL(SDPTR2+1)=DSIZE
C
C Output new directory and confirm completion
C
   60 IF (DISC(2,DEVICE,DIRSZ4,SDL,I42,NFMBYT,NFMBYT)) GOTO 250
C
      IF (VERB.NE.NRENUM) THEN
         IF (LVERIF) THEN
            WRITE (RECORD,20) DEVICE
            IF (SEMCON(RECORD)) GOTO 250
         ENDIF
      ENDIF
      GOTO 250
C
C Abandon requested during COMPRESS: bump SDPTR to next non-empty
C
   70 SDPTR=SDPTR+2
      IF (SDL(SDPTR).LE.0) GOTO 70
C
C Insert one empty SD
C
      SDL(SDPTR2)=0
      SDL(SDPTR2+1)=BLKN
      SDPTR2=SDPTR2+2
C
C Copy remainder
C
   80 NPIC=SDL(SDPTR)
      SDL(SDPTR2)=SDL(SDPTR)
      SDL(SDPTR2+1)=SDL(SDPTR+1)
      SDPTR2=SDPTR2+2
      SDPTR=SDPTR+2
      IF (NPIC.LT.1000) GOTO 80
      GOTO 60
C
C Code for RENUMBER
C -----------------
C
   90 NUM1=FROM-1000*DEVICE
      NUM2=IVAL(NTO)
      NUM2=NUM2-NUM2/1000*1000
      IDERR=NUM2
      IF (NUM2.LT.0) THEN
         ERROR=28
         GOTO 250
      ENDIF
C
C Search directory
C
      SDPTR2=0
      SDPTR=1
  100 NPIC=SDL(SDPTR)
      IF (NPIC.LT.1000) THEN
         IF (NPIC.EQ.NUM2) THEN
            ERROR=46
            IDERR=NPIC
            GOTO 250
         ENDIF
         IF (NPIC.EQ.NUM1) SDPTR2=SDPTR
         SDPTR=SDPTR+2
         GOTO 100
      ENDIF
C
C Terminator found: does FROM exist?
C
      IF (SDPTR2.EQ.0) THEN
         ERROR=30
         IDERR=FROM
         GOTO 250
      ENDIF
C
C Check WP flag
C
      IF (DISC(1,DEVICE,LNLAB4,LABEL,SDL(SDPTR2+1),NFMINT,NFMBYT))
     +   GOTO 250
      IF (LABEL(LBWP).NE.0) THEN
         ERROR=41
         IDERR=FROM
         GOTO 250
      ENDIF
      SDL(SDPTR2)=NUM2
C
C Select result
C
      CALL SEMSZZ(1000*DEVICE+NUM2)
C
C Amend PCB also if picture open
C
      DO 110 I=1,NLPS
         IF (DEVN(I).EQ.DEVICE) THEN
            IF (PICN(I).EQ.NUM1) PICN(I)=NUM2
         ENDIF
  110 CONTINUE
      GOTO 60
C
C Code for DIRECTORY
C ------------------
C Print number of SDs describing pictures,
C SDs describing free segments, unused SDs, total blocks used,
C total free, largest free segment and first unused picture number
C
  120 NU=0
      NF=0
      I4NU=0
      I4NF=0
      I4N2=0
      NSD=1
  130 IF (SDL(NSD).NE.1000) THEN
         I4N1=SDL(NSD+3)-SDL(NSD+1)
         IF (SDL(NSD).GT.0) THEN
C
C Accumulate picture
C
            NU=NU+1
            I4NU=I4NU+I4N1
         ELSE
C
C Accumulate free segment
C
            NF=NF+1
            I4NF=I4NF+I4N1
            IF (I4N1.GT.I4N2) I4N2=I4N1
         ENDIF
         NSD=NSD+2
         GOTO 130
      ENDIF
C
C End of directory
C
      N=DIRSZ4/LNINT4/2-1-NU-NF
      FNU=REAL(I4NU)*REAL(LNBLK)/1024.
      FNF=REAL(I4NF)*REAL(LNBLK)/1024.
      FN2=REAL(I4N2)*REAL(LNBLK)/1024.
C
      WRITE (RECORD,140) DEVICE
  140 FORMAT ('Directory information for device:',I3)
      IF (SEMCON(RECORD)) GOTO 250
C
      WRITE (RECORD,150) NU
  150 FORMAT ('Directory slots describing pictures',I5)
      IF (SEMCON(RECORD)) GOTO 250
C
      WRITE (RECORD,160) NF
  160 FORMAT ('           describing free segments',I5)
      IF (SEMCON(RECORD)) GOTO 250
C
      WRITE (RECORD,170) N
  170 FORMAT ('                             unused',I5)
      IF (SEMCON(RECORD)) GOTO 250
C
      WRITE (RECORD,180) FNU/1000.,I4NU
      IF (SEMCON(RECORD)) GOTO 250
  180 FORMAT ('     Disc space used',F7.1,'MB =',I9,' blocks')
C
      WRITE (RECORD,190) FNF/1000.,I4NF
      IF (SEMCON(RECORD)) GOTO 250
  190 FORMAT ('                free',F7.1,'MB =',I9,' blocks')
C
      WRITE (RECORD,200) FN2/1000.,I4N2
      IF (SEMCON(RECORD)) GOTO 250
  200 FORMAT ('Largest free segment',F7.1,'MB =',I9,' blocks')
C
C Print first unused picture number
C
      DO 220 N=1,999
         DO 210 I=1,999
            IF (SDL(I).EQ.1000) GOTO 230
            IF (SDL(I).EQ.N) GOTO 220
  210    CONTINUE
         GOTO 230
  220 CONTINUE
C
  230 WRITE (RECORD,240) N
  240 FORMAT ('First unused picture number',I4)
      IF (SEMCON(RECORD)) GOTO 250
C
  250 RETURN
C
C Non picture error
C
  260 ERROR = 90
      IDERR = DEVICE
      GOTO 250
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
