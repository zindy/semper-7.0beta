C Semper 6 processing module SAVER
C
      SUBROUTINE SAVER
C
C SAVE command
C
      REAL    VAL
      INTEGER IVAL,IVALPN !,IPACK
      LOGICAL OPT,OPTNO,VARSET,SETVAR,ASSNEW,ASSNAM
      LOGICAL SEMOPN,SEMCLS,SEMROW,SEMLNF,SEMCON
C
      REAL      RSIZE,RMIN
      INTEGER*4 NBLKS,FLSIZE,LNFRM4,NCOL4,NROW4,NLAY4
      INTEGER   N1,N2,ND1,ND2,NP1,NP2,OLDDEV,NEWDEV,OLDPIC,NEWPIC
      INTEGER   NP,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LNFORM
      INTEGER   I,J,K,N,SLOTS,SLTMAX,DRSIZE
      LOGICAL   LVERIF,LRENUM
C
      INCLUDE 'COMMON'
C
      INTEGER PICNUM(999),NAME(FILMAX+1)
C
      EQUIVALENCE (PICNUM,RB4),(NAME,RB5)
C
      INTEGER*4 I40
      PARAMETER ( I40 = 0 )
C
      INTEGER*4 LABSZ4
      PARAMETER ( LABSZ4 = LABSZE )
C
C Fetch pair of values for FROM key
C
      N1=IVALPN(10335)
      N2=IVALPN(10352)
C
C Separate device and picture numbers
C
      ND1=N1/1000
      ND2=N2/1000
C
      NP1=N1-1000*ND1
      NP2=N2-1000*ND2
C
C Fault bad value(s) for FROM key (bad device/picture number,
C different device number, invalid picture number range)
C
      IF (ND1.LT.1.OR.ND1.GT.NDVS.OR.ND2.LT.1.OR.ND2.GT.NDVS.OR.
     +    NP1.LT.1.OR.NP1.GT.999.OR.NP2.LT.1.OR.NP2.GT.999.OR.
     +    ND1.NE.ND2.OR.NP1.GT.NP2) THEN
         ERROR=3
         IDERR=10335
         GOTO 70
      ENDIF
C
C Set up source device number
C
      OLDDEV=ND1
C
C Determine number and disc space for source pictures
C
      N=0
      NBLKS=0
C
      DO 10 NP=NP1,NP2
C
C Set up source picture number
C
         NPIC=1000*OLDDEV+NP
C
C Try to open source picture
C
         IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP1)) THEN
C
C Check error return code
C
            IF (ERROR.EQ.30) THEN
               ERROR=0
            ELSE
               GOTO 70
            ENDIF
C
C Picture exists
C
         ELSE
C
C Close picture
C
            IF (SEMCLS(LP1)) GOTO 70
C
C Determine space required for this picture (picture label + data)
C
            NCOL4=NCOL
            NROW4=NROW
            NLAY4=NLAY
C
            IF (SEMLNF(FORM,LNFORM)) GOTO 70
            LNFRM4=LNFORM
C
            NBLKS=NBLKS+LABSZ4+(1+(NCOL4*LNFRM4-1)/LNBLK4)*NROW4*NLAY4
C
C Add picture number to source list
C
            N=N+1
            PICNUM(N)=NP
         ENDIF
   10 CONTINUE
C
C Fault if no pictures found
C
      IF (N.EQ.0) THEN
         ERROR=58
         GOTO 70
      ENDIF
C
C Set up destination device number
C
      IF (VARSET(6622)) THEN
C
C Fetch value for DEVICE key
C
         NEWDEV=IVAL(6622)
C
C Fault bad device number
C
         IF (NEWDEV.LT.1.OR.NEWDEV.GT.NDVS) THEN
            ERROR=76
            IDERR=NEWDEV
            GOTO 70
         ENDIF
C
C Fault if device is already assigned
C
         IF (MEDN(NEWDEV).NE.0) THEN
            ERROR=121
            IDERR=NEWDEV
            GOTO 70
         ENDIF
C
C Find a free device number
C
      ELSE
         DO 20 NEWDEV=2,NDVS
            IF (MEDN(NEWDEV).EQ.0) GOTO 30
   20    CONTINUE
C
C Fault no free device numbers
C
         ERROR=45
         GOTO 70
C
C Free device number found
C
   30    CONTINUE
      ENDIF
C
C See if SIZE key is set
C
      IF (VARSET(30786))THEN
C
C Fetch value for SIZE key
C
         RSIZE=VAL(30786)
C
C Determine minimum value for SIZE key
C
         RMIN=REAL(NBLKS)*REAL(LNBLK)/1024.0
C
C Fault bad value for SIZE key
C
         IF (RSIZE.LT.RMIN) THEN
            ERROR=77
            WRITE (IDMESS,80) RMIN
            GOTO 70
         ENDIF
C
C Convert size into logical blocks
C
         FLSIZE=1.0+(1024.0*RSIZE-1.0)/REAL(LNBLK)
C
C See if SLOTS key is set
C
         IF (VARSET(30895)) THEN
C
C Fetch value for SLOTS key
C
            SLOTS=IVAL(30895)
C
C Fault bad value for SLOTS key
C
            IF (SLOTS.LT.N+2) THEN
               ERROR=77
               WRITE (IDMESS,90) N+2
               GOTO 70
            ENDIF
C
C Otherwise, default number of slots = maximum allowed
C
         ELSE
            SLOTS=2002
         ENDIF
C
C Otherwise, data size and number of slots defaults to minimum required
C
      ELSE
         FLSIZE=NBLKS
         SLOTS=N+2
      ENDIF
C
C Determine required size for directory
C
      SLTMAX=LNBUF/LNINT4/2
C
      DRSIZE=1+(MIN(SLOTS,2002,SLTMAX)*2*LNINT4-1)/LNBLK
C
C Determine overall file size = header + directory + data
C
      FLSIZE=1+DRSIZE+FLSIZE
C
C Assign new picture disc
C
      IF (ASSNEW(NEWDEV,FLTPIC,FLSIZE,DRSIZE)) GOTO 70
C
C See if VERIFY option is set
C
      LVERIF=OPT(-3419)
C
C If so, confirm device assignment
C
      IF (LVERIF) THEN
         WRITE (RECORD,100) NEWDEV
         IF (SEMCON(RECORD(1:18))) GOTO 70
      ENDIF
C
C See if option RENUMBER is set
C
      LRENUM=OPT(29014)
C
C Copy source pictures to new picture disc
C
      DO 60 I=1,N
C
C Set up source picture number
C
         OLDPIC=1000*OLDDEV+PICNUM(I)
C
C Set up destination picture number
C
         IF (LRENUM) THEN
            NEWPIC=1000*NEWDEV+I
         ELSE
            NEWPIC=1000*NEWDEV+PICNUM(I)
         ENDIF
C
C Open source picture
C
         IF (SEMOPN(1,OLDPIC,NCOL,NROW,NLAY,CLASS,FORM,LP1)) GOTO 70
C
C Open destination picture
C
         LP2=LP1
         IF (SEMOPN(2,NEWPIC,NCOL,NROW,NLAY,CLASS,FORM,LP2)) GOTO 70
C
C Copy data from source to destination picture
C
         DO 50 K=1,NLAY
            DO 40 J=1,NROW
               IF (SEMROW(1,RB1,FORM,J,K,LP1)) GOTO 70
               IF (SEMROW(2,RB1,FORM,J,K,LP2)) GOTO 70
   40       CONTINUE
   50    CONTINUE
C
C Close source and destination pictures
C
         IF (SEMCLS(LP1)) GOTO 70
         IF (SEMCLS(LP2)) GOTO 70
C
C If VERIFY option is set, confirm data copy
C
         IF (LVERIF) THEN
            WRITE (RECORD,110) OLDPIC,NEWPIC
            IF (SEMCON(RECORD(1:34))) GOTO 70
         ENDIF
   60 CONTINUE
C
C See if option NODEASSIGN is set
C
      IF (OPTNO(6601)) THEN
C
C Return device number in variable N
C
         IF (SETVAR(22400,REAL(NEWDEV))) GOTO 70
C
C Otherwise, deassign new picture disc
C
      ELSE
C
C Fetch device file name
C
         IF (ASSNAM(1,NEWDEV,NAME)) GOTO 70
C
C Close the disc file
C
         CALL MCDC61(6,NEWDEV,I40,0,NAME,ERROR)
         IF (ERROR.NE.0) GOTO 70
C
C Record device deassignment
C
         MEDN(NEWDEV)=0
C
C Verify successful deassignment
C
         IF (LVERIF) THEN
            WRITE (RECORD,120) NEWDEV
            IF (SEMCON(RECORD(1:20))) GOTO 70
         ENDIF
      ENDIF
C
   70 RETURN
C
   80 FORMAT ('Value for size key is too small (minimum value =',
     +        F10.1,')')
   90 FORMAT ('Value for slots key is too small (minimum value =',
     +        I5,')')
  100 FORMAT ('Device',I3,' assigned')
  110 FORMAT ('Saved picture',I5,' as picture',I5)
  120 FORMAT ('Device',I3,' deassigned')
C
C Copyright (C) 1993:  Synoptics Ltd,  All Rights Reserved
C
      END
