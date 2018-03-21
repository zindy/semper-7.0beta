C Semper 6 subsidiary module SHOW2
C
      LOGICAL FUNCTION SHOW2(DEVICE,FULL)
C
      INTEGER DEVICE
      LOGICAL FULL
C
      LOGICAL ABANDN,PRUSOR,PRUIND,SEMCON,SEMTPS,SHOWNL,SHOW22
C
      INCLUDE 'COMMON'
C
      INTEGER*4 I4N,LABBLK,NDSLOT
C
      INTEGER LNSORT,INDENT
      PARAMETER (LNSORT=LNBUF/LNINT,INDENT=4)
      INTEGER SORTB(LNSORT),SORTI(LNSORT)
C
      INTEGER I,N,N1,N2,ISORT,ISL,ITL,LABLEN,ITYPE,NSORT
      INTEGER TEXT(80),NAME(80),CH,PTR,WIDTH,IND(5)
C
      INTEGER LABENT
      PARAMETER (LABENT=4)
C
      EQUIVALENCE (RB2,TEXT),(RB3,SORTB),(RB4,SORTI)
C
      SHOW2 = .TRUE.
C
      IF (SEMTPS(WIDTH,I)) GOTO 80
C
C Ask for a sorted list of all keys
C
      IF (PRUSOR(DEVICE,NDSLOT,SORTB,SORTI,NSORT)) GOTO 80
C
      WRITE(RECORD,10) DEVICE
   10 FORMAT ('Device ',I3,' contains:')
      IF (SHOWNL(RECORD)) GOTO 80
C
      IF (NSORT.LT.0) THEN
         IF (SEMCON('    (Too many programs to sort alphabetically)'))
     +      GOTO 80
      ELSE IF (NSORT.EQ.0) THEN
         IF (SEMCON('    No programs')) GOTO 80
      ENDIF
C
      IF (FULL) THEN
         IF (SEMCON('    --  Index Entry Usage   --   Program')) GOTO 80
         IF (SEMCON('    For Loop Label Buffer Free   Name')) GOTO 80
      ENDIF
C
C Scan directory, printing entries
C
      ISORT = 0
      PTR = 0
C
C Get next entry, switching code acc to whether sorted
C
   20 IF (ABANDN(ERROR)) GOTO 80
      IF (ERROR .NE. 0) GOTO 80
C
      IF (NSORT.GE.0) THEN
C
C Sorted: refer to pointer list
C
         IF (ISORT.GE.NSORT) GOTO 70
         ISORT=ISORT+1
         ISL=SORTB(ISORT)
      ELSE
C
C Unsorted: refer to entry directly
C
         I4N=ISORT
         IF (I4N.GE.NDSLOT) GOTO 70
         ISORT=ISORT+1
         ISL=ISORT
      ENDIF
C
C Get key and see if active
C
      IF (PRUIND(1,DEVICE,ISL,I4N,LABBLK,ITYPE,ITL,
     +             LABLEN,N,NAME)) GOTO 80
      IF (ITYPE.EQ.2) THEN
C
C Prefill line buffer with spaces if necessary
C
   30    IF (PTR.LE.INDENT) THEN
            DO 40 I=1,WIDTH
               TEXT(I)=KSPACE
   40       CONTINUE
            PTR=INDENT
         ENDIF
C
         IF (FULL) THEN
C
C Map and print line if anything already there?
C
            IF (PTR.GT.INDENT) THEN
               CALL SEMCHS(RECORD,TEXT,PTR)
               IF (SEMCON(RECORD(1:PTR))) GOTO 80
               PTR = 0
               GOTO 30
            ENDIF
C
            IF (SHOW22(DEVICE,LABBLK,LABLEN,
     +                 IND(1),IND(2),IND(3),IND(4),IND(5))) GOTO 80
            WRITE(RECORD,50) IND
   50       FORMAT(I3,2X,4(I2,4X))
            CALL SEMICS(RECORD,TEXT(INDENT+1),29)
            PTR = PTR + 29
         ELSE
C
C Is there space for the key on the current line?
C
            N1=PTR-INDENT
C
C Allow for two spaces after each item
C
            IF (N1.NE.0) N1=N1+2
            N1=(N1+9)/10*10
            N2=N1+N
            IF (N2.GT.WIDTH-INDENT) THEN
C
C Map and print line
C
               IF (PTR.GT.INDENT) THEN
                  CALL SEMCHS(RECORD,TEXT,PTR)
                  IF (SEMCON(RECORD(1:PTR))) GOTO 80
               ENDIF
               PTR = 0
               GOTO 30
            ENDIF
C
C It fits now, at least
C
            PTR=N1+INDENT
         ENDIF
C
C Copy key chars
C
         DO 60 I=1,N
            PTR=PTR+1
            CH=NAME(I)
            IF (I.EQ.1) THEN
               IF (CH.GE.KLCA.AND.CH.LE.KLCZ) CH=CH-KLCA+KUCA
            ENDIF
            TEXT(PTR)=CH
   60    CONTINUE
      ENDIF
      GOTO 20
C
C End of directory: flush line buffer and quit
C
   70 IF (PTR.GT.INDENT) THEN
         CALL SEMCHS(RECORD,TEXT,PTR)
         IF (SEMCON(RECORD(1:PTR))) GOTO 80
      ENDIF
C
      SHOW2 = .FALSE.
C
   80 RETURN
C
C Copyright (C) 1987-1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 system module SHOW22
C
      LOGICAL FUNCTION SHOW22(DEVICE,LABBLK,LABLEN,
     +                               NFOR,NLOO,NLAB,NLIN,NFRE)
C
C Reads a procedure index and analyses it
C Returns TRUE if error.
C
      INTEGER DEVICE,LABLEN,NFOR,NLOO,NLAB,NLIN,NFRE
      INTEGER*4 LABBLK
      LOGICAL DISC
C
      INCLUDE 'COMMON'
C
      INTEGER*4 N4
      INTEGER LABNUM,LABPTR,IDENT
C
      INTEGER LABINF(LNINDX),LABENT,LABMAX
      PARAMETER (LABENT=4,LABMAX=(LNINDX/LABENT)-1)
C
      SHOW22 = .TRUE.
C
      N4 = LABLEN
      N4 = N4*LNBLK4
      N4 = N4 / LNINT
C
      IF (DISC(1,DEVICE,N4,LABINF,LABBLK,NFMINT,NFMINT)) GOTO 20
      NFOR = 0
      NLOO = 0
      NLAB = 0
      NLIN = 0
      NFRE = LABMAX + 1
      LABNUM = 0
C
   10    LABPTR = LABNUM*LABENT
         LABNUM = LABNUM + 1
         IDENT = LABINF(LABPTR+1)
         IF (IDENT .EQ. TIDLIN) THEN
            NLIN = NLIN + 1
         ELSE IF (IDENT .EQ. TIDFOR) THEN
            NFOR = NFOR + 1
         ELSE IF (IDENT .EQ. TIDLOO) THEN
            NLOO = NLOO + 1
         ELSE IF (IDENT .EQ. TIDLAB) THEN
            NLAB = NLAB + 1
         ENDIF
         NFRE = NFRE - 1
         IF (IDENT .NE. TIDEND) GOTO 10
C
C Done - return results
C
      SHOW22 = .FALSE.
C
   20 RETURN
C
C Copyright (C) 1993:  Synoptics Ltd,  All Rights Reserved
C
      END
