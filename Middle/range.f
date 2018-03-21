C Semper 6 processing module RANGE
C
      LOGICAL FUNCTION RANGE(IOP,LPN)
C
C Manages picture range determination and recording: opcodes are
C  1: normal scan, recovering range if possible from label and
C     recording any new range determined
C  2: unconditional scan - as 1, but ignoring any existing record;
C     this is only intended as an emergency let-out in case the
C     system tries to be too clever for itself.. ?? probably not needed
C     in view of SURVEY FULL, which relies on MEANSD instead
C  3: delete any existing record
C  4: record current range
C
C For IOP=1,2: needs SMGL1 set to indicate whether subregion, if and so
C also needs SMGI1-6 set to define it (as for TSTSRG)
C
C Range is exchanged with calling module via (fixed) variables
C VMIN,VMAX, and recorded in picture labels in decimal char form
C
      LOGICAL SEMROW,OPT,SEMXA1,SEMLAB,AVAIL,SUBREG,SEMMON
      INTEGER LABEL(256),DEVICE,PTR,IOP,LPN,L,L1,L2,M1,M2,N,N1,N2,I,J
      INTEGER IB1(256),NPRESE,NSURVE,IMIN,IMAX,IA,INFORM
      REAL A
C
C Packed names
C
      PARAMETER (NPRESE=26325,NSURVE=31258)
C
      INCLUDE 'COMMON'
C
      EQUIVALENCE (RB1,IB1,LABEL)
      EQUIVALENCE (SMGL1,SUBREG)
C
C ------ MONITOR ------
C
      IF (MONIT) THEN
         WRITE (RECORD,10) IOP,LPN,WSTAT(LPN)
         IF (SEMMON(RECORD,'RANGE',4)) GOTO 190
      ENDIF
C
   10 FORMAT ('!-RANGE called: op,lpn,wstat',3I3)
C
C ------ ------- ------
C
C Initialise
C
      DEVICE=DEVN(LPN)
      INFORM=FORMN(LPN)
C
C Switch code according to opcode
C
      IF (IOP-2)20,40,150
C
C Normal mode: if PRESET, return at once unless VERB is SURVEY
C
   20 IF (VERB.NE.NSURVE) THEN
         IF (OPT(NPRESE)) GOTO 170
      ENDIF
C
C Recover range from label? not if subregion or written to..
C
      IF (SUBREG) GOTO 40
      IF (WSTAT(LPN).GT.0) GOTO 40
C
C Fetch label
C
      IF (SEMLAB(1,LABEL,LPN)) GOTO 190
      IF (.NOT.LBLINC) GOTO 40
C
C Decode range
C
      N=LABEL(LBNCRR)+LBNCRR
      PTR=LBRR1
      IF (SEMXA1(2,LABEL,N,PTR,VMIN,I)) GOTO 40
      PTR=PTR+1
      IF (SEMXA1(2,LABEL,N,PTR,VMAX,I)) CONTINUE
C
C ------ MONITOR ------
C
      IF (MONIT) THEN
         WRITE (RECORD,30) VMIN,VMAX
         IF (SEMMON(RECORD,'RANGE',4)) GOTO 190
      ENDIF
C
   30 FORMAT ('!-RANGE: ',2G14.6,' recovered from label')
C
C ------ ------- ------
C
      GOTO 170
C
C Establish region to be scanned
C
   40 IF (SUBREG) THEN
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
      AVAIL=.FALSE.
C
C Set internal form, scanning bytes in integer mode
C
      IF (INFORM.EQ.0) INFORM=1
      IF (INFORM.EQ.3) THEN
         M1=2*M1-1
         M2=2*M2
      ENDIF
C
C Pass through rows
C
      AVAIL=.FALSE.
      DO 130 L=L1,L2
         DO 120 J=N1,N2
            IF (SEMROW(1,RB1,INFORM,J,L,LPN)) GOTO 140
C
C On first row, set initial MIN, MAX
C
            IF (L.EQ.L1) THEN
               IF (J.EQ.N1) THEN
                  IF (INFORM.GE.2) THEN
                     VMAX=RB1(M1)
                     VMIN=VMAX
                  ELSE
                     IMAX=IB1(M1)
                     IMIN=IMAX
                  ENDIF
               ENDIF
            ENDIF
C
C Pass along rows
C
            IF (INFORM.GE.2) THEN
               DO 100 I=M1,M2
                  A=RB1(I)
                  IF (A.GT.VMAX) THEN
                     VMAX=A
                  ELSE IF (A.LT.VMIN) THEN
                     VMIN=A
                  ENDIF
  100          CONTINUE
            ELSE
               DO 110 I=M1,M2
                  IA=IB1(I)
                  IF (IA.GT.IMAX) THEN
                     IMAX=IA
                  ELSE IF (IA.LT.IMIN) THEN
                     IMIN=IA
                  ENDIF
  110          CONTINUE
               VMAX=IMAX
               VMIN=IMIN
            ENDIF
C
            AVAIL=.TRUE.
  120    CONTINUE
  130 CONTINUE
C
C End of loop or error state: if abandon, and interactive, and estimates
C are available, treat error as soft
C
  140 IF (ERROR.NE.0) THEN
         IF (ERROR.NE.4 .OR. INPUT.NE.TERM1) GOTO 190
         IF (.NOT.AVAIL) GOTO 190
         GOTO 170
      ENDIF
C
C Fetch label for updating, if not tape
C
      IF (SUBREG) GOTO 170
  150 CONTINUE
      IF (SEMLAB(1,LABEL,LPN)) GOTO 190
C
C Delete or record range?
C
      IF (IOP.EQ.3) THEN
         LABEL(LBNCRR)=0
C
C ------ MONITOR ------
C
         IF (MONIT) THEN
            IF (SEMMON('!-RANGE: record deleted','RANGE',4)) GOTO 190
         ENDIF
C
C ------ ------- ------
C
      ELSE
C
C Encode range into label, unless protected
C
         IF (PROTN(DEVN(LPN)).NE.0) GOTO 170
         PTR=LBRR1
         IF (SEMXA1(4,LABEL,256,PTR,VMIN,I)) CONTINUE
         LABEL(PTR)=KCOMMA
         PTR=PTR+1
         IF (SEMXA1(4,LABEL,256,PTR,VMAX,I)) CONTINUE
         LABEL(LBNCRR)=PTR-LBRR1
C
C ------ MONITOR ------
C
         IF (MONIT) THEN
            WRITE (RECORD,160) VMIN,VMAX
            IF (SEMMON(RECORD,'RANGE',4)) GOTO 190
         ENDIF
C
  160    FORMAT ('!-RANGE: ',2G14.6,' recorded in label')
C
C ------ ------- ------
C
      ENDIF
C
C Return label to disc
C
      IF (SEMLAB(2,LABEL,LPN)) GOTO 190
C
C Clear WSTAT
C
      WSTAT(LPN)=0
C
C Normal return
C
  170 RANGE=.FALSE.
C
  180 RETURN
C
C Error returns
C
  190 RANGE=.TRUE.
      GOTO 180
C
C Copyright (C) 1987-1996: Synoptics Ltd,  All Rights Reserved
C
      END
