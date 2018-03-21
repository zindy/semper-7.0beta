C Semper 6 system module SEMDEL
C
      LOGICAL FUNCTION SEMDEL(OPC,NUMBER)
      INTEGER OPC,NUMBER
C
C OPC=1  Deletes picture number NUMBER
C OPC=2  Deletes 'temporary' pictures (all those in the PCB table with
C        negative priority, and those in disc directories with neg
C        pic nums on devices for which the device table contains a
C        delete temporary request); clears REQDTS
C OPC=3  Writes a new empty disc directory to device NUMBER,
C        removing all trace of the original pictures
C OPC=4  As OPC=1, but deleting disc pictures even if label is malformed
C
C The function returns .TRUE. if anything went wrong (i.e. disc error,
C or request to delete a protected picture with OPC=1 or 2).  In all
C other cases it returns .FALSE.
C
C Only OPC=3 can delete pictures which are marked as write/delete
C protected.  Directory is condensed as necessary after any change
C (SEMOPN does NOT condense properly after implicit picture deletes)
C
      LOGICAL SEMOP2,SEMMED,SEMDPD,SEMCLS,DISC,SEMMON
C
      INTEGER*4 SDL(256),I42,LNDIR4
      PARAMETER (I42=2)
C
      INTEGER LABEL(256)
      INTEGER DEVICE,DIRSZE,I,L,M,MEDIUM,PNUM,SDPTR,SDPTR2
C
      INCLUDE 'COMMON'
C
      EQUIVALENCE (RB1,LABEL),(RB2,SDL)
C
C ------ MONITOR ------
C
      IF (MONIT) THEN
         WRITE (RECORD,10) OPC,NUMBER
         IF (SEMMON(RECORD,'SEMDEL',3)) GOTO 170
      ENDIF
C
   10 FORMAT ('!-SEMDEL: op',I2,' number',I5)
C
C ------ ------- ------
C
C Switch on action
C
      IF (OPC .EQ. 2) THEN
         GOTO 50
      ELSE IF (OPC .EQ. 3) THEN
         GOTO 90
      ENDIF
C
C Opcode 1:  Delete a single pic
C ------------------------------
C Check that pic number is valid
C
      IF (NUMBER.LE.1000) GOTO 140
      DEVICE=NUMBER/1000
      IF (SEMMED(DEVICE,MEDIUM)) GOTO 170
C
C Device protected?
C
      IF (PROTN(DEVICE).LT.0) GOTO 150
      PNUM = NUMBER-1000*DEVICE
      IF (PNUM.EQ.0) GOTO 130
C
C Switch on device
C
      IF (MEDIUM .EQ. MEDDC .OR. MEDIUM .EQ. MEDVM) THEN
C
C Disc pic
C --------
C Device not a picture device?
C
         IF (DVTYP(DEVICE).NE.FLTPIC) THEN
            ERROR=90
            IDERR=DEVICE
            GOTO 170
         ENDIF
C
         DIRSZE=DRSIZ(DEVICE)
         LNDIR4=DIRSZE*LNBLK
         IF (DISC(1,DEVICE,LNDIR4,SDL,I42,NFMBYT,NFMBYT)) GOTO 170
C
C Search directory for picture
C
         SDPTR=1
   20    L=SDL(SDPTR)
         IF (L.GE.1000) GOTO 130
         IF (L.NE.PNUM) THEN
            SDPTR=SDPTR+2
            GOTO 20
         ENDIF
C
C Get label and check whether malformed
C
         IF (DISC(1,DEVICE,LNLAB4,LABEL,SDL(SDPTR+1),NFMINT,NFMBYT))
     +       GOTO 170
C
C Fault malformation unless opcode 4
C
         IF (SEMOP2(LABEL)) THEN
            IF (OPC.NE.4) THEN
               ERROR=52
               GOTO 160
            ENDIF
         ELSE
C
C Check WP
C
            IF (LABEL(LBWP).NE.0) GOTO 150
         ENDIF
C
C OK: Delete pic
C
         SDL(SDPTR)=0
C
C Condense and re-write directory
C
         GOTO 100
C
C Close PCB(s) and exit
C
      ELSE IF (MEDIUM .EQ. MEDDS) THEN
C
C Display pic
C -----------
C
         IF (SEMDPD(1,PNUM)) GOTO 170
         IF (DPTYP.GT.0) THEN
            DPTYP=0
            IF (SEMDPD(2,PNUM)) GOTO 170
         ENDIF
      ENDIF
C
C Close any related PCB(s) and exit
C
   30 DO 40 I=1,NLPS
         IF (DEVN(I).EQ.DEVICE) THEN
            IF (PICN(I).EQ.PNUM) THEN
               IF (SEMCLS(I)) GOTO 170
            ENDIF
         ENDIF
   40 CONTINUE
      GOTO 130
C
C Opcode 2: Remove any temporary pictures from disc directory/PCB table
C --------------------------------------------------------------------
C
   50 REQDTS=.FALSE.
C
C Scan PCB table closing any temps found
C
      DO 60 I=1,NLPS
         IF (WHEN(I).LT.0) THEN
            IF (SEMCLS(I)) GOTO 170
         ENDIF
   60 CONTINUE
C
C Scan device table for discs with requests pending
C Starting from device 1 as device 0 is the work disc
C
      DEVICE=1
   70 IF (.NOT.(MEDN(DEVICE).EQ.MEDDC.OR.MEDN(DEVICE).EQ.MEDVM))
     +   GOTO 120
      IF (TPSID(DEVICE).EQ.0) GOTO 120
      TPSID(DEVICE)=0
C
C Fetch directory and delete temporaries
C
      DIRSZE=DRSIZ(DEVICE)
      LNDIR4=DIRSZE*LNBLK
      IF (DISC(1,DEVICE,LNDIR4,SDL,I42,NFMBYT,NFMBYT)) GOTO 170
      SDPTR=1
   80 L=SDL(SDPTR)
C
C Condense and re-write via internal routine call
C
      IF (L.GE.1000) GOTO 100
      IF (L.LT.0) SDL(SDPTR)=0
      SDPTR=SDPTR+2
      GOTO 80
C
C Opcode 3: Write a new empty disc directory (without reading old one)
C --------------------------------------------------------------------
C Check device number valid (and not work disc!)
C
   90 IF (NUMBER.LT.1 .OR. NUMBER.GT.NDVS) GOTO 140
      IF (.NOT.(MEDN(NUMBER).EQ.MEDDC.OR.MEDN(NUMBER).EQ.MEDVM)) THEN
         ERROR=29
         GOTO 160
      ENDIF
C
      DIRSZE=DRSIZ(NUMBER)
      LNDIR4=DIRSZE*LNBLK
C
C Construct empty directory:
C    Free slot starts after directory + header block
C    Space beyond the end of file is not allocatable !
C
      SDL(1)=0
      SDL(2)=DIRSZE+2
      SDL(3)=1000
      SDL(4)=FLSIZ(NUMBER)+1
      IF (DISC(2,NUMBER,LNDIR4,SDL,I42,NFMBYT,NFMBYT)) GOTO 170
      GOTO 130
C
C Routine to condense directory and re-write to DEVICE
C
  100 SDPTR=3
      SDPTR2=3
  110 M = SDL(SDPTR)
      IF (M.NE.0 .OR. SDL(SDPTR-2).NE.0) THEN
         IF (SDPTR .NE. SDPTR2) THEN
            SDL(SDPTR2) = M
            SDL(SDPTR2+1) = SDL(SDPTR+1)
         ENDIF
         SDPTR2 = SDPTR2+2
      ENDIF
      SDPTR = SDPTR + 2
      IF (M.LT.1000) GOTO 110
C
C Rewrite the disc directory
C
      IF (DISC(2,DEVICE,LNDIR4,SDL,I42,NFMBYT,NFMBYT)) GOTO 170
C
C Routine return
C
      IF (OPC.EQ.1) GOTO 30
C
C End of loop over devices
C
  120 DEVICE=DEVICE+1
      IF (DEVICE.LE.NDVS) GOTO 70
C
C Normal return
C
  130 SEMDEL=.FALSE.
      GOTO 180
C
C Error returns
C
  140 ERROR=28
      GOTO 160
C
  150 ERROR=41
  160 IDERR=NUMBER
C
  170 SEMDEL=.TRUE.
  180 RETURN
C
C Copyright (C) 1987-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
