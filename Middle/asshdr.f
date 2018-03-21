C Semper 6 subsidiary processing module ASSHDR
C
      LOGICAL FUNCTION ASSHDR(IOP,DEVICE)
C
      INTEGER IOP,DEVICE
C
C Reads (IOP = 1) or writes (IOP = 2) the disc/memory header structure
C Also creates an empty directory (IOP = 2)
C
      LOGICAL DISC,SEMDEL,PRUTIN
C
      LOGICAL   LDEMO
      INTEGER   I,FLTYPE,DRSIZE
!      INTEGER*2 SUM1,SUM2,SUMCHECK
      INTEGER   SUM1,SUM2,SUMCHECK
      INTEGER*4 I4,N1,N2,N3,N4,FLSIZE
C
      CHARACTER*11 TAG
C
      INCLUDE 'COMMON'
C
      INTEGER HEADER(LNBLK),BUFFER(LNBLK)
C
      INTEGER*4 I40,I41,I42
      PARAMETER ( I40 = 0, I41 = 1, I42 = 2 )
C
      ASSHDR=.TRUE.
C
C IOP = 1, read and verify existing header
C
      IF (IOP.EQ.1) THEN
C
C Fudge file size parameter to ensure that call to DISC works o.k.
C
         FLSIZ(DEVICE)=1
C
C Read header structure
C
         IF (DISC(1,DEVICE,LNBLK4,HEADER,I41,NFMINT,NFMBYT)) GOTO 50
C
C Convert tag string into character variable form
C
         CALL SEMCHS(TAG,HEADER,11)
C
C First 7 characters => normal/demo tag
C
         IF (TAG(1:7).EQ.'Semper.') THEN
            LDEMO=.FALSE.
         ELSE IF (TAG(1:7).EQ.'semper,') THEN
            LDEMO=.TRUE.
         ELSE
            GOTO 60
         ENDIF
C
C Last 4 characters => picture/program/help tag, giving device type
C
         IF (TAG(8:11).EQ.'disc') THEN
            FLTYPE=FLTPIC
         ELSE IF (TAG(8:11).EQ.'text') THEN
            FLTYPE=FLTRUN
         ELSE IF (TAG(8:11).EQ.'help') THEN
            FLTYPE=FLTHEL
         ELSE
            GOTO 60
         ENDIF
C
C Fault device type if not required type
C
         IF (FLTYPE.NE.DVTYP(DEVICE)) THEN
            ERROR=119
            GOTO 50
         ENDIF
C
C Fault incorrect version
C
         IF (HEADER(12).NE.6.OR.HEADER(13).NE.1) GOTO 60
C
C Decode device size
C
         N1=HEADER(14)
         N2=HEADER(15)
         N3=HEADER(16)
C
         FLSIZE=256*(256*N1+N2)+N3
C
C Decode directory size
C
         DRSIZE=256*HEADER(17)+HEADER(18)
C
C Record sizes in device table
C
         FLSIZ(DEVICE)=FLSIZE
         DRSIZ(DEVICE)=DRSIZE
C
C IOP = 2, write header and create empty directory
C
      ELSE IF (IOP.EQ.2) THEN
C
C Encode tag string for header
C
C First 7 characters => normal/demo tag
C
         TAG(1:7)='Semper.'
C
C Last 4 characters => picture/program/help tag
C
         FLTYPE=DVTYP(DEVICE)
C
         IF (FLTYPE.EQ.FLTPIC) THEN
            TAG(8:11)='disc'
         ELSE IF (FLTYPE.EQ.FLTRUN) THEN
            TAG(8:11)='text'
         ELSE IF (FLTYPE.EQ.FLTHEL) THEN
            TAG(8:11)='help'
         ENDIF
C
C Convert tag string into integer array form
C
         CALL SEMICS(TAG,HEADER,11)
C
C Encode version numbers
C
         HEADER(12)=6
         HEADER(13)=1
C
C Encode device size
C
         FLSIZE=FLSIZ(DEVICE)
C
         N1=FLSIZE/256/256
         N2=(FLSIZE-256*256*N1)/256
         N3=FLSIZE-256*(256*N1+N2)
C
         HEADER(14)=N1
         HEADER(15)=N2
         HEADER(16)=N3
C
C Encode directory size
C
         DRSIZE=DRSIZ(DEVICE)
C
         HEADER(17)=DRSIZE/256
         HEADER(18)=DRSIZE-256*HEADER(17)
C
C Encode checksum
C
         SUM1=HEADER(17)+HEADER(18)+90
         SUM2=HEADER(15)+HEADER(16)+165
C
C   LDM Change to get compatible types
         SUMCHECK = 255
         HEADER(19)=IAND(SUM1,SUMCHECK)
         HEADER(20)=IAND(SUM2,SUMCHECK)
C
C Encode date and time
C
         CALL MCTIME(HEADER(21))
C
         HEADER(21)=HEADER(21)-1900
C
C Fill rest of header block with zeros
C
         DO 10 I=28,LNBLK
            HEADER(I)=0
   10    CONTINUE
C
C Write header structure
C
         IF (DISC(2,DEVICE,LNBLK4,HEADER,I41,NFMINT,NFMBYT)) GOTO 50
C
C Create an empty directory
C
         IF (FLTYPE.EQ.FLTPIC) THEN
            IF (SEMDEL(3,DEVICE)) GOTO 50
         ELSE IF (FLTYPE.EQ.FLTRUN) THEN
            IF (PRUTIN(DEVICE)) GOTO 50
         ELSE IF (FLTYPE.EQ.FLTHEL) THEN
            N4=DRSIZE+2
C
            N1=N4/256/256
            N2=(N4-256*256*N1)/256
            N3=N4-256*(256*N1+N2)
C
            BUFFER(1)=N1
            BUFFER(2)=N2
            BUFFER(3)=N3
C
            DO 20 I=4,LNBLK
               BUFFER(I)=0
   20       CONTINUE
C
            IF (DISC(2,DEVICE,LNBLK4,BUFFER,I42,NFMINT,NFMBYT)) GOTO 50
C
            DO 30 I=1,LNBLK
               BUFFER(I)=0
   30       CONTINUE
C
            CALL CFORM(BUFFER,BUFFER,NFMINT,NFMBYT,LNBLK4)
C
            DO 40 I4=N4,FLSIZE
               IF (DISC(2,DEVICE,LNBLK4,BUFFER,I4,NFMBYT,NFMBYT))
     +            GOTO 50
   40       CONTINUE
C
            IF (DISC(3,DEVICE,I40,BUFFER,I40,0,0)) GOTO 50
         ENDIF
      ENDIF
C
      ASSHDR=.FALSE.
C
   50 RETURN
C
C Fault bad header
C
   60 ERROR=77
      IDMESS='Invalid disc file header'
      GOTO 50
C
C Copyright (C) 1993:  Synoptics Ltd,  All Rights Reserved
C
      END
