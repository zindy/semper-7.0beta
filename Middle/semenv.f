C Semper 6 system module SEMENV
C
      LOGICAL FUNCTION SEMENV(DEVICE,ISLOT,ILINE)
C
C Sets command input line (and index if not up to date)
C LINLEN is reset to LINPTR if all goes well
C Returns TRUE if error or line number out of range
C
      INTEGER DEVICE,ISLOT,ILINE
      LOGICAL DISC,PRUIND
C
      INCLUDE 'COMMON'
C
      INTEGER*4 TXTBLK,LABBLK,BLKN,N4
      INTEGER SLTYPE,TXTLEN,LABLEN,NAMLEN
      INTEGER NAME(PRNACT)
      INTEGER LABENT,LABNUM,LABPTR,IDENT,OUTBLK,TXTPTR
      LOGICAL RESULT
C
      PARAMETER (LABENT=4)
C
      EQUIVALENCE (NAME,LINDEX)
C
      RESULT = .TRUE.
C
C If current device and slot not the same as the requested one
C then a reload is necessary
C
      IF (DEVICE .NE. LINDEV .OR. ISLOT .NE. LINSLT) THEN
C
C Reload from interactive or obey command
C
         IF (DEVICE .EQ. 0) THEN
            BLKN = WRKLIN + (INPLEV * LINSZE)
            N4 = LNLINB
            IF (DISC(1,WRKDEV,N4,LINBUF,BLKN,NFMINT,NFMBYT)) GOTO 20
            BLKN = WRKIND + (INPLEV * IDXSZE)
            N4 = LNINDX
            IF (DISC(1,WRKDEV,N4,LINDEX,BLKN,NFMINT,NFMINT)) GOTO 20
            LINDEV = 0
            LINSLT = 0
            LINNUM = ILINE
         ELSE
C
C Reload from program library
C
            IF (PRUIND(1,DEVICE,ISLOT,TXTBLK,LABBLK,SLTYPE,TXTLEN,
     +                   LABLEN,NAMLEN,NAME)) GOTO 20
C
            N4 = LABLEN
            N4 = N4*LNBLK4
            N4 = N4 / LNINT
C
            IF (DISC(1,DEVICE,N4,LINDEX,LABBLK,NFMINT,NFMINT)) GOTO 20
            LINTXT = TXTBLK
            LINDEV = DEVICE
            LINSLT = ISLOT
            LINNUM = -1
         ENDIF
      ENDIF
      IF (ILINE .NE. LINNUM .AND. DEVICE .NE. 0) THEN
C
C Current line is not the first one (program library only)
C
         LABNUM = 0
   10    LABPTR = LABNUM*LABENT
         LABNUM = LABNUM + 1
         IDENT = LINDEX(LABPTR+1)
         IF (IDENT .EQ. TIDLIN) THEN
            IF (ILINE .EQ. LINDEX(LABPTR+2)) THEN
               OUTBLK = LINDEX (LABPTR + 3)
               TXTPTR = LINDEX (LABPTR + 4)
               N4 = TXTPTR
               BLKN = LINTXT + OUTBLK
               IF (DISC(1,DEVICE,N4,LINBUF,BLKN,NFMINT,NFMBYT)) GOTO 20
               LINNUM = ILINE
               LINPTR = TXTPTR
               IDENT = TIDEND
               RESULT = .FALSE.
            ELSE
               GOTO 10
            ENDIF
         ENDIF
         IF (IDENT .NE. TIDEND) GOTO 10
      ELSE
         RESULT = .FALSE.
      ENDIF
C
   20 IF (.NOT.RESULT) THEN
C
C Set actual length and save line number if in LIBRARY
C
         LINLEN = LINPTR
         IF (INPDEV(INPLEV) .NE. 0) INPLIN(INPLEV) = LINNUM
      ENDIF
      SEMENV = RESULT
      RETURN
C
C Copyright (C) 1988,1989,1990:  Synoptics Ltd,  All Rights Reserved
C
      END
