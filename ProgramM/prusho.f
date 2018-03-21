C Semper 6 system module PRUSHO
C
      LOGICAL FUNCTION PRUSHO(LNAME,DEVICE,IDSLOT,DEFIND)
C
C Writes the contents of program IDSLOT on device DEVICE to
C the file on Fortran unit RDWRTU if LNAME is .TRUE., and to the
C console output stream otherwise.  The default indent is DEFIND.
C
      LOGICAL LNAME
      INTEGER DEVICE,IDSLOT,DEFIND
C
      LOGICAL PRUIND,PRUCAS,DISC,ABANDN,SEMCON,SEMIOE,SEMTPS
C
      INCLUDE 'COMMON'
C
      INTEGER*4 BLKN,N4,TXTBLK,LABBLK
      INTEGER SLTYPE,TXTLEN,LABLEN,NAMLEN,IDLABT,IDLPTR,INDENT
      INTEGER PTR,PTRLIM,PTRNXT,TXTPTR,OUTBLK,I,J,IWID,ILEN
      INTEGER CHKFOR,ICOUNT,IOSIOE,IWIDTH,LABPTR
      INTEGER IOS
      LOGICAL INQUOT
C
C Text line buffer
C
      INTEGER PRTEXT(LNLINB)
C
C Program index area and size of an index entry
C Current label entry
C
      INTEGER LABINF(LNINDX),LABENT,LABNUM
      PARAMETER (LABENT=4)
C
      INTEGER NAME(PRNACT)
      CHARACTER*(PRNACT) PROG
      CHARACTER*60 FNM
C
      EQUIVALENCE (PRTEXT,RB2),(LABINF,RB5)
C
      PRUSHO = .TRUE.
C
C Width for output
C
      IF (LNAME) THEN
         IWIDTH = 70
      ELSE
         IF (SEMTPS(IWID,ILEN)) GOTO 90
C
         IWIDTH = IWID - 2
      ENDIF
C
      CHKFOR = 0
      LABNUM = 0
C
      IF (PRUIND(1,DEVICE,IDSLOT,TXTBLK,LABBLK,
     +             SLTYPE,TXTLEN,LABLEN,NAMLEN,NAME)) GOTO 90
C
      IF (PRUCAS(NAME,NAMLEN)) GOTO 90
      IF (NAME(1) .GE. KLCA .AND. NAME(1) .LE. KLCZ)
     +    NAME(1) = NAME(1) - KLCA + KUCA
C
      CALL SEMCHS(PROG,NAME,NAMLEN)
C
      RECORD(1:NAMLEN)=PROG(1:NAMLEN)
      RECORD(NAMLEN+1:NAMLEN+2)='()'
C
      IF (LNAME) THEN
         WRITE (RDWRTU,'(A)',ERR=100,IOSTAT=IOS) ' '
         WRITE (RDWRTU,'(A)',ERR=100,IOSTAT=IOS) RECORD(1:NAMLEN+2)
      ELSE
         IF (SEMCON(' ')) GOTO 90
         IF (SEMCON(RECORD(1:NAMLEN+2))) GOTO 90
      ENDIF
C
      N4 = LABLEN
      N4 = N4*LNBLK4
      N4 = N4 / LNINT
C
C Read program index
C
      IF (DISC(1,DEVICE,N4,LABINF,LABBLK,NFMINT,NFMINT)) GOTO 90
C
C Read index for next line entry
C
   10 LABPTR = LABNUM*LABENT
      LABNUM = LABNUM + 1
      IDLABT = LABINF(LABPTR + 1)
      IF (IDLABT .NE. TIDEND) THEN
         IF (IDLABT .NE. TIDLIN) THEN
            ERROR = 128
            IDMESS = PROG(1:NAMLEN)
            GOTO 90
         ENDIF
         OUTBLK = LABINF (LABPTR + 3)
         TXTPTR = LABINF (LABPTR + 4)
         N4 = TXTPTR
         BLKN = TXTBLK + OUTBLK
         IF (DISC(1,DEVICE,N4,PRTEXT,BLKN,NFMINT,NFMBYT)) GOTO 90
C
C Clear first element in case an empty program
C
         IF (TXTPTR .EQ. 0) PRTEXT(1) = KSPACE
C
C Text read - now output line, scanning index for FOR or LABEL entries
C
         PTR = 1
         LABPTR = LABNUM * LABENT
         IDLABT = LABINF (LABPTR + 1)
         IDLPTR = LABINF (LABPTR + 3)
C
C Scan for limit of command
C
   20    INQUOT = .FALSE.
         PTRLIM = PTR
   30    IF (PTRLIM .LT. TXTPTR .AND. PRTEXT(PTRLIM) .NE. KSEMIC) THEN
C
C Check for strings
C
   40       IF (PRTEXT(PTRLIM) .EQ. KQUOTE) INQUOT = .NOT. INQUOT
            PTRLIM = PTRLIM + 1
            IF (INQUOT .AND. PTRLIM .LT. TXTPTR) GOTO 40
            GOTO 30
         ENDIF
         PTRNXT = PTRLIM + 1
C
C Don't include semi-colon at end of command
C
         IF (PRTEXT(PTRLIM) .EQ. KSEMIC .AND.
     +                      .NOT. INQUOT) PTRLIM = PTRLIM - 1
         INDENT = DEFIND + (CHKFOR * 2)
C
C Now see if PTR represents a LABEL, FOR or LOOP
C
   50    IF (PTR .EQ. IDLPTR) THEN
            IF (IDLABT .EQ. TIDLAB) THEN
               INDENT = DEFIND
               IDLPTR = -1
            ELSE IF (IDLABT .EQ. TIDFOR) THEN
               CHKFOR = CHKFOR + 1
               IDLPTR = -1
            ELSE IF (IDLABT .EQ. TIDLOO) THEN
               CHKFOR = CHKFOR - 1
               INDENT = DEFIND + (CHKFOR * 2)
               IDLPTR = -1
            ENDIF
            IF (IDLPTR .EQ. -1) THEN
C
C Item was taken - advance to next item and repeat
C
               LABNUM = LABNUM + 1
               LABPTR = LABNUM * LABENT
               IDLABT = LABINF (LABPTR + 1)
               IDLPTR = LABINF (LABPTR + 3)
               GOTO 50
            ENDIF
         ENDIF
C
C Output PRTEXT(PTR:PTRLIM) with indent INDENT
C
         IF (INDENT .LT. DEFIND) INDENT = DEFIND
   60    ICOUNT = INDENT + PTRLIM - PTR + 1
         IF (ICOUNT .GE. IWIDTH) THEN
C
C Scan backwards for convenient break point
C
            INQUOT = .FALSE.
            I = PTRLIM
C
C Establish save point in case line doesn't fit
C
            J = PTR + IWIDTH - 2
   70       IF (I .GT. PTR .AND. PRTEXT(I) .NE. KSPACE) THEN
   80           IF (PRTEXT(I) .EQ. KQUOTE) THEN
                   INQUOT = .NOT. INQUOT
                   IF (.NOT. INQUOT .AND. I .LT. J) J = I
                ENDIF
                I = I - 1
                IF (INQUOT .AND. I .NE. PTR) GOTO 80
                GOTO 70
            ENDIF
            IF (I .EQ. PTR) I = J
            ICOUNT = INDENT + I - PTR + 1
            IF (ICOUNT .GE. IWIDTH) THEN
               IF (I .GT. PTR) THEN
                  I = I - 1
                  GOTO 70
               ENDIF
               IF (INDENT .GT. DEFIND) THEN
                  INDENT = DEFIND
                  GOTO 60
               ENDIF
               IF (INDENT .GT. 0) THEN
                  INDENT = 0
                  GOTO 60
               ENDIF
               IDMESS = 'Program line too long to output'
               ERROR = 77
               GOTO 90
            ENDIF
         ELSE
            I = PTRLIM
         ENDIF
C
         RECORD = ' '
C
         J = I - PTR + 1
         IF (J .LE. 0) THEN
C
C Null command (blank line)
C
            J = 1
         ELSE
            CALL SEMCHS(RECORD(INDENT+1:),PRTEXT(PTR),J)
            J = INDENT + J
         ENDIF
C
C If partial line add terminal continuation marker
C
         IF (I .NE. PTRLIM) THEN
            J = J + 1
            RECORD(J:J) = '+'
         ENDIF
C
         PTR = I + 1
C
         IF (LNAME) THEN
            WRITE(RDWRTU,'(A)',ERR=100,IOSTAT=IOS) RECORD(1:J)
         ELSE
            IF (SEMCON(RECORD(1:J))) GOTO 90
         ENDIF
C
         IF (PTR .LT. PTRLIM) GOTO 60
C
C Any more of this line to output ? Check for user abandon first
C
         IF (ABANDN(ERROR)) GOTO 90
         PTR = PTRNXT
         IF (PTR .LT. TXTPTR) GOTO 20
         GOTO 10
      ENDIF
C
      PRUSHO = .FALSE.
C
   90 RETURN
C
  100 INQUIRE(RDWRTU,NAME=FNM)
      IOSIOE = IOS
      IF (SEMIOE(IOSIOE,RDWRTU,FNM)) GOTO 90
      GOTO 90
C
C Copyright (C) 1987-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
