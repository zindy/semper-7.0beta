C Semper 6 system module SEMFVD
C
      LOGICAL FUNCTION SEMFVD(SYN,NAME,MODULE,NOSYN)
C
C Returns TRUE if command NAME is a known processing command, decoding
C the command descriptor into simple lists for use by SEMINX and SEMSYN.
C If the command is found, the module number is returned in MODULE.
C If the $ option is specified, the flag NOSYN is set to .TRUE. to
C disable syntax checking in the command interpreter and to set the
C $ key to point to the first item after the command name.
C
      INTEGER SYN(*),NAME,MODULE
      LOGICAL NOSYN
C
      INCLUDE 'COMMON'
C
      INTEGER I,IQUOT,J,K,L
      INTEGER LASTKY,LASTVB,LSTKEY,LSTOPT,N,NOPENR,NVBPTR,VBPTR
C
C Packed names
C
      INTEGER NDOLLR,NBYTE,NINTEG,NFP,NCOMPL,NERASE,NRE,NIM,NVIEW
      INTEGER NMARK,NMKMOD,NMKSIZ
      PARAMETER (NDOLLR=-11201)
      PARAMETER (NBYTE=4220,NINTEG=14980,NFP=10240,NCOMPL=5413)
      PARAMETER (NERASE=8721,NRE=29000,NIM=14920,NVIEW=-3566)
      PARAMETER (NMARK=20858,NMKMOD=21253,NMKSIZ=21259)
C
      INTEGER OPTION(MAXOPT),KEY(MAXKEY),DEF(MAXKEY),OPENR(1+5*MAXOPE)
C
      EQUIVALENCE (RB4(1),OPTION,LSTOPT)
      EQUIVALENCE (RB4(1+MAXOPT),KEY,LSTKEY)
      EQUIVALENCE (RB4(1+MAXOPT+MAXKEY),DEF)
      EQUIVALENCE (RB4(1+MAXOPT+2*MAXKEY),OPENR,NOPENR)
C
      LASTVB = SYN(1)
      LASTKY = SYN(2)
      VBPTR = 5
C
C Search processing command list
C
   10 NVBPTR = SYN(VBPTR)
C
C Double test to avoid possible overflow for 16 bit integers
C
      IF (NVBPTR .LE. 0) THEN
         IF (NVBPTR .LT. -20000) THEN
C
            VBPTR = NVBPTR + 25000
            IF (VBPTR .NE. 0) GOTO 10
C
C Command not found
C
            ERROR = 14
            GOTO 60
         ENDIF
      ENDIF
C
      IF (NVBPTR .NE. NAME) THEN
         VBPTR = VBPTR+1
         GOTO 10
      ENDIF
C
C Command found
C
      SEMFVD=.FALSE.
      NOSYN=.FALSE.
C
C Decode command descriptor - general options and keys first
C
      OPTION(2)=NERASE
      OPTION(3)=NBYTE
      OPTION(4)=NINTEG
      OPTION(5)=NFP
      OPTION(6)=NCOMPL
      OPTION(7)=NRE
      OPTION(8)=NIM
      OPTION(9)=NVIEW
C
C Parameters NGOPTS,NGKEYS to be updated if any alterations are
C made to the general opt/key lists
C
      LSTOPT=NGOPTS+1
      LSTKEY=NGKEYS+1
      NOPENR=0
      MODULE=0
C
C Now peculiar items
C
      K=0
      N=0
C
C Skip any synonyms and get pointer to end of command descriptor
C
   20 VBPTR=VBPTR+1
      NVBPTR=SYN(VBPTR)
C
C Double test again
C
      IF (NVBPTR.GT.0) GOTO 20
      IF (NVBPTR.GT.-20000) GOTO 20
      NVBPTR=NVBPTR+25000
C
C End of this command descriptor?
C
   30 VBPTR=VBPTR+1
      IF (VBPTR.GE.NVBPTR) GOTO 70
C
C Pick up next item
C
      I=SYN(VBPTR)
C
C Double test again
C
      IF (I.LE.0) THEN
         IF (I.LT.-20000) GOTO 40
      ENDIF
C
C Option
C
      LSTOPT=LSTOPT+1
C
C Check for too many options
C
      IF (LSTOPT.GT.MAXOPT) THEN
         ERROR=85
         GOTO 60
      ENDIF
      OPTION(LSTOPT)=I
C
C Check for $ option
C
      IF (I.EQ.NDOLLR) NOSYN=.TRUE.
      GOTO 30
C
C Pointer
C
   40 I=I+25000
      IF (I .LT. 0) THEN
C
C Routine call
C
         MODULE=-I
      ELSE IF (I .EQ. 0) THEN
C
C Return from continuation
C
         VBPTR=K
         NVBPTR=N
      ELSE IF (I.LE.LASTVB) THEN
C
C Continuation command: save link
C
         K=VBPTR
         N=NVBPTR
         VBPTR=I+1
         NVBPTR=SYN(VBPTR)+25000
      ELSE IF (I.LE.LASTKY) THEN
C
C Key
C
         LSTKEY=LSTKEY+1
C
C Check for too many keys
C
         IF (LSTKEY.GT.MAXKEY) THEN
            ERROR=84
            GOTO 60
         ENDIF
         KEY(LSTKEY)=SYN(I)
C
C Assume no default
C
         DEF(LSTKEY)=0
         IF (I .LT. LASTKY) THEN
            J=SYN(I+1)
C
C Double test again
C
            IF (J .LT. 0) THEN
               IF (J .LE. -20000) THEN
C
C Set default pointer
C
                  DEF(LSTKEY)=J+25000
               ENDIF
            ENDIF
         ENDIF
      ELSE
C
C Open specification
C
         NOPENR=NOPENR+1
C
C Check for too many open specifications
C
         IF (NOPENR.GT.MAXOPE) THEN
            ERROR=86
            GOTO 60
         ENDIF
         J=NOPENR+(1+4*MAXOPE)
         OPENR(J)=SYN(I+1)
         I=SYN(I)
         DO 50 L=1,4
            J=J-MAXOPE
            IQUOT=I/10
            OPENR(J)=I-IQUOT*10
            I=IQUOT
   50    CONTINUE
      ENDIF
      GOTO 30
C
C Error return
C
   60 IDERR=NAME
      SEMFVD=.TRUE.
   70 RETURN
C
C Copyright (C) 1987-1990:  Synoptics Ltd,  All Rights Reserved
C
      END
