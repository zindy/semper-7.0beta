C Semper 6 system module SEMSYN
C
      SUBROUTINE SEMSYN(VD)
C
C Manages command SYNTAX, providing information directly on command
C descriptors. All this is obtained from the command descriptor array VD
C
      INTEGER VD(*)
C
      LOGICAL SEMFVD,SEMXA1,SEMSY2,SEMCON,SEMTPS
      INTEGER IVAL
C     Change: LDM, July 2005
C     Ensure that gfortran does not think of range, unpack as intrinsics
      EXTERNAL RANGE, UNPACKF
      CHARACTER*3 UNPACKF
C
      INTEGER I,M,N,IPTR,NAME,MODULE,IWID,ILEN
      LOGICAL NOSYN
      REAL X
      CHARACTER*9 DIGITS
      CHARACTER*70 STRING
C
C Packed names
C
      INTEGER NDOLLR
      PARAMETER (NDOLLR=-11201)
C
      INCLUDE 'COMMON'
C
      INTEGER OPTION(MAXOPT),KEY(MAXKEY),DEF(MAXKEY),OPENR(1+5*MAXOPE)
C
      INTEGER LSTOPT,LSTKEY,NOPENR
C
      EQUIVALENCE (RB4(1),OPTION,LSTOPT)
      EQUIVALENCE (RB4(1+MAXOPT),KEY,LSTKEY)
      EQUIVALENCE (RB4(1+MAXOPT+MAXKEY),DEF)
      EQUIVALENCE (RB4(1+MAXOPT+2*MAXKEY),OPENR,NOPENR)
C
      DATA DIGITS / '123456789' /
C
C Return if no verb name specified
C
      IPTR=IVAL(NDOLLR)
      IF (IPTR.EQ.0) GOTO 40
C
C Read verb name
C
      IF (SEMXA1(1,LINBUF,COMLIM,IPTR,X,NAME)) GOTO 40
C
C Fault item that is not a valid name
C
      IF (NAME.EQ.0) THEN
         ERROR=17
         GOTO 40
      ENDIF
C
C Fetch current terminal page size
C
      IF (SEMTPS(IWID,ILEN)) GOTO 40
C
C Decode verb descriptor
C
      IF (SEMFVD(VD,NAME,MODULE,NOSYN)) GOTO 40
C
C See whether this command has any syntax defined for it
C
      IF ((LSTOPT.LT.NGOPTS+2.AND.LSTKEY.LT.NGKEYS+2.AND.NOPENR.EQ.0)
     +    .OR.NOSYN) THEN
         IF (SEMCON('No syntax available for this command')) GOTO 40
         GOTO 40
      ENDIF
C
      IF (SEMCON(' ')) GOTO 40
C
      STRING=' '
C
C Print options
C
      IF (LSTOPT.GE.NGOPTS+2) THEN
         IF (SEMCON('Options:')) GOTO 40
C
         RECORD=' '
         N=1
C
C Encode each option in turn
C
         DO 10 I=NGOPTS+2,LSTOPT
            STRING(3:5)=UNPACKF(OPTION(I))
            IF (SEMSY2(RECORD(1:IWID),N,STRING(1:5))) GOTO 40
   10    CONTINUE
C
C Flush any left-over text
C
         IF (N.GT.1) THEN
            IF (SEMCON(RECORD(1:N))) GOTO 40
         ENDIF
      ENDIF
C
C Print keys (with defaults)
C
      IF (LSTKEY.GE.NGKEYS+2) THEN
         IF (SEMCON('Keys:')) GOTO 40
C
         RECORD=' '
         N=1
C
C Encode each key (with default) in turn
C
         DO 20 I=NGKEYS+2,LSTKEY
            STRING(3:5)=UNPACKF(KEY(I))
            STRING(6:6)=' '
            M=INDEX(STRING(3:),' ')+1
C
            IPTR=DEF(I)
            IF (IPTR.NE.0) THEN
               M=M+1
               STRING(M:M)='='
C
               CALL SEMCHS(STRING(M+1:),VD(IPTR+1),VD(IPTR))
               M=MIN(M+VD(IPTR),LEN(STRING))
            ENDIF
C
C Round up length of output string to multiple of 5 and buffer it
C
            M=5*(1+(M-1)/5)
            IF (SEMSY2(RECORD(1:IWID),N,STRING(1:M))) GOTO 40
   20    CONTINUE
C
C Flush any left-over text
C
         IF (N.GT.1) THEN
            IF (SEMCON(RECORD(1:N))) GOTO 40
         ENDIF
      ENDIF
C
C Print open definitions
C
      IF (NOPENR.GE.1) THEN
         IF (SEMCON('Picture opens:')) GOTO 40
C
         RECORD=' '
         N=1
C
C Encode each open request in turn
C
         DO 30 I=1,NOPENR
            STRING(3:3)='('
C
            STRING(4:5)='lp'
C
            M=OPENR(1+I)
            STRING(6:6)=DIGITS(M:M)
C
            STRING(7:7)=','
C
            IF (OPENR((1+MAXOPE)+I).EQ.1) THEN
               STRING(8:10)='old'
            ELSE
               STRING(8:10)='new'
            ENDIF
C
            M=OPENR((1+2*MAXOPE)+I)
            IF (M.EQ.0) THEN
               M=10
            ELSE
               STRING(11:11)=','
               STRING(12:13)='lp'
               STRING(14:14)=DIGITS(M:M)
               M=14
            ENDIF
C
            M=M+1
            STRING(M:M)=')'
C
            IPTR=OPENR((1+4*MAXOPE)+I)
C
            M=M+1
            STRING(M:M)='='
C
            CALL SEMCHS(STRING(M+1:),VD(IPTR+1),VD(IPTR))
            M=MIN(M+VD(IPTR),LEN(STRING))
C
C Round up length of output string to multiple of 5 and buffer it
C
            M=5*(1+(M-1)/5)
            IF (SEMSY2(RECORD(1:IWID),N,STRING(1:M))) GOTO 40
   30    CONTINUE
C
C Flush any left-over text
C
         IF (N.GT.1) THEN
            IF (SEMCON(RECORD(1:N))) GOTO 40
         ENDIF
      ENDIF
C
      IF (SEMCON(' ')) GOTO 40
C
   40 RETURN
C
C Copyright (C) 1987,1988,1989,1990:  Synoptics Ltd, All Rights Reserved
C
      END
C Semper 6 system module SEMSY2
C
      LOGICAL FUNCTION SEMSY2(REC,N,STRING)
C
C Add text in STRING to REC from position N, first printing out the
C contents of REC if the text to be added would overflow.
C
      CHARACTER*(*) REC,STRING
      INTEGER N
C
      LOGICAL SEMCON
C
      INCLUDE 'COMMON'
C
      SEMSY2=.TRUE.
C
C If too many characters, print out text in REC and clear it
C
      IF (N+LEN(STRING).GT.LEN(REC)) THEN
         IF (SEMCON(REC(1:N))) GOTO 10
         REC=' '
         N=1
      ENDIF
C
C Append text in STRING to REC
C
      REC(N+1:N+LEN(STRING))=STRING
C
C Update character count N
C
      N=N+LEN(STRING)
C
      SEMSY2=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1987,1988,1989,1990:  Synoptics Ltd, All Rights Reserved
C
      END
