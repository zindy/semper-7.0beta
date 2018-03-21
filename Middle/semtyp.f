C Semper 6 system module SEMTYP
C
      LOGICAL FUNCTION SEMTYP(IN,LENIN,PTR,OUT,LENOUT,SCANMD)
C
C Evaluates TYPE argument string in IN, placing resulting text in
C OUT(LENOUT); LENOUT returned reduced to significant length
C
      INTEGER IN(*),LENIN,PTR,OUT(*),LENOUT
      LOGICAL SCANMD
C
      LOGICAL SEMEXP,SEMXA1
C
      INCLUDE 'COMMON'
C
      REAL X
      INTEGER CH,MAXOUT,OUTPTR
      LOGICAL COMEXP,QPEND,EXPPR
C
      SEMTYP=.FALSE.
C
C COMEXP => comma expected next
C
      COMEXP=.FALSE.
C
C EXPPR => preceding item was an expression
C
      EXPPR=.FALSE.
      MAXOUT=0
      OUTPTR=1
      IF (PTR.GT.LENIN) GOTO 120
C
C Blank-fill OUT in case of tabs
C
      DO 10 CH=1,LENOUT
         OUT(CH)=KSPACE
   10 CONTINUE
C
C Fetch next character
C
   20 IF (SEMXA1(0,IN,LENIN,PTR,X,CH)) GOTO 90
      IF (COMEXP) THEN
         IF (CH.NE.KCOMMA) GOTO 120
         COMEXP=.FALSE.
         PTR=PTR+1
         GOTO 20
      ENDIF
C
C Switch code according to tab / text / expression
C
      IF (CH.EQ.KHASH) GOTO 80
      IF (CH.EQ.KQUOTE) GOTO 50
C
C Evaluate expression
C
      COMEXP=.TRUE.
      IF (SEMEXP(IN,LENIN,PTR,X,SCANMD)) GOTO 110
      IF (.NOT.EXPPR) GOTO 40
      CH=KSPACE
C
C Deposit CH
C
   30 IF (OUTPTR.LE.LENOUT) THEN
         OUT(OUTPTR)=CH
         IF (OUTPTR.GT.MAXOUT) MAXOUT=OUTPTR
         OUTPTR=OUTPTR+1
      ENDIF
      IF (.NOT.COMEXP) GOTO 50
C
C Write expression value
C
   40 QPEND=SEMXA1(4,OUT,LENOUT,OUTPTR,X,CH)
      EXPPR=.TRUE.
      IF (OUTPTR.GT.MAXOUT) MAXOUT=OUTPTR-1
      GOTO 20
C
C Process text string
C
   50 QPEND=.FALSE.
C
   60 PTR=PTR+1
      IF (PTR.GT.LENIN) THEN
         IF (QPEND) GOTO 120
         GOTO 100
      ENDIF
C
      CH=IN(PTR)
      IF (CH.EQ.KQUOTE) THEN
C
C Quote found - output if double, note if single
C
         IF (QPEND) GOTO 30
         QPEND=.TRUE.
         GOTO 60
      ENDIF
C
      IF (.NOT.QPEND) GOTO 30
C
   70 COMEXP=.TRUE.
      EXPPR=.FALSE.
      GOTO 20
C
C Process tab
C
   80 PTR=PTR+1
      IF (PTR.GT.LENIN) GOTO 100
      IF (SEMEXP(IN,LENIN,PTR,X,SCANMD)) GOTO 110
      CH=NINT(X)
      IF (CH.LE.0 .OR. CH.GT.LENOUT) THEN
         ERROR=31
         GOTO 110
      ENDIF
      OUTPTR=CH
      GOTO 70
C
C Source exhausted
C
   90 IF (COMEXP) GOTO 120
C
C Errors
C
  100 ERROR=17
  110 SEMTYP=.TRUE.
C
  120 LENOUT=MAXOUT
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
