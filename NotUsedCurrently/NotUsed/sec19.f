      SUBROUTINE FIOP4
C
C  exercise timing function
C
C  SYNTAX FI4  :FIOP4
C
C
      LOGICAL TIMOPE,TIMCLS,TIMLOG
C
      INCLUDE 'COMMON'
C
C  create a large array
C
      INTEGER ROWSIZ
      PARAMETER (ROWSIZ=(2*LNEDGE+LNBUF/LNINT))
      INTEGER BIG(ROWSIZ,3)
      EQUIVALENCE(BIG(LNEDGE+1,1),RB1(1))
C
      INTEGER HANDLE,TEMP
C
C  open timing file
C
      IF(TIMOPE(HANDLE)) RETURN
C
C  time row access
C
      IF(TIMLOG(HANDLE,'Starting row access',19)) GOTO 10
      CALL ROWACC(BIG(1,1),ROWSIZ)
      IF(TIMLOG(HANDLE,'Finished row access',19)) GOTO 10
C
C  time column access
C
      IF(TIMLOG(HANDLE,'Starting column access',22)) GOTO 10
      CALL COLACC(BIG(1,1),ROWSIZ)
      IF(TIMLOG(HANDLE,'Finished column access',22)) GOTO 10
C
C  close log file
C
   10 TEMP=ERROR
      ERROR=0
      IF(TIMCLS(HANDLE)) THEN
         ERROR=TEMP
         RETURN
      ENDIF
C
      ERROR=TEMP
C
      RETURN
      END
C
C
      LOGICAL FUNCTION TIMOPE(HANDLE)
C
C  open timing log file
C
      INCLUDE 'COMMON'
C
      LOGICAL FILOPN,SEMCON
C
      INTEGER LENREC,HANDLE
      CHARACTER*80 PATH
C
      TIMOPE=.TRUE.
C
C  open log file
C
      LENREC=80
C  .. LENREC is ignored..
      IF(FILOPN('times.log',' ',1,2,1,LENREC,PATH,HANDLE)) RETURN
      WRITE(RECORD,10) PATH
   10 FORMAT('Logging timings to ',A)
      IF(SEMCON(RECORD)) RETURN
C
      TIMOPE=.FALSE.
C
      RETURN
      END
C
C
      LOGICAL FUNCTION TIMCLS(HANDLE)
C
C  close logging file
C
      LOGICAL FILCLS
      INCLUDE 'COMMON'
      INTEGER HANDLE
C
      TIMCLS=.TRUE.
C
      IF(FILCLS(HANDLE,.FALSE.)) RETURN
C
      TIMCLS=.FALSE.
C
      RETURN
      END
C
C
C
      LOGICAL FUNCTION TIMLOG(HANDLE,MESSAG,L)
C
C  Log current time, date, message to file
C
      INCLUDE 'COMMON'
C
      LOGICAL FILCIO
      CHARACTER*11 DATSTR
      CHARACTER*8 TIMSTR
C
      INTEGER HANDLE,IBUF(7),L
      CHARACTER*80 DATE
      CHARACTER*4 CSEC
      CHARACTER*(*) MESSAG
C
      TIMLOG=.TRUE.
C
C  get date/time
C
      CALL MCTIME(IBUF)
C
C  generate date/time in pretty format
C
      DATE(1:12)=DATSTR(IBUF(3),IBUF(2),IBUF(1))
      DATE(13:20)=TIMSTR(IBUF(4),IBUF(5),IBUF(6))
C
C  write centiseconds to character string DATE
C
      WRITE(CSEC,10) IBUF(7)
   10 FORMAT('::',I2)
      DATE(21:25)=CSEC
C
C  add message to string
C
      IF(26+L-1 .GT. 80) L=80-26+1
      DATE(26:26+L-1)=MESSAG(1:L)
      IF(FILCIO(HANDLE,2,DATE,26+L-1)) RETURN
C
      TIMLOG=.FALSE.
C
      RETURN
      END
C
C
      SUBROUTINE ROWACC(X,N)
C
C  access 1-D array
C
      INTEGER N,X(3*N),I,K
C
      DO 20 K=1,10
         DO 10 I=2,3*N
               X(I)=37
   10    CONTINUE
   20 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE COLACC(X,N)
C
C  access 2-D array along columns
C
      INTEGER N,X(3,N),I,J,K
C
      DO 30 K=1,10
         DO 20 J=1,3
            DO 10 I=2,N
               X(J,I)=37
   10       CONTINUE
   20    CONTINUE
   30 CONTINUE
      RETURN
      END
