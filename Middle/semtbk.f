C Semper 6 system module SEMTBK
C
      LOGICAL FUNCTION SEMTBK()
C
C Prints out program traceback to diagnostic output stream
C
      LOGICAL PRUIND,SEMDIA,SEMUNW
      INTEGER I,N
      INTEGER*4 I4N
C
      INCLUDE 'COMMON'
C
      INTEGER PRNAME(PRNACT)
C
      SEMTBK=.TRUE.
C
C Check for command execution within program
C
   10 IF (INPLEV .GT. 0) THEN
         IF (INPDEV(INPLEV) .NE. 0) THEN
C
C Fetch program name
C
            IF (PRUIND(1,INPDEV(INPLEV),INPSLT(INPLEV),
     +                   I4N,I4N,I,I,I,N,PRNAME)) GOTO 20
C
C Construct and print traceback string
C
            RECORD(1:12) = 'In program '''
            CALL SEMCHS(RECORD(13:),PRNAME,N)
            N = N+13
            RECORD(N:N)=''''
         ELSE
C
C In obeyed command - retrieve command ? - maybe later
C
            N = 20
            RECORD(1:20) = 'In immediate command'
         ENDIF
         IF (SEMDIA(RECORD(1:N),NDIERR)) GOTO 20
C
C Unwind variable table back to next program level
C
         IF (SEMUNW(INPLOC(INPLEV))) GOTO 20
C
         FORLEV = INPFOR(INPLEV)
         INPLEV = INPLEV - 1
C
         GOTO 10
      ENDIF
C
      SEMTBK=.FALSE.
C
   20 RETURN
C
C Copyright (C) 1987,1989,1990:  Synoptics Ltd,  All Rights Reserved
C
      END
