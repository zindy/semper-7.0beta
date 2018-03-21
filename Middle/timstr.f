C Semper 6 system module TIMSTR
C
      CHARACTER*8 FUNCTION TIMSTR(IHOUR,IMINUT,ISECON)
C
C Returns an 8 character string containing the specified time in
C text form.  If any argument is out of range, asterisks '*' are
C substituted in the return string.
C
      INTEGER IHOUR,IMINUT,ISECON
C
      CHARACTER DECCHA(0:9)
      INTEGER N1,N2
C
      DATA DECCHA /'0','1','2','3','4','5','6','7','8','9'/
C
C Initialise the return string
      TIMSTR='  :  :  '
C
C Encode the hour string
      IF (IHOUR.LT.0.OR.IHOUR.GT.23) THEN
         TIMSTR(1:2)='**'
      ELSE
         N1=IHOUR/10
         N2=IHOUR-10*N1
         TIMSTR(1:1)=DECCHA(N1)
         TIMSTR(2:2)=DECCHA(N2)
      ENDIF
C
C Encode the minute string
      IF (IMINUT.LT.0.OR.IMINUT.GT.59) THEN
         TIMSTR(4:5)='**'
      ELSE
         N1=IMINUT/10
         N2=IMINUT-10*N1
         TIMSTR(4:4)=DECCHA(N1)
         TIMSTR(5:5)=DECCHA(N2)
      ENDIF
C
C Encode the second string
      IF (ISECON.LT.0.OR.ISECON.GT.59) THEN
         TIMSTR(7:8)='**'
      ELSE
         N1=ISECON/10
         N2=ISECON-10*N1
         TIMSTR(7:7)=DECCHA(N1)
         TIMSTR(8:8)=DECCHA(N2)
      ENDIF
C
      RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
