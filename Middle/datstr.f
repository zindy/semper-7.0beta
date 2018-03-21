C Semper 6 system module DATSTR
C
      CHARACTER*11 FUNCTION DATSTR(IDAY,IMONTH,IYEAR)
C
C Returns an 11 character string containing the specified date in
C text form.  If any argument is out of range, asterisks '*' are
C substituted in the return string.
C
      INTEGER IDAY,IMONTH,IYEAR
C
      CHARACTER DECCHA(0:9)
      CHARACTER*3 MONSTR(12)
      INTEGER N1,N2,N3,N4
C
      DATA DECCHA /'0','1','2','3','4','5','6','7','8','9'/
      DATA MONSTR /'Jan','Feb','Mar','Apr','May','Jun',
     +             'Jul','Aug','Sep','Oct','Nov','Dec'/
C
C Initialise the return string
      DATSTR='  -   -    '
C
C Encode the day string
      IF (IDAY.LT.1.OR.IDAY.GT.31) THEN
         DATSTR(1:2)='**'
      ELSE
         N1=IDAY/10
         N2=IDAY-10*N1
         IF (N1.NE.0) DATSTR(1:1)=DECCHA(N1)
         DATSTR(2:2)=DECCHA(N2)
      ENDIF
C
C Encode the month string
      IF (IMONTH.LT.1.OR.IMONTH.GT.12) THEN
         DATSTR(4:6)='***'
      ELSE
         DATSTR(4:6)=MONSTR(IMONTH)
      ENDIF
C
C Encode the year string
      IF (IYEAR.LT.1000.OR.IYEAR.GT.9999) THEN
         DATSTR(8:11)='****'
      ELSE
         N1=IYEAR/1000
         N2=MOD(IYEAR,1000)/100
         N3=MOD(IYEAR,100)/10
         N4=MOD(IYEAR,10)
         DATSTR(8:8)=DECCHA(N1)
         DATSTR(9:9)=DECCHA(N2)
         DATSTR(10:10)=DECCHA(N3)
         DATSTR(11:11)=DECCHA(N4)
      ENDIF
C
      RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
