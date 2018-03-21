C Semper 6 primitive proving program for A1CONV
C
      program main
      INTEGER TERM2
C
C ****** CHANGE ******
C TERM2 is the unit number for verification messages
      PARAMETER (TERM2=6)
C INPUT is the number of a unit on which the following records (without
C the initial C-spaces!) should be supplied  -
C  !"#$%&'()*+,-./0123456789:;<=>?
C @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_
C `abcdefghijklmnopqrstuvwxyz{|}~
      PARAMETER (INPUT=5)
C (ideally, try both disc file and terminal input in turn)
C ****** ****** ******
C
      LOGICAL ERRFLG
      INTEGER B1(96),B2(96),NPTS
      CHARACTER*1 BB1(96)
C     Note: the \\ may cause problems
      DATA BB1/
     + ' ','!','"','#','$','%','&','''','(',')','*','+',',','-','.','/',
     + '0','1','2','3','4','5','6','7','8','9',':',';','<','=','>','?',
     + '@','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O',
     + 'P','Q','R','S','T','U','V','W','X','Y','Z','[','\\',']','^','_',
     + '`','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o',
     + 'p','q','r','s','t','u','v','w','x','y','z','{','|','}','~',' '/
      DATA ERRFLG/.FALSE./
      DO I=1,96
        B1(I)=ichar(BB1(I))
      ENDDO
C
C Character map from input records
      WRITE (TERM2,10)
   10 FORMAT (/' Character mapping from input records'/1X,36(1H-)/)
      WRITE (TERM2,20) INPUT,B1
   20 FORMAT (' The following records should be supplied on unit',I4
     +  ,3(/32A1)/)
      READ (INPUT,30) B2
   30 FORMAT (32A1)
      write(term2,30)' '
      write(TERM2,*)' Data received was '
      write(term2,30)' '
      write(TERM2,30) B2


C Call checking routine
      IRET=1
      GOTO 90
C
C Character map from DATA stmt
C   40 WRITE (TERM2,50)
   50 FORMAT (/' Character mapping from data statement'/1X,37(1H-)/)
   40 DO 60 I=1,96
   60 B2(I)=B1(I)
      IRET=2
      GOTO 90
C
C Complete
   70 IF (.NOT.ERRFLG) WRITE (TERM2,80)
   80 FORMAT (/' Test completed without errors')
      STOP
C
C Routine testing character map - omits char 127 (DEL)
   90 NPTS=96
      if(iret.eq.1)then
                write(term2,*)' '
                write(term2,*)'Testing data read in '
      else
                write(term2,*)' '
                write(term2,*)'Testing data stored '
      endif
      CALL A1CONV(B2,NPTS)
      DO 110 I=1,95
      IM1=I+31
      IF (B2(I).EQ.IM1) GOTO 110
      WRITE (TERM2,100) B1(I),B2(I),IM1
  100 FORMAT (' Error: char ',A1,' mapped to',I4,' instead of',I4)
      ERRFLG=.TRUE.
  110 CONTINUE
      GOTO (40,70), IRET
C
C Copyright (C) 1987:  Synoptics Ltd,  All Rights Reserved
C
      END
