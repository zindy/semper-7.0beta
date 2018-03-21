C Semper 6 processing module BPRINT
C
      SUBROUTINE BPRINT
C
C Prints out a rectangular subregion of a picture as determined by
C TSTSRG (with SIZE defaulted acc to terminal width)
C
C Global declarations
C
      INCLUDE 'COMMON'
C
      INTEGER MAXIPB
      PARAMETER (MAXIPB=LNBUF/LNINT)
C
      LOGICAL SEMROW,OPT,VARSET,SEMLU,TSTSRG,MP,USEINT,SEMCON,ABANDN
      LOGICAL SEMTPS
      REAL SCALE,T,R
      INTEGER IB1(MAXIPB),IB2(MAXIPB),IWID,ILEN
      INTEGER I,I1,I2,IS,IVAL,J,NEPR,SIZE,SI2,SWITCH,POWER,FORM,INFORM
      CHARACTER HEADER(4)*7,MINUS
      PARAMETER (MINUS='-')
C
C Packed names
C
      INTEGER NSIZE,NSI3,NMP,NLAYER
      PARAMETER (NSIZE=30786,NSI3=30793,NMP=21440,NLAYER=19265)
C
      EQUIVALENCE (RB1,IB1),(RB2,IB2)
C
      DATA HEADER / '       ', 'Im part', 'Modulus', 'Phase  ' /
C
C Initialise options etc
C
      FORM=FORMN(LP1)
      SWITCH=1
      MP=OPT(NMP).AND.FORM.EQ.NFMCOM
      IF (MP) SWITCH=3
C
C Establish internal form
C
      IF (FORM.EQ.NFMCOM) THEN
         INFORM=NFMCOM
      ELSE IF (FORM.EQ.NFMFP) THEN
         INFORM=NFMFP
      ELSE
         INFORM=NFMINT
      ENDIF
C
C Fetch current terminal page size
C
      IF (SEMTPS(IWID,ILEN)) GOTO 120
C
C Find largest size fitting current page width
C
      I=(MIN(IWID,RECLEN)-8)/7
      SIZE=IVAL(NSIZE)
      IF (SIZE.LE.0 .OR. SIZE.GT.I) THEN
C
C If defaulting, odd size looks nicer
C
         IF (I/2*2.EQ.I) I=I-1
         IF (SEMLU(2,NSIZE,REAL(I))) GOTO 120
         IF (.NOT.VARSET(NLAYER)) THEN
            IF (SEMLU(2,NSI3,1.0)) GOTO 120
         ENDIF
      ENDIF
      IF (TSTSRG(1,LP1)) GOTO 120
      SIZE=SMGI4-SMGI1+1
      NEPR=SIZE
      IF (FORM.EQ.NFMCOM) NEPR=2*NEPR
      SI2=SMGI5-SMGI2+1
C
C Fetch data to local store
C
      I2=0
      DO 20 J=1,SI2
         IF (SEMROW(1,RB1,INFORM,SMGI2+J-1,SMGI3,LP1)) GOTO 120
         I1=SMGI1
         IF (FORM.EQ.NFMCOM) I1=2*I1-1
         DO 10 I=I1,I1+NEPR-1
            I2=I2+1
            IF (INFORM.EQ.NFMINT) THEN
               RB2(I2)=IB1(I)
            ELSE
               RB2(I2)=RB1(I)
            ENDIF
   10    CONTINUE
   20 CONTINUE
C
C Find maximum modulus within data
C
      IS=1
      IF (MP) IS=2
      SCALE=0.0
      DO 30 I=1,I2,IS
         T=ABS(RB2(I))
         IF (MP) THEN
            R=RB2(I+1)
            T=T*T+R*R
         ENDIF
         SCALE=MAX(SCALE,T)
   30 CONTINUE
      IF (MP) SCALE=SQRT(SCALE)
C
C For byte data or suitable integer data, print as
C unscaled integer values
C
      USEINT = FORM.EQ.NFMBYT .OR. (FORM.EQ.NFMINT .AND. SCALE.LT.1.0E5)
      IF (USEINT .OR. SCALE .EQ. 0.0) THEN
         POWER=0
         SCALE=1.
C
C Otherwise, drop scaling factor back to next smaller power of ten
C
      ELSE
         POWER=INT(LOG10(SCALE)+100.0)-100
         SCALE=10.0**POWER
      ENDIF
      IF (ABANDN(ERROR)) GOTO 120
C
C Check if scale seems reasonable (for NaN etc.)
C
      IF (POWER .GT. 99 .OR. POWER .LT. -99) THEN
         ERROR = 92
         GOTO 120
      ENDIF
C
C Print part header, numbering columns
C
      IF (POWER.NE.0) THEN
         WRITE (RECORD,40) POWER
   40    FORMAT ('Values printed / 10^',I3)
         IF (SEMCON(RECORD)) GOTO 120
      ENDIF
C
      IF (NLAYS(LP1).NE.1) THEN
         WRITE (RECORD,50) SMGI3,SMGI3-CLAYN(LP1)
   50    FORMAT ('Layer ',I5,'  Z-coord ',I5)
         IF (SEMCON(' ')) GOTO 120
         IF (SEMCON(RECORD)) GOTO 120
      ENDIF
C
   60 I1=SMGI1-CCOLN(LP1)
      I2=SMGI4-CCOLN(LP1)
C
      WRITE (RECORD,70) HEADER(SWITCH),(I,I=I1,I2)
   70 FORMAT (A7,1X,30I7)
      IF (SEMCON(' ')) GOTO 120
      IF (SEMCON(RECORD)) GOTO 120
C
      RECORD(1:8)='       -'
      DO 80 I=1,SIZE
         RECORD(7*I+2:7*I+8)='-------'
   80 CONTINUE
      IF (SEMCON(RECORD(1:7*SIZE+8))) GOTO 120
C
C For each row within data..
C
      I1=1
      IF (FORM.EQ.NFMCOM) IS=2
      IF (SWITCH.EQ.2) I1=2
      DO 110 J=1,SI2
C
C ..print out relevant values from local buffer after scaling
C
         DO 90 I=1,SIZE
            T=RB2(I1)/SCALE
            IF (SWITCH.EQ.3) THEN
               R=RB2(I1+1)/SCALE
               T=SQRT(T*T+R*R)
            ELSE IF (SWITCH.EQ.4) THEN
               T=RB2(I1)
               R=RB2(I1+1)
               IF (T.NE.0.0 .OR. R.NE.0.0) THEN
                  T=ATAN2(R,T)
               ELSE
                  T=0.
               ENDIF
            ENDIF
            RB3(I)=T
            I1=I1+IS
   90    CONTINUE
C
C Write in I7 or F7.3 acc to form and range
C
         WRITE (RECORD(1:5),'(I5)') CROWN(LP1)-SMGI2-J+1
         RECORD(6:8)=' | '
         DO 100 I=1,SIZE
            IF (USEINT) THEN
               WRITE (RECORD(7*I+2:7*I+8),'(I7)') NINT(RB3(I))
            ELSE
               WRITE (RECORD(7*I+2:7*I+8),'(F7.3)') RB3(I)
            ENDIF
  100    CONTINUE
         IF (SEMCON(RECORD(1:7*SIZE+8))) GOTO 120
  110 CONTINUE
C
C Second pass required?
C
      IF (SWITCH.EQ.1 .AND. FORM.EQ.NFMCOM) THEN
         SWITCH=2
         GOTO 60
      ELSE IF (SWITCH.EQ.3) THEN
         SWITCH=4
         GOTO 60
      ENDIF
C
  120 RETURN
C
C Copyright (C) 1987-1993:  Synoptics Ltd,  All Rights Reserved
C
      END
