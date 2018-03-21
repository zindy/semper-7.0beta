      SUBROUTINE CLIPP
C---------------------------------------------------------------------
C
C   Processes a picture outputing values clipped at OMIN and OMAX
C
C   NOTE:  Complex input generates an error.
C          OMIN <= OMAX. OMIN=OMAX produces a warning message.
C
C---------------------------------------------------------------------
C
C  Semper syntax:
C
C  Clip :CLIPP $1=sel from=$1 $2=from to=$2 omin=0 omax=
C             open(lp1,old)=from open(lp2,new,lp1)=to
C
C---------------------------------------------------------------------
C
C  Semper functions:
C
      LOGICAL SEMROW,SEMCON,VARSET,GETRNG
      INTEGER IPACK,IVALPN
      REAL VAL
C  Variables
      LOGICAL DOWORK
      INTEGER NCOL,NROW,NLAY,INFORM,NFROM,NOMIN,NOMAX,I,J,K
      REAL OMIN,OMAX,PMIN,PMAX
C
      INCLUDE 'COMMON'
C
C Set NFROM = SEMPER internal code for variable FROM
C
      NFROM=IPACK('FROM')
C
C Set NOMAX, NOMIN similarly
C
      NOMAX=IPACK('OMAX')
      NOMIN=IPACK('OMIN')
C
C Set up dimensions and form of source picture
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      NLAY=NLAYS(LP1)
      INFORM=FORMN(LP1)
C
C Fault complex input
C
      IF(INFORM .EQ. NFMCOM) THEN
         ERROR=43
         IDERR=IVALPN(NFROM)
         RETURN
      ENDIF
C
C Read values of OMIN and OMAX
C
      OMIN=VAL(NOMIN)
C
      IF(VARSET(NOMAX)) THEN
         OMAX=VAL(NOMAX)
      ELSE
         ERROR=25
         IDERR=NOMAX
         RETURN
      ENDIF
C
C Check for silly values of OMIN and OMAX
C
      IF(OMIN .GE. OMAX) THEN
C OMIN = OMAX
         IF(OMIN .EQ. OMAX) THEN
            WRITE(RECORD,10) OMIN
   10       FORMAT( ' **** WARNING: OMAX = OMIN =',E10.4,' in CLIPP')
            IF(SEMCON(RECORD)) RETURN
         ELSE
C  OMIN > OMAX
            ERROR=60
            IDERR=NOMIN
            IDERR2=NOMAX
            RETURN
         ENDIF
      ENDIF
C
C Read range of current picture to PMIN, PMAX
C
      IF(GETRNG(PMIN,PMAX,LP1)) RETURN
C
C Determine whether processing is required
C
      IF((PMIN .GE. OMIN) .AND. (PMAX .LE. OMAX)) THEN
         DOWORK=.FALSE.
      ELSE
         DOWORK=.TRUE.
      ENDIF
C
C  Start main body of routine..
C
C For each row in each layer in turn..
C
      DO 40 K=1,NLAY
         DO 30 J=1,NROW
C
C ..read in row data to row buffer RB1 in floating point form, from LP1
C
            IF (SEMROW(1,RB1,NFMFP,J,K,LP1)) RETURN
C
C For each row element, alter values held in buffer if required
C
            IF(DOWORK) THEN
C
               DO 20 I=1,NCOL
                  IF(RB1(I) .GT. OMAX) THEN
                     RB1(I)=OMAX
                  ELSE IF(RB1(I) .LT. OMIN) THEN
                     RB1(I)=OMIN
                  ENDIF
   20          CONTINUE
C
            ENDIF
C
C  Output processed row to LP2,
C     (note that conversion from FP to the target form is automatic)
C
            IF (SEMROW(2,RB1,NFMFP,J,K,LP2)) RETURN
C
   30    CONTINUE
   40 CONTINUE
C
      RETURN
C
      END
