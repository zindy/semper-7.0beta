C Semper 6 system module FSOPTN
C
      LOGICAL FUNCTION FSOPTN(OPC,N)
C
C Sets OPC to 1, 2 or 3 according to whether option FRAME, PARTITION or
C PICTURE is set.  If none of these options is set, option PICTURE is
C assumed.  If more than one of these options is set, an error is
C returned. N is set to the value of key $1.  If key $1 is not set,
C N defaults to the value of variable CFRAME, for option FRAME and to
C the value of variable DISPLA for option PARTITION or PICTURE.
C
      INTEGER OPC,N
C
      INTEGER IVAL
      LOGICAL OPT,VARSET
C
      INCLUDE 'COMMON'
C
      LOGICAL LOPT(3),LFRAME,LPARTI
      INTEGER I,J,NOPT(3)
C
      EQUIVALENCE (LFRAME,LOPT(1)),(LPARTI,LOPT(2))
C
C Packed names
C
      INTEGER NFRAME,NPARTI,NPICTU,NDLLR1
      PARAMETER (NFRAME=10321,NPARTI=25658,NPICTU=25963,NDLLR1=-12441)
C
      DATA NOPT /NFRAME,NPARTI,NPICTU/
C
      FSOPTN=.TRUE.
C
C See if options FRAME, PARTITION and PICTURE are set
C
      DO 10 I=1,3
         LOPT(I)=OPT(NOPT(I))
   10 CONTINUE
C
C Fault conflict between options FRAME, PARTITION and PICTURE
C
      DO 30 I=1,2
         DO 20 J=I+1,3
            IF (LOPT(I).AND.LOPT(J)) THEN
               ERROR=60
               IDERR=NOPT(I)
               IDERR2=NOPT(J)
               GOTO 40
            ENDIF
   20    CONTINUE
   30 CONTINUE
C
C Determine whether to use frame, partition or picture coordinates for
C graphics coordinates
      IF (LFRAME) THEN
         OPC=1
      ELSE IF (LPARTI) THEN
         OPC=2
      ELSE
         OPC=3
      ENDIF
C
C Determine frame, partition or picture number from key $1 if set
      IF (VARSET(NDLLR1)) THEN
         N=IVAL(NDLLR1)
C
C Otherwise, use current frame or current display partition/picture
C number as appropriate
      ELSE
         IF (OPC.EQ.1) THEN
            N=INT(CFRAME)
         ELSE
            N=INT(DISPLA)
         ENDIF
      ENDIF
C
      FSOPTN=.FALSE.
   40 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
