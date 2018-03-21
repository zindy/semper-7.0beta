C Semper 6 processing module GAU
C
      SUBROUTINE GAU
C
C Generates (real) Lorentzians and Gaussians in LP1
C
      REAL VAL
      LOGICAL SEMROW
C
      INCLUDE 'COMMON'
C
      REAL A,A2,B2,C,CENTRE,D,R,RAD,X,Y
      INTEGER I,I0,J,L
      LOGICAL LOR
C
C Packed names
C
      INTEGER NRADIU,NLOREN
      PARAMETER (NRADIU=28844,NLOREN=19818)
C
C Initialise
C
      LOR=VERB.EQ.NLOREN
C
C Fill with function determined by VERB and RADIUS
C
      I0=CCOLN(LP1)
      RAD=VAL(NRADIU)
      IF (RAD.EQ.0.) THEN
C
C Error
C
         IDERR=NRADIU
         ERROR=3
         GOTO 50
      ENDIF
C
      IF (LOR) THEN
         A2=RAD*RAD
         IF (RAD.GT.0.) THEN
            B2=A2
         ELSE
            B2=0.
         ENDIF
         IF (RAD.LT.0.) THEN
            CENTRE=0.
         ELSE
            CENTRE=1.
         ENDIF
      ELSE
         A=-0.5/RAD/RAD
         D=-7.5/A
         GOTO 10
      ENDIF
C
C Make function
C
   10 DO 40 L=1,NLAYS(LP1)
         DO 30 J=1,NROWS(LP1)
            X=L-CLAYN(LP1)
            Y=J-CROWN(LP1)
            Y=Y*Y+X*X
            DO 20 I=1,NCOLS(LP1)
               X=I-I0
               R=Y+X*X
               IF (LOR) THEN
                  C=CENTRE
                  IF (R.NE.0.) C=A2/(B2+R)
               ELSE
                  C=0.
                  IF (R.LE.D) C=EXP(A*R)
               ENDIF
               RB1(I)=C
   20       CONTINUE
            IF (SEMROW(2,RB1,2,J,L,LP1)) RETURN
   30    CONTINUE
   40 CONTINUE
   50 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
