C Semper 6 processing module BLKMEA
C
      SUBROUTINE BLKMEA
C
C Provides verbs LMEAN, HP and SHARPEN
C - As LMEAN, replaces source points by average taken over square block
C   of neighbouring pixels (size OVER), edges being processed
C   as if outside pixels were repeated outwards
C - As HP, produces original minus block average;
C - As SHARPEN, produces original plus o/p of HP.
C
C Execution time virtually independent of size of square block; could
C be improved further in future through use of FIFO output buffer
C as in FIRFIL
C
C Uses FP/COMPLEX internally
C
      LOGICAL SEMOPN,OPT,SEMROW,LSHARP,LHP,LCOMPL,LHORIZ,LVERT
      INTEGER WIDTH,WIDL,WIDR
      INTEGER NOVER,NSHARP,NHP,NBLUR,NHORIZ,NVERTI
      INTEGER NCOL,NROW,INFORM,ISTEP,LAST,JWKO,JWKI,JIN,JOUT,LINK
      INTEGER I,IVAL
      REAL RNORM,X
C
C Packed names
C
      PARAMETER (NOVER=24885,NSHARP=30721,NHP=13440,NBLUR=3701)
      PARAMETER (NHORIZ=13418,NVERTI=-3419)
C
      INCLUDE 'COMMON'
C
C Initialise
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      LSHARP=VERB.EQ.NSHARP
      LHP=VERB.EQ.NHP
      LHORIZ=OPT(NHORIZ).OR.NROW.EQ.1
      LVERT=OPT(NVERTI)
      WIDTH=IVAL(NOVER)
      IF (WIDTH.LE.1) GOTO 310
      IF (WIDTH.GE.NCOL.AND..NOT.LVERT) GOTO 310
      IF (WIDTH.GE.NROW.AND..NOT.LHORIZ) GOTO 310
      RNORM = WIDTH
      RNORM = 1.0/RNORM
C
C Central point
C
      WIDL = WIDTH/2+1
C
C Open workspace
C
         LCOMPL=FORMN(LP1).EQ.3.AND.FORMN(LP2).EQ.3
         INFORM=2
         ISTEP=1
         LAST=NCOL
         IF (LCOMPL) THEN
            INFORM=3
            ISTEP=2
            LAST=NCOL+NCOL
         ENDIF
C
C Open workspace
C
         IF (.NOT.LHORIZ) THEN
            LP3=0
            IF (SEMOPN(3,0,NCOL,WIDTH,1,1,INFORM,LP3)) GOTO 320
         ENDIF
C
         WIDR=(WIDTH-1)/2
         JWKO=1
         JWKI=1
         JIN=1
         JOUT=1
         IF (LHORIZ) GOTO 220
         IF (.NOT.LVERT) RNORM=RNORM*RNORM
C
C Filter is separable - code for column filter follows, using calls
C to input, output, save and recover routines below; the row
C averages are performed by RSUM1D calls embedded in the input code
C
C Initialise column average at top
C Call INPUT
C
         LINK=1
         GOTO 240
C
   40    X=WIDL
         DO 50 I=1,LAST
            RB2(I)=RB3(I)
            RB1(I)=RB2(I)*X
   50    CONTINUE
         IF (WIDR.LT.1) GOTO 90
C
C Call SAVE
C
   60    LINK=1
         GOTO 290
C
   70    DO 80 I=1,LAST
            RB1(I)=RB1(I)+RB3(I)
   80    CONTINUE
         IF (JIN.LE.WIDR) THEN
C
C Call INPUT
C
            LINK=2
            GOTO 240
         ENDIF
C
C Set top edge
C Call INPUT
C
   90    LINK=3
         GOTO 240
C
C Call SAVE
C
  100    LINK=2
         GOTO 290
C
  110    DO 120 I=1,LAST
            RB1(I)=RB1(I)+RB3(I)-RB2(I)
  120    CONTINUE
C
C Call OUTPUT
C
         LINK=1
         GOTO 250
  130    IF (JOUT.LE.WIDL) GOTO 90
C
C Set central range
C
  140    IF (JIN.GT.NROW) GOTO 190
C
C Call INPUT
C
         LINK=4
         GOTO 240
C
C Call RECOVER
C
  150    LINK=1
         GOTO 300
C
C Call SAVE
C
  160    LINK=3
         GOTO 290
C
  170    DO 180 I=1,LAST
            RB1(I)=RB1(I)+RB3(I)-RB2(I)
  180    CONTINUE
C
C Call OUTPUT
C
         LINK=2
         GOTO 250
C
C Set bottom edge
C
  190    IF (JOUT.GT.NROW) GOTO 320
C
C Call RECOVER
C
         LINK=2
         GOTO 300
C
  200    DO 210 I=1,LAST
            RB1(I)=RB1(I)+RB3(I)-RB2(I)
  210    CONTINUE
C
C Call OUTPUT
C
         LINK=3
         GOTO 250
C
C Horizontal filtering only (or 1-D source)
C
  220    IF (SEMROW(1,RB4,INFORM,JOUT,1,LP1)) GOTO 320
         CALL RSUM1D(RB1,RB4,NCOL,WIDTH,ISTEP)
C
C Call OUTPUT
C
         LINK=4
         GOTO 250
C
  230    IF (JOUT.LE.NROW) GOTO 220
         GOTO 320
C
C INPUT - code inputting row JIN to RB3 via RB4,
C         including 1-D running sum unless vertical
C
  240    IF (LVERT) THEN
            IF (SEMROW(1,RB3,INFORM,JIN,1,LP1)) GOTO 320
         ELSE
            IF (SEMROW(1,RB4,INFORM,JIN,1,LP1)) GOTO 320
            CALL RSUM1D(RB3,RB4,NCOL,WIDTH,ISTEP)
         ENDIF
         JIN=JIN+1
         GOTO (40,60,100,150), LINK
C
C OUTPUT - code outputting normalised row JOUT from RB1 via RB4,
C          including extra processing for HP, SHARPEN
C
  250    IF (LSHARP.OR.LHP) THEN
            IF (.NOT.LHORIZ) THEN
C
C Fetch original row
C
               IF (SEMROW(1,RB4,INFORM,JOUT,1,LP1)) GOTO 320
            ENDIF
            IF (LHP) THEN
               DO 260 I=1,LAST
                  RB4(I)=RB4(I)-RB1(I)*RNORM
  260          CONTINUE
            ELSE
               DO 270 I=1,LAST
                  RB4(I)=RB4(I)*2.-RB1(I)*RNORM
  270          CONTINUE
            ENDIF
         ELSE
C
            DO 280 I=1,LAST
               RB4(I)=RB1(I)*RNORM
  280       CONTINUE
         ENDIF
C
         IF (SEMROW(2,RB4,INFORM,JOUT,1,LP2)) GOTO 320
         JOUT=JOUT+1
         GOTO (130,140,190,230), LINK
C
C SAVE - code saving RB3 in workspace
C
  290    IF (SEMROW(2,RB3,INFORM,JWKO,1,LP3)) GOTO 320
         JWKO=JWKO+1
         IF (JWKO.GT.WIDTH) JWKO=1
         GOTO (70,110,170), LINK
C
C RECOVER - code recovering RB2 from workspace
C
  300    IF (SEMROW(1,RB2,INFORM,JWKI,1,LP3)) GOTO 320
         JWKI=JWKI+1
         IF (JWKI.GT.WIDTH) JWKI=1
         GOTO (160,200), LINK
C
C Errors
C
  310 IDERR=NOVER
      ERROR=3
  320 RETURN
C
C Copyright (C) 1987,1988,1989,1990: Synoptics Ltd, All Rights Reserved
C
      END
