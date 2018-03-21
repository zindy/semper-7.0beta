C Semper 6 subsidiary module DFTM1
C
      LOGICAL FUNCTION DFTM1(CBS,WCOL,WROW,N,NP,OLEN,RBSP,FN,BASE,
     +                       SINGLE,BFLYN)
C
      REAL    CBS(0:*),WCOL(0:*),WROW(0:*)
      INTEGER N(2),NP(2),OLEN(2),FN,BASE(9,2),BFLYN
      INTEGER*4 RBSP
      LOGICAL SINGLE
C
      LOGICAL DFTM2
C
      DFTM1=.TRUE.
C
C Interfacing routine to pass appropriate array addresses to DFTM2 which
C carries out the necessary transformation
C
C Carry out transform over rows
C
      IF (DFTM2(1,CBS,WCOL,N,NP,OLEN,RBSP,FN,BASE,SINGLE,BFLYN)) GOTO 10
C
C Carry out transform over columns
C
      IF (DFTM2(2,CBS,WROW,N,NP,OLEN,RBSP,FN,BASE,SINGLE,BFLYN)) GOTO 10
C
      DFTM1=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module DFTM2
C
      LOGICAL FUNCTION DFTM2(DIR,CBS,W,N,NP,OLEN,RBSP,FN,BASE,SINGLE,
     +                       BFLYN)
C
      INTEGER DIR,N(2),NP(2),OLEN(2),FN,BASE(9,2),BFLYN
      INTEGER*4 RBSP
      REAL    CBS(0:*),W(0:*)
      LOGICAL SINGLE
C
C Performs complete or part 2-D level FN xform pass
C
C [in]     integer DIR: 1 = x direction, 2 = y direction
C [in,out] complex CBS: complex array of size N(1)*N(2) to be xformed
C [in]     complex W(0:*): twiddle factors for x or y direction
C [in]     integer N(2): xform size
C [in]     integer NP(2): size of data segment present in CBS
C [in]     integer OLEN(2): output block lengths for current xform pass
C [in]     integer*4 RBSP: spacing at which xform rows are stored in CBS
C [in]     integer FN: xform factor number (pass number)
C [in]     integer BASE(9,2): xform size factors
C [in]     logical SINGLE: single butterfly mode flag
C [in]     integer BFLYN: butterfly number
C
C If not SINGLE:
C    full row,col pass to size OLEN(D) xforms over array size NP(D)
C If SINGLE:
C    row xform: for all butterflies/blocks in the rows
C    col xform: for single butterfly of rows only (number BFLYN)
C Directions identified by D=1,2 for xform along rows,cols, ie wrt X,Y
C - full data set size is N(D); level base is BASE(FN,D);
C   twid.facs exp{+2PIiJ/N(D)} provided in W(J)
C Rows are provided in CBS at spacing RBSP; not nec = N(1) or OLEN(1)
C
C N(D) gives original full data set size, and is used here only in
C      twiddle increment calculation
C NP(D) gives size of data set portion provided to DFTM2; this is
C      always complete rows [ie NP(1) = N(1) ], but not usually
C      complete cols: if SINGLE, 2,3,4,5 isolated rows are supplied,
C      and otherwise a set of NP(2) contiguous rows
C
      LOGICAL SEMBRK
C
      REAL CS1,SN1,CS2,SN2,CS3,SN3,CS4,SN4
      REAL C1R,C1I,C2R,C2I,C3R,C3I,C4R,C4I
      INTEGER INLEN,WI,WINC,P,PB,Q
      INTEGER*4 ESP,DPP,PF,PP,QP,RP,SP,TP
C
C Cube roots of one
C
      REAL R31R,R31I,R32R,R32I
      PARAMETER ( R31R = -0.5, R31I =  0.8660254 )
      PARAMETER ( R32R = -0.5, R32I = -0.8660254 )
C
C Fifth roots of one
C
      REAL R51R,R51I,R52R,R52I,R53R,R53I,R54R,R54I
      PARAMETER ( R51R =  0.3090169, R51I =  0.9510565 )
      PARAMETER ( R52R = -0.8090169, R52I =  0.5877852 )
      PARAMETER ( R53R = -0.8090169, R53I = -0.5877852 )
      PARAMETER ( R54R =  0.3090169, R54I = -0.9510565 )
C
C ---------------------------------------------------------------------
C
      DFTM2=.TRUE.
C
C Xform input lengths
C
      INLEN=OLEN(DIR)/BASE(FN,DIR)
C
C Twiddle factor increment (2PIi/OLEN = 2PIi.WINC/N)
C
      WINC=N(DIR)/OLEN(DIR)
C
      WINC=WINC+WINC
C
C Prepare WI = initial offset into twiddle factor array
C
      IF (DIR.EQ.2.AND.SINGLE) THEN
         WI=BFLYN*WINC
      ELSE
         WI=0
      ENDIF
C
C Prepare ESP = incr between row/col elements INLEN apart
C         DPP = PP increment repeating PP over all rows/cols
C         PF  = factor for determining initial value of PP
C
      IF (DIR.EQ.1) THEN
         ESP=INLEN
         DPP=RBSP
         PF=1
      ELSE
         IF (SINGLE) THEN
            ESP=RBSP
            PF=0
         ELSE
            ESP=INLEN
            ESP=ESP*RBSP
            PF=RBSP
         ENDIF
C
         DPP=1
      ENDIF
C
      ESP=ESP+ESP
      DPP=DPP+DPP
C
C -----------
C Base 2 code
C -----------
C
C Base 2 only appears as a first pass, with INLEN=1, so the butterfly
C loop is trivial and can be omitted from the code
C
      IF (BASE(FN,DIR).EQ.2) THEN
C
C Loop over blocks
C
         DO 20 P=0,NP(DIR)-1,2
C
C Prepare PP = pointer to element P of first row,col (DIR=1,2)
C
            PP=P
            PP=PF*PP
C
            PP=PP+PP
C
C Core loop over col/row elements
C
            DO 10 Q=1,NP(3-DIR)
               QP=PP+ESP
C
C Core twiddle code
C
               C1R=CBS(QP  )
               C1I=CBS(QP+1)
C
C Core butterfly code
C
               CBS(QP  )=CBS(PP  )-C1R
               CBS(QP+1)=CBS(PP+1)-C1I
C
               CBS(PP  )=CBS(PP  )+C1R
               CBS(PP+1)=CBS(PP+1)+C1I
C
               PP=PP+DPP
   10       CONTINUE
C
C If SINGLE, break out after this single butterfly
C
            IF (DIR.EQ.2.AND.SINGLE) GOTO 120
C
C Check for any abandon requests
C
            IF (SEMBRK()) GOTO 130
   20    CONTINUE
C
C -----------
C Base 3 code
C -----------
C
      ELSE IF (BASE(FN,DIR).EQ.3) THEN
C
C Loop over butterflies within block
C
         DO 50 PB=0,INLEN-1
C
C Prepare twiddle factors
C
            CS1=W(WI)
            SN1=W(WI+1)
            CS2=W(2*WI)
            SN2=W(2*WI+1)
C
C Loop over blocks
C
            DO 40 P=PB,NP(DIR)-1,OLEN(DIR)
C
C Prepare PP = pointer to element P of first row,col (DIR=1,2)
C
               PP=P
               PP=PF*PP
C
               PP=PP+PP
C
C Core loop over col/row elements
C
               DO 30 Q=1,NP(3-DIR)
                  QP=PP+ESP
                  RP=QP+ESP
C
C Core twiddle code
C
                  IF (WI.EQ.0) THEN
                     C1R=CBS(QP)
                     C1I=CBS(QP+1)
                     C2R=CBS(RP)
                     C2I=CBS(RP+1)
                  ELSE
                     C1R=CBS(QP)*CS1-CBS(QP+1)*SN1
                     C1I=CBS(QP)*SN1+CBS(QP+1)*CS1
                     C2R=CBS(RP)*CS2-CBS(RP+1)*SN2
                     C2I=CBS(RP)*SN2+CBS(RP+1)*CS2
                  ENDIF
C
C Core butterfly code
C
                  CBS(QP  )=CBS(PP  )+R31R*C1R-R31I*C1I
     +                               +R32R*C2R-R32I*C2I
                  CBS(QP+1)=CBS(PP+1)+R31R*C1I+R31I*C1R
     +                               +R32R*C2I+R32I*C2R
C
                  CBS(RP  )=CBS(PP  )+R32R*C1R-R32I*C1I
     +                               +R31R*C2R-R31I*C2I
                  CBS(RP+1)=CBS(PP+1)+R32R*C1I+R32I*C1R
     +                               +R31R*C2I+R31I*C2R
C
                  CBS(PP  )=CBS(PP  )+     C1R
     +                               +     C2R
                  CBS(PP+1)=CBS(PP+1)+     C1I
     +                               +     C2I
C
                  PP=PP+DPP
   30          CONTINUE
C
C If SINGLE, break out after this single butterfly
C
               IF (DIR.EQ.2.AND.SINGLE) GOTO 120
C
C Check for any abandon requests
C
               IF (SEMBRK()) GOTO 130
   40       CONTINUE
C
C Bump twiddle index
C
            WI=WI+WINC
   50    CONTINUE
C
C -----------
C Base 4 code
C -----------
C
      ELSE IF (BASE(FN,DIR).EQ.4) THEN
C
C Loop over butterflies within block
C
         DO 80 PB=0,INLEN-1
C
C Prepare twiddle factors
C
            CS1=W(WI)
            SN1=W(WI+1)
            CS2=W(2*WI)
            SN2=W(2*WI+1)
            CS3=W(3*WI)
            SN3=W(3*WI+1)
C
C Loop over blocks
C
            DO 70 P=PB,NP(DIR)-1,OLEN(DIR)
C
C Prepare PP = pointer to element P of first row,col (DIR=1,2)
C
               PP=P
               PP=PF*PP
C
               PP=PP+PP
C
C Core loop over col/row elements
C
               DO 60 Q=1,NP(3-DIR)
                  QP=PP+ESP
                  RP=QP+ESP
                  SP=RP+ESP
C
C Core twiddle code
C
                  IF (WI.EQ.0) THEN
                     C1R=CBS(QP)
                     C1I=CBS(QP+1)
                     C2R=CBS(RP)
                     C2I=CBS(RP+1)
                     C3R=CBS(SP)
                     C3I=CBS(SP+1)
                  ELSE
                     C1R=CBS(QP)*CS1-CBS(QP+1)*SN1
                     C1I=CBS(QP)*SN1+CBS(QP+1)*CS1
                     C2R=CBS(RP)*CS2-CBS(RP+1)*SN2
                     C2I=CBS(RP)*SN2+CBS(RP+1)*CS2
                     C3R=CBS(SP)*CS3-CBS(SP+1)*SN3
                     C3I=CBS(SP)*SN3+CBS(SP+1)*CS3
                  ENDIF
C
C Core butterfly code (fourth roots of one are (0,1),(-1,0),(0,-1))
C
                  CBS(QP  )=CBS(PP  )-C1I-C2R+C3I
                  CBS(QP+1)=CBS(PP+1)+C1R-C2I-C3R
C
                  CBS(RP  )=CBS(PP  )-C1R+C2R-C3R
                  CBS(RP+1)=CBS(PP+1)-C1I+C2I-C3I
C
                  CBS(SP  )=CBS(PP  )+C1I-C2R-C3I
                  CBS(SP+1)=CBS(PP+1)-C1R-C2I+C3R
C
                  CBS(PP  )=CBS(PP  )+C1R+C2R+C3R
                  CBS(PP+1)=CBS(PP+1)+C1I+C2I+C3I
C
                  PP=PP+DPP
   60          CONTINUE
C
C If SINGLE, break out after this single butterfly
C
               IF (DIR.EQ.2.AND.SINGLE) GOTO 120
C
C Check for any abandon requests
C
               IF (SEMBRK()) GOTO 130
   70       CONTINUE
C
C Bump twiddle index
C
            WI=WI+WINC
   80    CONTINUE
C
C -----------
C Base 5 code
C -----------
C
      ELSE IF (BASE(FN,DIR).EQ.5) THEN
C
C Loop over butterflies within block
C
         DO 110 PB=0,INLEN-1
C
C Prepare twiddle factors
C
            CS1=W(WI)
            SN1=W(WI+1)
            CS2=W(2*WI)
            SN2=W(2*WI+1)
            CS3=W(3*WI)
            SN3=W(3*WI+1)
            CS4=W(4*WI)
            SN4=W(4*WI+1)
C
C Loop over blocks
C
            DO 100 P=PB,NP(DIR)-1,OLEN(DIR)
C
C Prepare PP = pointer to element P of first row,col (DIR=1,2)
C
               PP=P
               PP=PF*PP
C
               PP=PP+PP
C
C Core loop over col/row elements
C
               DO 90 Q=1,NP(3-DIR)
                  QP=PP+ESP
                  RP=QP+ESP
                  SP=RP+ESP
                  TP=SP+ESP
C
C Core twiddle code
C
                  IF (WI.EQ.0) THEN
                     C1R=CBS(QP)
                     C1I=CBS(QP+1)
                     C2R=CBS(RP)
                     C2I=CBS(RP+1)
                     C3R=CBS(SP)
                     C3I=CBS(SP+1)
                     C4R=CBS(TP)
                     C4I=CBS(TP+1)
                  ELSE
                     C1R=CBS(QP)*CS1-CBS(QP+1)*SN1
                     C1I=CBS(QP)*SN1+CBS(QP+1)*CS1
                     C2R=CBS(RP)*CS2-CBS(RP+1)*SN2
                     C2I=CBS(RP)*SN2+CBS(RP+1)*CS2
                     C3R=CBS(SP)*CS3-CBS(SP+1)*SN3
                     C3I=CBS(SP)*SN3+CBS(SP+1)*CS3
                     C4R=CBS(TP)*CS4-CBS(TP+1)*SN4
                     C4I=CBS(TP)*SN4+CBS(TP+1)*CS4
                  ENDIF
C
C Core butterfly code
C
                  CBS(QP  )=CBS(PP  )+R51R*C1R-R51I*C1I
     +                               +R52R*C2R-R52I*C2I
     +                               +R53R*C3R-R53I*C3I
     +                               +R54R*C4R-R54I*C4I
                  CBS(QP+1)=CBS(PP+1)+R51R*C1I+R51I*C1R
     +                               +R52R*C2I+R52I*C2R
     +                               +R53R*C3I+R53I*C3R
     +                               +R54R*C4I+R54I*C4R
C
                  CBS(RP  )=CBS(PP  )+R52R*C1R-R52I*C1I
     +                               +R54R*C2R-R54I*C2I
     +                               +R51R*C3R-R51I*C3I
     +                               +R53R*C4R-R53I*C4I
                  CBS(RP+1)=CBS(PP+1)+R52R*C1I+R52I*C1R
     +                               +R54R*C2I+R54I*C2R
     +                               +R51R*C3I+R51I*C3R
     +                               +R53R*C4I+R53I*C4R
C
                  CBS(SP  )=CBS(PP  )+R53R*C1R-R53I*C1I
     +                               +R51R*C2R-R51I*C2I
     +                               +R54R*C3R-R54I*C3I
     +                               +R52R*C4R-R52I*C4I
                  CBS(SP+1)=CBS(PP+1)+R53R*C1I+R53I*C1R
     +                               +R51R*C2I+R51I*C2R
     +                               +R54R*C3I+R54I*C3R
     +                               +R52R*C4I+R52I*C4R
C
                  CBS(TP  )=CBS(PP  )+R54R*C1R-R54I*C1I
     +                               +R53R*C2R-R53I*C2I
     +                               +R52R*C3R-R52I*C3I
     +                               +R51R*C4R-R51I*C4I
                  CBS(TP+1)=CBS(PP+1)+R54R*C1I+R54I*C1R
     +                               +R53R*C2I+R53I*C2R
     +                               +R52R*C3I+R52I*C3R
     +                               +R51R*C4I+R51I*C4R
C
                  CBS(PP  )=CBS(PP  )+     C1R
     +                               +     C2R
     +                               +     C3R
     +                               +     C4R
                  CBS(PP+1)=CBS(PP+1)+     C1I
     +                               +     C2I
     +                               +     C3I
     +                               +     C4I
C
                  PP=PP+DPP
   90          CONTINUE
C
C If SINGLE, break out after this single butterfly
C
               IF (DIR.EQ.2.AND.SINGLE) GOTO 120
C
C Check for any abandon requests
C
               IF (SEMBRK()) GOTO 130
  100       CONTINUE
C
C Bump twiddle index
C
            WI=WI+WINC
  110    CONTINUE
C
C -----------
C Base 1 code
C -----------
C
      ELSE
C
C Trivial, but necessary to pad out the direction with fewer passes
C
         CONTINUE
      ENDIF
C
  120 DFTM2=.FALSE.
C
  130 RETURN
C
C Copyright (C) 1992:  Synoptics Ltd,  All Rights Reserved
C
      END
