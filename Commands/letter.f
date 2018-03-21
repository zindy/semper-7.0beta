C Semper 6 processing module LETTER
C
      SUBROUTINE LETTER
C
C Generates pictures containing lettering on a 5x6 point matrix, or
C copies pictures adding such lettering at the bottom.
C
      INTEGER IVAL,IVALPN,SEMFRM,SEMPPN
      LOGICAL SEMKTX,SEMOPN,SEMROW,SEMLAB,OPT,VARSET
C
      INCLUDE 'COMMON'
C
      REAL BG,BIT,FG
      INTEGER IB1(256),IB2(256),TEXT(80),ENDPTR(10)
      INTEGER CLASS,FORM,I,IFROM,INFORM,IP,ITO,J,L,LABEL(256),LAST
      INTEGER LEFT,MASK,M1,N,N2,NCOL,NCPL,NL,NLINES,NROW,ROW,P1,P2,P2M
      LOGICAL APPEND
C
C Packed names
C
      INTEGER NTO,NFROM,NTEXT,NTITLE,NSIZE,NFG,NBG
      PARAMETER (NTO=-601,NFROM=10335,NTEXT=-225,NTITLE=-381)
      PARAMETER (NSIZE=30786,NFG=9880,NBG=3480)
C
      EQUIVALENCE (RB4,LABEL),(LABEL(LBNCTT),LAST),(LABEL(LBTT1),TEXT)
      EQUIVALENCE (RB1,IB1),(RB2,IB2)
C
      INTEGER M(2,96)
C
C Single bit masks used for decoding
C
      INTEGER LEAD(3)
      DATA LEAD/16384,512,16/
C
C Character masks - 6 rows of 5 dots per character, packed 3 to
C a word, with the first word negated as a 2 row descender flag
C
C Chars 32-63
C
      DATA M(1,1)/     0/,M(2,1)/     0/
      DATA M(1,2)/  4228/,M(2,2)/  4100/
      DATA M(1,3)/ 10560/,M(2,3)/     0/
      DATA M(1,4)/ 11242/,M(2,4)/ 32064/
      DATA M(1,5)/ 13006/,M(2,5)/  5806/
      DATA M(1,6)/ 26436/,M(2,6)/  8819/
      DATA M(1,7)/ 12876/,M(2,7)/ 21101/
      DATA M(1,8)/  4352/,M(2,8)/     0/
      DATA M(1,9)/  2180/,M(2,9)/  4226/
      DATA M(1,10)/  4162/,M(2,10)/  2116/
      DATA M(1,11)/  4778/,M(2,11)/ 21632/
      DATA M(1,12)/   132/,M(2,12)/ 31876/
      DATA M(1,13)/    -4/,M(2,13)/  4352/
      DATA M(1,14)/  -448/,M(2,14)/     0/
      DATA M(1,15)/     0/,M(2,15)/  4096/
      DATA M(1,16)/    34/,M(2,16)/  4368/
      DATA M(1,17)/ 14965/,M(2,17)/ 26158/
      DATA M(1,18)/  4484/,M(2,18)/  4238/
      DATA M(1,19)/ 14882/,M(2,19)/  4383/
      DATA M(1,20)/ 14886/,M(2,20)/  1582/
      DATA M(1,21)/  2250/,M(2,21)/ 19426/
      DATA M(1,22)/ 32286/,M(2,22)/  1086/
      DATA M(1,23)/  6448/,M(2,23)/ 31278/
      DATA M(1,24)/ 31778/,M(2,24)/  4228/
      DATA M(1,25)/ 14894/,M(2,25)/ 17966/
      DATA M(1,26)/ 14897/,M(2,26)/ 15406/
      DATA M(1,27)/     4/,M(2,27)/   128/
      DATA M(1,28)/  -128/,M(2,28)/  4352/
      DATA M(1,29)/    68/,M(2,29)/  8322/
      DATA M(1,30)/    14/,M(2,30)/   448/
      DATA M(1,31)/   260/,M(2,31)/  2184/
      DATA M(1,32)/ 15906/,M(2,32)/  4100/
C
C Chars 64-95
C
      DATA M(1,33)/ 14901/,M(2,33)/ 24078/
      DATA M(1,34)/ 14897/,M(2,34)/ 32305/
      DATA M(1,35)/ 29278/,M(2,35)/ 17982/
      DATA M(1,36)/ 14896/,M(2,36)/ 16942/
      DATA M(1,37)/ 31281/,M(2,37)/ 17982/
      DATA M(1,38)/ 32287/,M(2,38)/ 16927/
      DATA M(1,39)/ 32286/,M(2,39)/ 16912/
      DATA M(1,40)/ 14896/,M(2,40)/ 24110/
      DATA M(1,41)/ 17983/,M(2,41)/ 17969/
      DATA M(1,42)/ 14468/,M(2,42)/  4238/
      DATA M(1,43)/  1057/,M(2,43)/ 17966/
      DATA M(1,44)/ 19096/,M(2,44)/ 21073/
      DATA M(1,45)/ 16912/,M(2,45)/ 16927/
      DATA M(1,46)/ 18293/,M(2,46)/ 17969/
      DATA M(1,47)/ 18229/,M(2,47)/ 20017/
      DATA M(1,48)/ 14897/,M(2,48)/ 17966/
      DATA M(1,49)/ 31281/,M(2,49)/ 31248/
      DATA M(1,50)/ 14897/,M(2,50)/ 22093/
      DATA M(1,51)/ 31281/,M(2,51)/ 31313/
      DATA M(1,52)/ 15886/,M(2,52)/  1086/
      DATA M(1,53)/ 31876/,M(2,53)/  4228/
      DATA M(1,54)/ 17969/,M(2,54)/ 17966/
      DATA M(1,55)/ 17969/,M(2,55)/ 10564/
      DATA M(1,56)/ 22197/,M(2,56)/ 22186/
      DATA M(1,57)/ 17732/,M(2,57)/ 10801/
      DATA M(1,58)/ 17962/,M(2,58)/  4228/
      DATA M(1,59)/ 31812/,M(2,59)/  8735/
      DATA M(1,60)/  6276/,M(2,60)/  4230/
      DATA M(1,61)/   520/,M(2,61)/  4161/
      DATA M(1,62)/ 12420/,M(2,62)/  4236/
      DATA M(1,63)/  4433/,M(2,63)/     0/
      DATA M(1,64)/     0/,M(2,64)/    31/
C
C Chars 96-127
C
      DATA M(1,65)/  8320/,M(2,65)/     0/
      DATA M(1,66)/    14/,M(2,66)/ 19022/
      DATA M(1,67)/ 16924/,M(2,67)/ 19036/
      DATA M(1,68)/    14/,M(2,68)/ 16910/
      DATA M(1,69)/  2126/,M(2,69)/ 19022/
      DATA M(1,70)/    12/,M(2,70)/ 31244/
      DATA M(1,71)/  6412/,M(2,71)/  8456/
      DATA M(1,72)/-14930/,M(2,72)/ 14412/
      DATA M(1,73)/ 16924/,M(2,73)/ 19026/
      DATA M(1,74)/   128/,M(2,74)/  4226/
      DATA M(1,75)/ -4228/,M(2,75)/  4232/
      DATA M(1,76)/  8458/,M(2,76)/ 12618/
      DATA M(1,77)/  8456/,M(2,77)/  8452/
      DATA M(1,78)/    26/,M(2,78)/ 22197/
      DATA M(1,79)/    28/,M(2,79)/ 19026/
      DATA M(1,80)/    12/,M(2,80)/ 19020/
      DATA M(1,81)/-29266/,M(2,81)/ 29200/
      DATA M(1,82)/-14930/,M(2,82)/ 14403/
      DATA M(1,83)/    12/,M(2,83)/ 18960/
      DATA M(1,84)/    14/,M(2,84)/ 28764/
      DATA M(1,85)/   284/,M(2,85)/  8452/
      DATA M(1,86)/    18/,M(2,86)/ 19022/
      DATA M(1,87)/    17/,M(2,87)/ 17732/
      DATA M(1,88)/    21/,M(2,88)/ 22186/
      DATA M(1,89)/    18/,M(2,89)/ 12690/
      DATA M(1,90)/-19026/,M(2,90)/ 14412/
      DATA M(1,91)/    30/,M(2,91)/  4382/
      DATA M(1,92)/   196/,M(2,92)/ 12422/
      DATA M(1,93)/  4228/,M(2,93)/  4228/
      DATA M(1,94)/   388/,M(2,94)/  6284/
      DATA M(1,95)/  8866/,M(2,95)/     0/
      DATA M(1,96)/ 32767/,M(2,96)/ 32767/
C
C Establish row length
C
      IFROM = IVALPN(NFROM)
      APPEND = IFROM.GT.0
      ITO = IVALPN(NTO)
      IF (APPEND) THEN
         IF (SEMOPN(1,IFROM,NCOL,NROW,NL,CLASS,FORM,LP1)) GOTO 130
         IF (ITO.EQ.0) ITO=IFROM
      ELSE
         NCOL=IVAL(NSIZE)
         NROW=0
         CLASS=NCLIMA
         FORM=NFMBYT
         LP1=0
         IF (ITO.EQ.0) ITO = SEMPPN(999)
      ENDIF
C
C Establish text to be generated
C
      IF (APPEND.AND.OPT(NTITLE)) THEN
         IF (SEMLAB(1,LABEL,LP1)) GOTO 130
         LAST=LABEL(LBNCTT)
         IF (LAST.EQ.0) THEN
            ERROR=132
            IDERR=IFROM
            GOTO 130
         ENDIF
      ELSE
         LAST=80
         IF (SEMKTX(NTEXT,'Text (as textstring): ',TEXT,LAST,
     +      .FALSE.)) GOTO 130
         IF (LAST.EQ.0) THEN
            IDERR=NTEXT
            GOTO 140
         ENDIF
      ENDIF
C
C Split lines if necessary
C
      IF (NCOL.EQ.0) NCOL=6*LAST+1
      IF (NCOL.LT.6) THEN
         IDERR=NSIZE
         GOTO 140
      ENDIF
      NCPL=NCOL/6
      N=0
      L=0
      NLINES=0
   10 NLINES=NLINES+1
      N=N+NCPL
      IF (N.LT.LAST) THEN
   20    IF (TEXT(N).NE.KSPACE) THEN
            N=N-1
            IF (N.NE.L) GOTO 20
            N=L+NCPL
         ENDIF
         L=N
         ENDPTR(NLINES)=N
         IF (NLINES.EQ.10) GOTO 30
         GOTO 10
      ENDIF
      ENDPTR(NLINES)=LAST
C
C Open output
C
   30 LP2=LP1
      FORM=SEMFRM(FORM)
      IF (SEMOPN(2,ITO,NCOL,NROW+10*NLINES,1,CLASS,FORM,LP2)) GOTO 130
      IF (FORM.EQ.NFMBYT) THEN
         INFORM=NFMINT
      ELSE IF (FORM.EQ.NFMCOM) THEN
         INFORM=NFMFP
      ELSE
         INFORM=FORM
      ENDIF
C
C Establish background and foreground values for characters
C
      IF (VARSET(NBG)) THEN
         BG = IVAL(NBG)
      ELSE
         BG = VMIN
      ENDIF
C
      IF (VARSET(NFG)) THEN
         FG = IVAL(NFG)
      ELSE
         FG = VMAX
      ENDIF
C
      IF (INFORM.EQ.NFMINT) THEN
         BG = NINT(BG)
         FG = NINT(FG)
      ENDIF
C
C Fill RB2 with background
C
      IF (INFORM .EQ. NFMINT) THEN
         J = BG
         DO 40 I=1,NCOL
            IB2(I) = J
   40    CONTINUE
      ELSE
         DO 50 I=1,NCOL
            RB2(I) = BG
   50    CONTINUE
      ENDIF
C
C Copy picture itself, if any
C
      DO 60 J=1,NROW
         IF (SEMROW(1,RB1,FORM,J,1,LP1)) GOTO 130
         IF (SEMROW(2,RB1,FORM,J,1,LP2)) GOTO 130
   60 CONTINUE
C
C Generate text
C
      P1 = 1
      ROW = NROW+1
      DO 120 NL=1,NLINES
         P2 = ENDPTR(NL)
C
C Initial clear row
C
         IF (SEMROW(2,RB2,INFORM,ROW,1,LP2)) GOTO 130
         ROW=ROW+1
C
C Strip trailing space from this line?
C
         P2M=P2
         IF (TEXT(P2M).EQ.KSPACE) P2M=P2M-1
         LEFT=NCOL/2-(P2M-P1)*3-1
C
C Eight (=6+2) character rows from mask
C
         DO 110 L=1,8
            IP=LEFT
C
C Flood background
C
            IF (INFORM .EQ. NFMINT) THEN
               J = BG
               DO 70 I=1,NCOL
                  IB1(I) = J
   70          CONTINUE
            ELSE
               DO 80 I=1,NCOL
                  RB1(I) = BG
   80          CONTINUE
            ENDIF
            DO 100 I=P1,P2M
               N=L
               N2=TEXT(I)-KSPACE+1
               MASK=M(1,N2)
               IF (MASK.LE.0) THEN
                  MASK=-MASK
                  N=N-2
               ENDIF
               IF (N.LE.0.OR.N.GT.6) THEN
                  MASK=0
                  N2=16
               ELSE
                  IF (N.GT.3) THEN
                     N=N-3
                     MASK=M(2,N2)
                  ENDIF
                  N2=LEAD(N)
               ENDIF
C
C Decode mask
C
               DO 90 N=1,5
                  BIT = BG
                  M1=MASK/N2
                  IF (M1/2*2.NE.M1) BIT=FG
                  N2=N2/2
                  IF (INFORM.EQ.NFMINT) THEN
                     IB1(IP)=BIT
                  ELSE
                     RB1(IP)=BIT
                  ENDIF
                  IP=IP+1
   90          CONTINUE
               IP=IP+1
  100       CONTINUE
            IF (SEMROW(2,RB1,INFORM,ROW,1,LP2)) GOTO 130
            ROW=ROW+1
  110    CONTINUE
C
C Final clear row
C
         IF (SEMROW(2,RB2,INFORM,ROW,1,LP2)) GOTO 130
         ROW=ROW+1
         P1=P2+1
  120 CONTINUE
  130 RETURN
C
C Errors
C
  140 ERROR=3
      GOTO 130
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
