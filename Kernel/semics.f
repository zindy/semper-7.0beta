C Semper 6 system module SEMICS
C
      SUBROUTINE SEMICS(TEXT,ITEXT,N)
C
C Convert character string into array of I/C codes
C The destination is blank padded if it is longer than the source
C
      CHARACTER*(*) TEXT
      INTEGER ITEXT(*),N
C
      INTEGER I,L
C
      INCLUDE 'ICSET'
C
      L = LEN(TEXT)
C
      DO 10 I = 1,MIN(L,N)
         ITEXT(I) = ICHAR(TEXT(I:I))
   10 CONTINUE
C
      DO 20 I = L+1,N
         ITEXT(I) = KSPACE
   20 CONTINUE
C
      RETURN
C
C Copyright (C) 1989,1990:  Synoptics Ltd,  All Rights Reserved
C
      END
