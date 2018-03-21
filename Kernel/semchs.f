C Semper 6 system module SEMCHS
C
      SUBROUTINE SEMCHS(TEXT,ITEXT,N)
C Convert array of I/C codes into character string
C The destination is blank padded if it is longer than the source
C
      CHARACTER*(*) TEXT
      INTEGER ITEXT(*),N
C
      INTEGER I,L
C
      L = LEN(TEXT)
C
      DO 10 I = 1,MIN(L,N)
         TEXT(I:I) = CHAR(ITEXT(I))
   10 CONTINUE
C
      IF (L.GT.N) TEXT(N+1:L) = ' '
C
      RETURN
C
C Copyright (C) 1987,1988,1989,1990: Synoptics Ltd,  All Rights Reserved
C
      END
