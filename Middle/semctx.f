C Semper 6 system module SEMCTX
C
      SUBROUTINE SEMCTX(TEXT,LENGTH,PTR,STRING)
C
C Generate string for reporting error context
C
      INTEGER TEXT(*),LENGTH,PTR
      CHARACTER*(*) STRING
C
      INTEGER M,N,S
      LOGICAL TRAIL
C
      STRING(1:4) = 'at: '
C
C Determine room for faulty text, leaving space for trailing dots
C
      M = MIN(PTR,LENGTH)
      TRAIL = M .LT. LENGTH
      N = LEN(STRING) - 4
      IF (TRAIL) N = N - 2
C
C Copy text into return string, with leading and trailing dots as needed
C
      IF (M.GT.N) THEN
         STRING(5:6) = '..'
         S = 7
         N = N - 2
         M = M - N + 1
      ELSE
         S = 5
         N = M
         M = 1
      ENDIF
      CALL SEMCHS(STRING(S:),TEXT(M),N)
      S = S + N
      IF (TRAIL) STRING(S:S+1) = '..'
C
      RETURN
C
C Copyright (C) 1989-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
