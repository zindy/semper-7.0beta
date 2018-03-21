C Semper 6 system module SEMSOU
C
      LOGICAL FUNCTION SEMSOU(TEXT,N)
C
      CHARACTER*(*) TEXT
      INTEGER N
C
C Outputs a line of N characters to the standard output stream.
C
      LOGICAL SEMPAG,WRITSO
C
      INCLUDE 'COMMON'
C
C If in batch mode , output text directly to standard output stream
C
      IF (BATCH.NE.0.0) THEN
         SEMSOU=WRITSO(TEXT,N)
C
C Otherwise in interactive mode, output text in pages (as determined
C by the PAGE command)
C
      ELSE
         SEMSOU=SEMPAG(TEXT,N)
      ENDIF
C
      RETURN
C
C Copyright (C) 1993:  Synoptics Ltd,  All Rights Reserved
C
      END
