C Semper 6 utility module FILPAT
C
      LOGICAL FUNCTION FILPAT(N,PATH)
C
C Returns element N of the current search path.  PATH is returned as a
C blank string if N points outside the search path
C
      INTEGER N
      CHARACTER*(*) PATH
C
      LOGICAL GETPAT,FILELM
C
      CHARACTER*2048 PATHS
      INTEGER NP
C
      FILPAT = .TRUE.
C
C Fetch current search path string
C
      IF (GETPAT(PATHS,NP)) GOTO 10
C
C If requested search path element contained in string, pull out the
C corresponding directory string, otherwise, return blank string
C
      IF (N.GE.1.AND.N.LE.NP) THEN
         IF (FILELM(PATHS,N,PATH)) GOTO 10
      ELSE
         PATH = ' '
      ENDIF
C
      FILPAT = .FALSE.
C
   10 RETURN
C
C Copyright (C) 1989,1990:  Synoptics Ltd,  All Rights Reserved
C
      END
