C Semper 6 primitive module FILPOS
C
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION FILPOS ( HANDLE, MODE, IPOS )
C       ------------------------------------------------------
C
C       PARAMETERS:
C
C       integer handle : INPUT - Handle to the file to position
C
C       integer mode : INPUT - Mode to use for positioning
C                      1 = Relative to start
C                      2 = Relative to current position
C                      3 = Relative to end
C
C       integer ipos : INPUT - Offset in file at which to position
C
C       Positions the file with the given handle to the the given offset
C       from the start, current position or end of the file depending
C       on the value of MODE.
C
C       Function returns TRUE in case of error, otherwise FALSE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION FILPOS ( HANDLE, MODE, IPOS )
C
C     ==============================================
C
      INTEGER HANDLE, MODE, IPOS
C
C LOCAL VARIABLES:
C
C Long integer copies of the arguments
C
      INTEGER*4 HAND4, IPOS4, MODE4
C
C CALLED FUNCTIONS:
C
C Seeks a file
C
C     SGI, IFORT
C      INTEGER*4 FSEEK
C
      INCLUDE 'COMMON'
C
C Set things up to seek correctly
C
      HAND4 = ( HANDLE)
      IPOS4 = ( IPOS)
      MODE4 = ( MODE - 1)
C
C And seek as required
C
C      Version for ifc, SGI,
C       FILPOS = FSEEK ( HAND4, IPOS4, MODE4 ) .NE. 0
C      
C      G77 version
       FILPOS = .false.
       call FSEEK ( HAND4, IPOS4, MODE4 )
C
C Check for error
C
      IF (FILPOS) THEN
         ERROR = INTERR
         IDERR = HANDLE
         IDERR2 = -1
         IDMESS = 'FILPOS'
      ENDIF
C
      RETURN
C
C Copyright (C) 1989-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
