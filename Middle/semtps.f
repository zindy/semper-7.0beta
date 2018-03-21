C Semper 6 system module SEMTPS
C
      LOGICAL FUNCTION SEMTPS(IWID,ILEN)
C
C Returns the current terminal page size.  This is the size last
C defined by the PAGE command, but truncated to fit within the limits
C determined by the hardware (if any).
C
      INTEGER IWID,ILEN
C
      INCLUDE 'COMMON'
C
C See if in batch mode
C
      IF (BATCH.NE.0.0) THEN
C
C If so, return software page limits
C
         IWID=TERWID
         ILEN=TERLEN
C
C Otherwise, page limits may be truncated according to hardware limits
C
      ELSE
C
C Establish hardware limits for the page size
C
         CALL TERSIZ(IWID,ILEN)
C
         IF (IWID.LT.1) IWID=TERWID
         IF (ILEN.LT.1) ILEN=TERLEN
C
C Truncate user-defined page size to hardware limits
C
         IWID=MIN(TERWID,IWID)
         ILEN=MIN(TERLEN,ILEN)
      ENDIF
C
      SEMTPS=.FALSE.
C
      RETURN
C
C Copyright (C) 1989-1993:  Synoptics Ltd,  All Rights Reserved
C
      END
