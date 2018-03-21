C Semper 6 system module SEMTIT
C
      LOGICAL FUNCTION SEMTIT(IOP,TITLE,LPN)
      INTEGER IOP,LPN
      CHARACTER*(*) TITLE
C
C Read/write picture title.  Blank string passed to/from TITLE implies
C a non-existent title string
C
      INTEGER LNBLNK
      LOGICAL SEMLAB
C
      INTEGER LABEL(256)
C
      INCLUDE 'PARAMS'
C
      SEMTIT=.TRUE.
C
C Fetch picture label
      IF (SEMLAB(1,LABEL,LPN)) GOTO 10
C
C See if reading title from picture label
      IF (IOP.EQ.1) THEN
C
C Copy title string to return argument
         CALL SEMCHS(TITLE,LABEL(LBTT1),LABEL(LBNCTT))
C
C Otherwise, write title out to picture label
      ELSE
C
C Determine length of title string
         LABEL(LBNCTT)=MIN(LNBLNK(TITLE),LBTT2-LBTT1+1)
C
C Copy new title string (if any) to picture label array
         CALL SEMICS(TITLE,LABEL(LBTT1),LABEL(LBNCTT))
C
C Store picture label
         IF (SEMLAB(2,LABEL,LPN)) GOTO 10
      ENDIF
C
      SEMTIT=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
