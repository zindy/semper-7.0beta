C Semper 6 subsidiary processing module ASSNAM
C
      LOGICAL FUNCTION ASSNAM(IOP,DEVICE,NAME)
C
      INTEGER IOP,DEVICE,NAME(*)
C
C Provide access to device name string on work disc
C
      LOGICAL DISC
C
      INTEGER I,N
C
      INTEGER*4 BLKN
C
      INCLUDE 'COMMON'
C
C Maximum size of device name string array
C
      INTEGER NAMSZE
      PARAMETER ( NAMSZE = FILMAX+1 )
C
C Number of logicl blocks to accomodate device name string
C
      INTEGER NAMBLK
      PARAMETER ( NAMBLK = 1+(NAMSZE-1)/LNBLK )
C
C Size of buffer array (rounded up to logical block size)
C
      INTEGER   BUFSZE
      INTEGER*4 BUFSZ4
      PARAMETER ( BUFSZE = NAMBLK*LNBLK )
      PARAMETER ( BUFSZ4 = BUFSZE )
C
      INTEGER BUFFER(BUFSZE)
C
      ASSNAM=.TRUE.
C
C Fault bad device number
C
      IF (DEVICE.LT.1.OR.DEVICE.GT.NDVS) THEN
         ERROR=76
         IDERR=DEVICE
         GOTO 40
      ENDIF
C
C Determine block address for name string on work disc
C
      BLKN=WRKDVS+(DEVICE-1)*NAMBLK
C
C IOP = 1, fetch name string from work disc
C
      IF (IOP.EQ.1) THEN
C
C Read name from work disc converting from byte to integer form
C
         IF (DISC(1,WRKDEV,BUFSZ4,BUFFER,BLKN,NFMINT,NFMBYT)) GOTO 40
C
C Copy name from buffer array to return array
C
         DO 10 I=1,BUFFER(1)+1
            NAME(I)=BUFFER(I)
   10    CONTINUE
C
C IOP = 2, store name string in work disc
C
      ELSE IF (IOP.EQ.2) THEN
C
C Truncate name to fit space allocated for device name string
C
         N=MIN(MAX(0,NAME(1)),FILMAX)
C
C Store count in first array element
C
         BUFFER(1)=N
C
C Copy name into buffer array
C
         DO 20 I=2,N+1
            BUFFER(I)=NAME(I)
   20    CONTINUE
C
C Zero remainder of array
C
         DO 30 I=N+2,BUFSZE
            BUFFER(I)=0
   30    CONTINUE
C
C Write name to work disc in byte form
C
         IF (DISC(2,WRKDEV,BUFSZ4,BUFFER,BLKN,NFMINT,NFMBYT)) GOTO 40
      ENDIF
C
      ASSNAM=.FALSE.
C
   40 RETURN
C
C Copyright (C) 1993:  Synoptics Ltd,  All Rights Reserved
C
      END
