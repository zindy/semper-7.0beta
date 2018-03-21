C Semper 6 system module DISC
C
      LOGICAL FUNCTION DISC(IOP,DEVICE,NITEMS,MEM,BLKN,MEMF,DISCF)
C
C Provides access to disc file storage via a 'cache' of buffers, each
C holding a contiguous series of recently used disc data.  Transfers
C to/from the physical disc are made only in units of a whole buffer.
C If all the buffers are in use when new data is to be accessed, the
C least recently accessed buffer is freed and re-used.  When a buffer
C is written to (modified), its contents are not written to disc until
C the buffer is re-allocated or explicitly flushed, thereby avoiding
C any unnecessary output to disc.
C
C IOP = 0, initialises cache buffer contents
C     = 1/2, reads/writes NITEMS items from/to block BLKN of disc file
C       on DEVICE, to/from MEM; data is form-converted from MEMF to
C       DISCF in transit
C     = 3, flushes all buffers for DEVICE if written-to;
C       if DEVICE<0, simply flushes all
C       written-to buffers for security, retaining contents
C     = 5, clears all buffers for DEVICE, flushing first if written-to;
C       used on deassigning files
C
      INTEGER   IOP,DEVICE,MEM(*),MEMF,DISCF
      INTEGER*4 NITEMS,BLKN
C
      LOGICAL SEMMED,SEMLNF,CDISC
C
      INTEGER MEDIUM,LNFORM
C
      INCLUDE 'COMMON'
C
      DISC = .TRUE.
C
C Bypass checks if work disc
C
      IF (DEVICE.NE.0) THEN
C
C Fault bad medium for device (and bad device number via SEMMED)
C Bypass check if flushing (IOP = 3) and device number is negative
C
         IF (.NOT.(IOP.EQ.3.AND.DEVICE.LT.0)) THEN
            IF (SEMMED(DEVICE,MEDIUM)) GOTO 10
C
            IF (.NOT.(MEDIUM.EQ.MEDDC.OR.MEDIUM.EQ.MEDVM)) THEN
               ERROR = 29
               IDERR = DEVICE
               GOTO 10
            ENDIF
         ENDIF
C
C Fault bad block number if read/write operation
C
         IF (IOP.EQ.1.OR.IOP.EQ.2) THEN
            IF (SEMLNF(DISCF,LNFORM)) GOTO 10
C
            IF (BLKN.LT.1.OR.
     +          BLKN+(NITEMS*LNFORM-1)/LNBLK.GT.FLSIZ(DEVICE)) THEN
               ERROR = 50
               IDERR = DEVICE
               I4IDER = BLKN
               GOTO 10
            ENDIF
         ENDIF
C
C Fault write operation on write-protected device
C
         IF (IOP.EQ.2.AND.PROTN(DEVICE).NE.0) THEN
            ERROR = 41
            IDERR = DEVICE
            GOTO 10
         ENDIF
      ENDIF
C
C Carry out disc operation and report any errors
C
      IF (CDISC(IOP,DEVICE,NITEMS,MEM,BLKN,MEMF,DISCF,ERROR)) THEN
         IF (ERROR.EQ.8) THEN
            IDERR = DEVICE
            IF (IOP.EQ.3 .OR. IOP.EQ.5) THEN
               I4IDER = 0
            ELSE
               I4IDER = BLKN
            ENDIF
         ENDIF
         GOTO 10
      ENDIF
C
      DISC = .FALSE.
C
   10 RETURN
C
C Copyright (C) 1990-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
