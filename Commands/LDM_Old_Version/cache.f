C Semper 6 processing module CACHE
C
      SUBROUTINE CACHE
C
C Syntax:  Cache :CACHE number= size= device= memory free flush show
C
C Semper's disc cache can be re-configured at any time.  The cache
C consists of a specified number of buffers of a given size.  The
C buffer size is restricted to a multiple of 8 so that no individual
C datum (byte, integer, floating-point or complex value) is split
C between two cache buffers.  Otherwise there are no restrictions on
C the size of cache (even no cache if the NUMBER or SIZE key is set
C to zero).  Some care should be exercised, however, to ensure that
C the cache is not so large that it overruns all of the available
C memory and is then swapped out to disc.  At this point the benefit
C of having the disc cache is lost.  If option FLUSH is set, the entire
C disc cache is flushed immediately.  If the DEVICE key is also set the
C specified device is flushed.  The option MEMORY or FREE may also be
C used in combination with the DEVICE key to allocate or free a
C permanent memory buffer for the specified device.  While a device is
C buffered directly in memory, all i/o requests are satisfied without
C involving the disc cache.  Any cache buffers allocated to the device
C before the memory buffer is created are freed.  If option SHOW is
C set, the cache parameters are listed on the console output stream.
C
      REAL    VAL
      INTEGER IVAL !,IPACK
      LOGICAL CCACHE,VARSET,OPT,SEMCON,CDISC,SEMMED
C
      INTEGER   DEVICE,MEDIUM
      INTEGER*4 NUMBER,SIZE
C
      INCLUDE 'COMMON'
C
      LOGICAL   INIT
      INTEGER*4 MAXNUM,MAXSIZ,SIZFAC,BUFNUM,BUFSIZ
C
      SAVE INIT,BUFNUM,BUFSIZ
C
      DATA INIT / .FALSE. /
C
   10 FORMAT('   Number of buffers:  ',I9)
   20 FORMAT('   Buffer size (bytes):',I9)
   30 FORMAT('   Cache size (Kbytes):',F9.1)
   40 FORMAT('   Buffer size must be a multiple of ',I5,' bytes')
C
C Initialise disc cache parameters if first time around
C
      IF (.NOT.INIT) THEN
         IF (CCACHE(1,BUFNUM,BUFSIZ,0)) GOTO 60
C
         INIT = .TRUE.
      ENDIF
C
C Fetch cache parameter limits
C
      IF (CCACHE(0,MAXNUM,MAXSIZ,SIZFAC)) GOTO 60
C
C Fetch new values for disc cache parameters (default = current values)
C
      IF (VARSET(23253)) THEN
         NUMBER = IVAL(23253)
      ELSE
         NUMBER = BUFNUM
      ENDIF
C
      IF (VARSET(30786)) THEN
         SIZE = VAL(30786)
      ELSE
         SIZE = BUFSIZ
      ENDIF
C
C If new values specified, reconfigure disc cache
C
      IF (NUMBER.NE.BUFNUM.OR.SIZE.NE.BUFSIZ) THEN
C
C Fault bad value for NUMBER and SIZE keys (negative value or buffer
C size which is not a multiple of SIZFAC or 8)
C
         IF (NUMBER.LT.0.OR.(MAXNUM.GT.0.AND.NUMBER.GT.MAXNUM)) THEN
            ERROR = 3
            IDERR = 23253
            GOTO 50
         ENDIF
C
         IF (SIZE.LT.0.OR.(MAXSIZ.GT.0.AND.SIZE.GT.MAXSIZ).OR.
     +       MOD(SIZE,SIZFAC).NE.0.OR.MOD(SIZE,8).NE.0) THEN
            ERROR = 3
            IDERR = 30786
            GOTO 50
         ENDIF
C
C Reconfigure disc cache
C
         IF (CCACHE(2,NUMBER,SIZE,0)) GOTO 60
C
C Record new values for disc cache parameters
C
         BUFNUM = NUMBER
         BUFSIZ = SIZE
      ENDIF
C
C See if DEVICE key is set
C
      IF (VARSET(6622)) THEN
C
C Fetch value for DEVICE key
C
         DEVICE = IVAL(6622)
C
C Fetch medium number for device
C
         IF (SEMMED(DEVICE,MEDIUM)) GOTO 50
C
C Fault medium if not disc device
C
         IF (MEDIUM.NE.MEDDC) THEN
            ERROR = 29
            IDERR = MEDIUM
            GOTO 50
         ENDIF
C
C See if MEMORY option is set - buffer entire file in memory
C
         IF (OPT(21013)) THEN
            IF (CDISC(6,DEVICE,0,0,0,0,0,ERROR)) GOTO 50
C
C See if FREE option is set - free any memory buffer associated with
C                              specified device
C
         ELSE IF (OPT(10325)) THEN
            IF (CDISC(7,DEVICE,0,0,0,0,0,ERROR)) GOTO 50
C
C See if FLUSH option is set - write to disc any data associated with
C                              specified device
C
         ELSE IF (OPT(10101)) THEN
            IF (CDISC(3,DEVICE,0,0,0,0,0,ERROR)) GOTO 50
         ENDIF
C
C See if option FLUSH is set with no DEVICE key set
C
      ELSE IF (OPT(10101)) THEN
C
C Flush the entire disc cache now
C
         IF (CDISC(3,-1,0,0,0,0,0,ERROR)) GOTO 50
      ENDIF
C
C See if option SHOW is set
C
      IF (OPT(30735)) THEN
C
C List cache parameters - number of buffers, buffer size, cache size
C
         IF (SEMCON(' ')) GOTO 50
         IF (SEMCON('Semper disc I/O cache settings:')) GOTO 50
         WRITE (RECORD,10) BUFNUM
         IF (SEMCON(RECORD)) GOTO 50
         WRITE (RECORD,20) BUFSIZ
         IF (SEMCON(RECORD)) GOTO 50
         WRITE (RECORD,30) REAL(BUFNUM*BUFSIZ)/1024.0
         IF (SEMCON(RECORD)) GOTO 50
         IF (SIZFAC.GT.1) THEN
            WRITE (RECORD,40) SIZFAC
            IF (SEMCON(RECORD)) GOTO 50
         ENDIF
      ENDIF
C
   50 RETURN
C
C Error return
C
   60 ERROR = 77
      IDMESS = 'Error while accessing disc cache'
      GOTO 50
C
C Copyright (C) 1990-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
