c
c----------------------------------------------------------------------
c
c	LOGICAL FUNCTION WHOCNA ( DEVICE, WID, NAME, LENGTH )
c       -----------------------------------------------------
c
c	PARAMETERS:
c
c       integer device : INPUT - the device on which the window resides.
c
c       integer wid : INPUT - the identifier of the window to name.
c
c       character*(*) name : INPUT - the name to give to the window.
c
c       integer length : INPUT - then length of the name.
c
c	Changes the name of a window on the host device.  Is a noop for dumb
c       window managers.
c
c	Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function whocna ( device, wid, name, length )
c     =====================================================
c
      integer device
      integer wid
      character*(*) name
      integer length
c
      whocna = .FALSE.
c
      return
      end
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
